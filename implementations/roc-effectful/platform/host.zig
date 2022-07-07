const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;
const always_inline = std.builtin.CallOptions.Modifier.always_inline;

comptime {
    // This is a workaround for https://github.com/ziglang/zig/issues/8218
    // which is only necessary on macOS.
    //
    // Once that issue is fixed, we can undo the changes in
    // 177cf12e0555147faa4d436e52fc15175c2c4ff0 and go back to passing
    // -fcompiler-rt in link.rs instead of doing this. Note that this
    // workaround is present in many host.zig files, so make sure to undo
    // it everywhere!
    if (builtin.os.tag == .macos) {
        _ = @import("compiler_rt");
    }
}

const Align = extern struct { a: usize, b: usize };
extern fn malloc(size: usize) callconv(.C) ?*align(@alignOf(Align)) anyopaque;
extern fn realloc(c_ptr: [*]align(@alignOf(Align)) u8, size: usize) callconv(.C) ?*anyopaque;
extern fn free(c_ptr: [*]align(@alignOf(Align)) u8) callconv(.C) void;
extern fn memcpy(dest: *anyopaque, src: *anyopaque, count: usize) *anyopaque;

// var AllocCount: usize = 0;
export fn roc_alloc(size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    _ = alignment;
    // AllocCount += 1;
    return malloc(size);
}

export fn roc_realloc(c_ptr: *anyopaque, new_size: usize, old_size: usize, alignment: u32) callconv(.C) ?*anyopaque {
    _ = old_size;
    _ = alignment;

    return realloc(@alignCast(@alignOf(Align), @ptrCast([*]u8, c_ptr)), new_size);
}

export fn roc_dealloc(c_ptr: *anyopaque, alignment: u32) callconv(.C) void {
    _ = alignment;

    free(@alignCast(@alignOf(Align), @ptrCast([*]u8, c_ptr)));
}

export fn roc_memcpy(dest: *anyopaque, src: *anyopaque, count: usize) callconv(.C) void {
    _ = memcpy(dest, src, count);
}

// NOTE roc_panic is provided in the JS file, so it can throw an exception

const mem = std.mem;
const Allocator = mem.Allocator;

pub const REFCOUNT_ONE_ISIZE: isize = std.math.minInt(isize);
pub const REFCOUNT_ONE: usize = @bitCast(usize, REFCOUNT_ONE_ISIZE);
pub fn alloc(size: usize, alignment: u32) ?[*]u8 {
    return @ptrCast(?[*]u8, @call(.{ .modifier = always_inline }, roc_alloc, .{ size, alignment }));
}
fn allocateWithRefcount(
    data_bytes: usize,
    element_alignment: u32,
) [*]u8 {
    const ptr_width = @sizeOf(usize);
    const alignment = std.math.max(ptr_width, element_alignment);
    const length = alignment + data_bytes;

    var new_bytes: [*]u8 = alloc(length, alignment) orelse unreachable;

    const data_ptr = new_bytes + alignment;
    const refcount_ptr = @ptrCast([*]usize, @alignCast(ptr_width, data_ptr) - ptr_width);
    refcount_ptr[0] = REFCOUNT_ONE;

    return data_ptr;
}

const RocList = extern struct {
    bytes: ?[*]u8,
    length: usize,
    capacity: usize,

    fn allocate(
        alignment: u32,
        length: usize,
        element_size: usize,
    ) RocList {
        const data_bytes = length * element_size;

        return RocList{
            .bytes = allocateWithRefcount(data_bytes, alignment),
            .length = length,
            .capacity = length,
        };
    }
};

extern fn roc__mainForHost_1_exposed_generic([*]u8) void;
extern fn roc__mainForHost_size() i64;
extern fn roc__mainForHost_1_Fx_caller(*const u8, [*]u8, [*]usize) void;
extern fn roc__mainForHost_1_Fx_size() i64;
extern fn roc__mainForHost_1_Fx_result_size() i64;

fn call_the_closure(closure_data_pointer: [*]u8) usize {
    const allocator = std.heap.page_allocator;

    const size = roc__mainForHost_1_Fx_result_size();
    const raw_output = allocator.allocAdvanced(u8, @alignOf(u64), @intCast(usize, size), .at_least) catch unreachable;
    var output = @ptrCast([*]usize, raw_output);

    defer {
        allocator.free(raw_output);
    }

    const flags: u8 = 0;

    roc__mainForHost_1_Fx_caller(&flags, closure_data_pointer, output);

    return output[0];
}

const Unit = extern struct {};

extern fn set_output_count(cnt: usize) void;
extern fn buffer_length() usize;
extern fn fill_buffer(list_bytes: ?[*]u8) void;

var memory: RocList = RocList{ .bytes = null, .length = 0, .capacity = 0 };

pub fn main() u8 {
    // for now just return the correct cpu count.
    const len = buffer_length();
    memory = RocList.allocate(8, len, 1);
    fill_buffer(memory.bytes);

    // Setup roc closure calling.
    const size = @intCast(usize, roc__mainForHost_size());
    const raw_output = roc_alloc(@intCast(usize, size), @alignOf(u64)).?;
    var output = @ptrCast([*]u8, raw_output);

    defer {
        roc_dealloc(raw_output, @alignOf(u64));
    }

    // Call roc function.
    roc__mainForHost_1_exposed_generic(output);
    const closure_data_pointer = @ptrCast([*]u8, output);
    const cnt = call_the_closure(closure_data_pointer);

    set_output_count(cnt);
    // set_output_count(AllocCount);
    return 0;
}

pub export fn roc_fx_readMem(addr: u16) u8 {
    return memory.bytes.?[addr];
}

pub export fn roc_fx_writeMem(addr: u16, byte: u8) void {
    memory.bytes.?[addr] = byte;
}
