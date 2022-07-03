async function roc_gen_func() {
  let exit_code;
  let wasm_exports;
  let buffer;
  let count;

  const importObj = {
    wasi_snapshot_preview1: {
      proc_exit: (code) => {
        if (code !== 0) {
          console.error(`Exited with code ${code}`);
        }
        exit_code = code;
      },
      fd_write: (_) => { console.error("We don't deal with fd_write"); },
    },
    env: {
      fill_buffer: (ptr) => {
        bytes = new Uint8Array(wasm_exports.memory.buffer, ptr, buffer.length);
        bytes.set(buffer);
      },
      buffer_length: () => { return buffer.length; },
      set_output_count: (cnt) => { count = cnt; },
      roc_panic: (_pointer, _tag_id) => {
        throw "Roc panicked!";
      },
    },
  };

  let wasm;

  const response = await fetch("./implementations/roc/emulator.wasm");

  if (WebAssembly.instantiateStreaming) {
    // streaming API has better performance if available
    wasm = await WebAssembly.instantiateStreaming(response, importObj);
  } else {
    const module_bytes = await response.arrayBuffer();
    wasm = await WebAssembly.instantiate(module_bytes, importObj);
  }

  wasm_exports = wasm.instance.exports;

  return function (buf) {
    buffer = new Uint8Array(buf);
    try {
      wasm_exports._start();
    } catch (e) {
      const is_ok = e.message === "unreachable" && exit_code === 0;
      if (!is_ok) {
        console.error(e);
      }
    }
    return count;
  }
}

if (typeof module !== "undefined") {
  module.exports = {
    run,
  };
}
