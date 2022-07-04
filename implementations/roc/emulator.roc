app "emulator"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

main = \mem ->
    emu = newEmulator 0x438b mem

    mainHelper emu 0

mainHelper : Emulator, Nat -> Nat
mainHelper = \emu0, cnt ->
    if emu0.cpu.pc != 0x640b then
        emu1 = step emu0

        mainHelper emu1 (cnt + 1)
    else
        cnt

Emulator : {
        cpu : CPU,
        mem : Mem,
    }

newEmulator : Addr, Mem -> Emulator
newEmulator = \pc, mem -> {
    cpu: newCPU pc,
    mem,
}

Addr : U16
Byte : U8
Flag : Byte
Mem : List U8

CPU : {
        regA : Byte,
        regX : Byte,
        regY : Byte,
        status : Byte,
        sp : Byte,
        pc : Addr,
    }

newCPU : Addr -> CPU
newCPU = \pc -> {
    regA: 0x00,
    regX: 0x00,
    regY: 0x00,
    status: 0x00,
    sp: 0xFF,
    pc,
}

Reg : [A, X, Y, SP]

readReg : Emulator, Reg -> Byte
readReg = \emu, reg ->
    when reg is
        A -> emu.cpu.regA
        X -> emu.cpu.regX
        Y -> emu.cpu.regY
        SP -> emu.cpu.sp

writeReg : Emulator, Reg, Byte -> Emulator
writeReg = \{ cpu: cpu0, mem }, reg, byte ->
    cpu1 =
        when reg is
            A -> { cpu0 & regA: byte }
            X -> { cpu0 & regX: byte }
            Y -> { cpu0 & regY: byte }
            SP -> { cpu0 & sp: byte }

    { cpu: cpu1, mem }

readMem : Mem, Addr -> Byte
readMem = \mem, addr ->
    when List.get mem (Num.toNat addr) is
        Ok b -> b
        Err OutOfBounds ->
            # Force a panic by overflowing a U8
            0xFF + 1

writeMem : Mem, Addr, Byte -> Mem
writeMem = \mem, addr, byte ->
    List.set mem (Num.toNat addr) byte

fetch : Emulator -> [T Emulator Byte]
fetch = \{ cpu, mem } ->
    addr = cpu.pc
    byte = readMem mem addr
    T
        {
            cpu: { cpu & pc: Num.addWrap addr 1 },
            mem,
        }
        byte

toAddr : Byte, Byte -> Addr
toAddr = \lo, hi ->
    Num.bitwiseOr (Num.shiftLeftBy 8 (Num.toU16 hi)) (Num.toU16 lo)

fetchAddr : Emulator -> [T Emulator Addr]
fetchAddr = \emu0 ->
    (T emu1 lo) = fetch emu0
    (T emu2 hi) = fetch emu1
    T emu2 (toAddr lo hi)

readMemAddr : Emulator, Addr -> [T Emulator Addr]
readMemAddr = \emu, addr ->
    lo = readMem emu.mem addr
    hi = readMem emu.mem (Num.addWrap addr 1)
    T emu (toAddr lo hi)

push : Emulator, Byte -> Emulator
push = \{ cpu, mem: mem0 }, byte ->
    ptr = Num.toU16 cpu.sp
    mem1 = writeMem mem0 (Num.addWrap ptr 0x100) byte
    {
        # This may be a bad idea. It probably shouldn't wrap.
        # instead it should just crash, but this is what the js version does.
        cpu: { cpu & sp: Num.subWrap (Num.toU8 ptr) 1 },
        mem: mem1,
    }

pushAddr : Emulator, Addr -> Emulator
pushAddr = \emu0, addr ->
    hi = Num.toU8 (Num.shiftRightZfBy 8 addr)
    lo = Num.toU8 (Num.bitwiseAnd addr 0xFF)
    emu1 = push emu0 hi
    push emu1 lo

pop : Emulator -> [T Emulator Byte]
pop = \{ cpu, mem } ->
    ptr = Num.toU16 (Num.addWrap cpu.sp 1)
    byte = readMem mem (Num.addWrap ptr 0x100)
    T
        {
            cpu: { cpu & sp: Num.toU8 ptr },
            mem,
        }
        byte

popAddr : Emulator -> [T Emulator Addr]
popAddr = \emu0 ->
    (T emu1 lo) = pop emu0
    (T emu2 hi) = pop emu1
    T emu2 (toAddr lo hi)

getFlag : Emulator, Flag -> Bool
getFlag = \{ cpu }, flag ->
    Num.bitwiseAnd cpu.status flag != 0

setFlag : Emulator, Flag, Bool -> Emulator
setFlag = \{ cpu, mem }, flag, b ->
    status =
        if b then
            Num.bitwiseOr cpu.status flag
        else
            Num.bitwiseAnd cpu.status (Num.bitwiseXor flag 0xFF)
    {
        cpu: { cpu & status },
        mem,
    }

carry = 0b0000_0001
zero = 0b0000_0010
interruptEnable = 0b0000_0100
decimal = 0b0000_1000
overflow = 0b0100_0000
negative = 0b1000_0000

PartialByteOp : Emulator, Byte -> [T Emulator Byte]
ByteOp : Emulator, Byte -> Emulator
AddrOp : Emulator, Addr -> Emulator
Addressing : Emulator -> [T Emulator Addr]
imm : Emulator, ByteOp -> Emulator
imm = \emu0, op ->
    (T emu1 byte) = fetch emu0
    op emu1 byte

byVal : Emulator, Addressing, ByteOp -> Emulator
byVal = \emu0, addressing, op ->
    (T emu1 addr) = addressing emu0
    byte = readMem emu1.mem addr
    op emu1 byte

byRef : Emulator, Addressing, AddrOp -> Emulator
byRef = \emu0, addressing, op ->
    (T emu1 addr) = addressing emu0
    op emu1 addr

inplace : Emulator, Addressing, PartialByteOp -> Emulator
inplace = \emu, addressing, op ->
    byRef
        emu
        addressing
        \{ cpu: cpu0, mem: mem0 }, addr ->
            readByte = readMem mem0 addr
            (T {cpu: cpu1, mem: mem1} outByte) = op { cpu: cpu0, mem: mem0 } readByte
            mem2 = writeMem mem1 addr outByte
            { cpu: cpu1, mem: mem2 }

implied : Emulator, Reg, PartialByteOp -> Emulator
implied = \emu0, reg, op ->
    readByte = readReg emu0 reg
    T emu1 outByte = op emu0 readByte
    writeReg emu1 reg outByte

zp : Addressing
zp = \emu0 ->
    (T emu1 byte) = fetch emu0
    T emu1 (Num.toU16 byte)

zpX : Addressing
zpX = \emu0 ->
    (T emu1 addr) = zp emu0
    byte = readReg emu1 X
    T emu1 (Num.addWrap addr (Num.toU16 byte))

zpY : Addressing
zpY = \emu0 ->
    (T emu1 addr) = zp emu0
    byte = readReg emu1 Y
    T emu1 (Num.addWrap addr (Num.toU16 byte))

abs : Addressing
abs = \emu0 ->
    fetchAddr emu0

absX : Addressing
absX = \emu0 ->
    (T emu1 addr) = fetchAddr emu0
    byte = readReg emu1 X
    T emu1 (Num.addWrap addr (Num.toU16 byte))

absY : Addressing
absY = \emu0 ->
    (T emu1 addr) = fetchAddr emu0
    byte = readReg emu1 Y
    T emu1 (Num.addWrap addr (Num.toU16 byte))

xInd : Addressing
xInd = \emu0 ->
    (T emu1 z) = fetch emu0
    offset = readReg emu1 X
    ref = Num.addWrap (Num.toU16 z) (Num.toU16 offset)
    readMemAddr emu1 ref

indY : Addressing
indY = \emu0 ->
    (T emu1 z) = fetch emu0
    offset = readReg emu1 Y
    (T emu2 base) = readMemAddr emu1 (Num.toU16 z)
    addr = Num.addWrap base (Num.toU16 offset)
    T emu2 addr

signed : Emulator, (Addr, Addr, Bool -> Addr), Addr, Addr -> [T Emulator Byte]
signed = \emu0, f, v1, v2 ->
    c0 = getFlag emu0 carry
    result = f v1 v2 c0
    #TODO: BCD

    emu1 = setFlag emu0 overflow ((Num.bitwiseAnd result 0x80) != (Num.bitwiseAnd (Num.bitwiseAnd v1 v2) 0x80))
    emu2 = setFlag emu1 carry (result >= 0x100)
    emu3 = setFlag emu2 zero ((Num.bitwiseAnd result 0xFF) == 0x00)
    emu4 = setFlag emu3 negative ((Num.bitwiseAnd result 0x80) == 0x80)
    T emu4 (Num.toU8 result)

adc : ByteOp
adc = \emu0, byte ->
    a = readReg emu0 A
    (T emu1 result) = signed emu0
        (\v1, v2, c0 ->
            Num.addWrap (Num.addWrap v1 v2) (if c0 then 1 else 0)
        ) (Num.toU16 a) (Num.toU16 byte)
    writeReg emu1 A result

sub : Emulator, Addr, Addr -> [T Emulator Byte]
sub = \emu0, v1, v2 ->
    # Not sure why this doesn't use signed, but just following the javascript code.
    c0 = getFlag emu0 carry
    extended = Num.subWrap (Num.subWrap v1 v2) (if c0 then 0 else 1)
    emu1 = setFlag emu0 overflow ((Num.bitwiseAnd extended 0x80) != (Num.bitwiseAnd (Num.bitwiseAnd v1 v2) 0x80))
    emu2 = setFlag emu1 carry (extended >= 0x100)
    emu3 = setFlag emu2 zero ((Num.bitwiseAnd extended 0xFF) == 0x00)
    emu4 = setFlag emu3 negative ((Num.bitwiseAnd extended 0x80) == 0x80)
    T emu4 (Num.toU8 extended)

cmp : Byte -> ByteOp
cmp = \a ->
    \emu0, v ->
        emu1 = setFlag emu0 carry True
        (T emu2 byte) = sub emu1 (Num.toU16 a) (Num.toU16 v)
        emu2

sbc : ByteOp
sbc = \emu0, byte ->
    a = readReg emu0 A
    (T emu1 result) = sub emu0 (Num.toU16 a) (Num.toU16 byte)
    writeReg emu1 A result

alu : (Addr -> Addr) -> PartialByteOp
alu = \f ->
    \emu0, a ->
        result = f (Num.toU16 a)
        emu1 = setFlag emu0 zero ((Num.bitwiseAnd result 0xFF) == 0x00)
        emu2 = setFlag emu1 negative ((Num.bitwiseAnd result 0x80) == 0x80)
        T emu2 (Num.toU8 result)

and : ByteOp
and = \emu0, byte ->
    a = readReg emu0 A
    (T emu1 result) = (alu (\v -> Num.bitwiseAnd (Num.toU16 byte) v)) emu0 a
    writeReg emu1 A result

eor : ByteOp
eor = \emu0, byte ->
    a = readReg emu0 A
    (T emu1 result) = (alu (\v -> Num.bitwiseXor (Num.toU16 byte) v)) emu0 a
    writeReg emu1 A result

ora : ByteOp
ora = \emu0, byte ->
    a = readReg emu0 A
    (T emu1 result) = (alu (\v -> Num.bitwiseOr (Num.toU16 byte) v)) emu0 a
    writeReg emu1 A result

shiftRot : (Bool, Byte -> {c: Byte, v: Byte}) -> PartialByteOp
shiftRot = \f ->
    \emu0, v ->
        c = getFlag emu0 carry
        cv = f c v
        emu1 = setFlag emu0 carry (cv.c != 0)
        emu2 = setFlag emu1 zero (cv.v == 0x00)
        emu3 = setFlag emu2 negative ((Num.bitwiseAnd cv.v 0x80) == 0x80)
        T emu3 cv.v

asl : PartialByteOp
asl = shiftRot (\c,v -> { c: Num.bitwiseAnd v 0x80, v: Num.shiftLeftBy 1 v })

lsr : PartialByteOp
lsr = shiftRot (\c,v -> { c: Num.bitwiseAnd v 0x01, v: Num.shiftRightZfBy 1 v })

rol : PartialByteOp
rol = shiftRot (\c,v -> { c: Num.bitwiseAnd v 0x80, v: Num.bitwiseOr (Num.shiftLeftBy 1 v) (if c then 0x01 else 0x00) })

ror : PartialByteOp
ror = shiftRot (\c,v -> { c: Num.bitwiseAnd v 0x01, v: Num.bitwiseOr (Num.shiftRightZfBy 1 v) (if c then 0x80 else 0x00) })

step : Emulator -> Emulator
step = \emu0 ->
    (T emu1 op) = fetch emu0
    when op is
        _ ->
            cpu1 = emu1.cpu
            {
                # Panic on unknown opcodes.
                cpu: {cpu1 & pc: 0xFF + 1},
                mem: emu1.mem
            }
