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

Reg : [A, X, Y, SP, Status]

readReg : Emulator, Reg -> Byte
readReg = \emu, reg ->
    when reg is
        A -> emu.cpu.regA
        X -> emu.cpu.regX
        Y -> emu.cpu.regY
        SP -> emu.cpu.sp
        Status -> emu.cpu.status

writeReg : Emulator, Reg, Byte -> Emulator
writeReg = \{ cpu: cpu0, mem }, reg, byte ->
    cpu1 =
        when reg is
            A -> { cpu0 & regA: byte }
            X -> { cpu0 & regX: byte }
            Y -> { cpu0 & regY: byte }
            SP -> { cpu0 & sp: byte }
            Status -> { cpu0 & status: byte }

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
readMemAddr = \{cpu, mem}, addr ->
    lo = readMem mem addr
    hi = readMem mem (Num.addWrap addr 1)
    T {cpu, mem} (toAddr lo hi)

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
asl = shiftRot (\_c,v -> { c: Num.bitwiseAnd v 0x80, v: Num.shiftLeftBy 1 v })

lsr : PartialByteOp
lsr = shiftRot (\_c,v -> { c: Num.bitwiseAnd v 0x01, v: Num.shiftRightZfBy 1 v })

rol : PartialByteOp
rol = shiftRot (\c,v -> { c: Num.bitwiseAnd v 0x80, v: Num.bitwiseOr (Num.shiftLeftBy 1 v) (if c then 0x01 else 0x00) })

ror : PartialByteOp
ror = shiftRot (\c,v -> { c: Num.bitwiseAnd v 0x01, v: Num.bitwiseOr (Num.shiftRightZfBy 1 v) (if c then 0x80 else 0x00) })

bit : ByteOp
bit = \emu0, v ->
    a = readReg emu0 A
    emu1 = setFlag emu0 zero ((Num.bitwiseAnd a v) == 0x00)
    emu2 = setFlag emu1 negative ((Num.bitwiseAnd v 0x80) == 0x80)
    setFlag emu2 overflow ((Num.bitwiseAnd v 0x40) == 0x40)

br : Emulator, Flag, Bool -> Emulator
br = \emu0, flag, target ->
    b = getFlag emu0 flag
    (T {cpu: cpu1, mem: mem1} offset) = fetch emu0
    if b == target then
        nextPC =
                addResult = Num.addWrap cpu1.pc (Num.toU16 offset)
                if offset < 0x80 then
                    addResult
                else
                    Num.subWrap addResult 0x100
        {
            cpu: {cpu1 & pc: nextPC},
            mem: mem1
        }
    else
        {
            cpu: cpu1,
            mem: mem1
        }

dec : PartialByteOp
dec = alu \v -> Num.toU16 (Num.subWrap (Num.toU8 v) 1)

inc : PartialByteOp
inc = alu \v -> Num.toU16 (Num.subWrap (Num.toU8 v) 1)

load : Reg -> ByteOp
load = \reg ->
    \emu0, v ->
        emu1 = setFlag emu0 zero (v == 0x00)
        emu2 = setFlag emu1 negative ((Num.bitwiseAnd v 0x80) == 0x80)
        writeReg emu2 reg v

store : Reg -> AddrOp
store = \reg ->
    \{cpu: cpu0, mem: mem0}, addr ->
        readByte = readReg {cpu: cpu0, mem: mem0} reg
        mem1 = writeMem mem0 addr readByte
        {
            cpu: cpu0,
            mem: mem1
        }

jsr : AddrOp
jsr = \emu0, addr ->
    curr = emu0.cpu.pc
    {cpu: cpu1, mem: mem1} = pushAddr emu0 (Num.subWrap curr 1)
    {
        cpu: {cpu1 & pc: addr},
        mem: mem1
    }

transfer : Emulator, Reg, Reg -> Emulator
transfer = \emu0, from, to ->
    # This looks wrong, but I am just gonna copy what the js emulator does
    v = readReg emu0 from
    (T emu1 result) = (alu (\_ -> 0)) emu0 v
    writeReg emu1 to result

rts : Emulator -> Emulator
rts = \emu0 ->
    (T {cpu: cpu1, mem: mem1} addr) = popAddr emu0
    {
        cpu: {cpu1 & pc: Num.addWrap addr 1},
        mem: mem1
    }

step : Emulator -> Emulator
step = \emu0 ->
    (T emu1 op) = fetch emu0
    when op is
        0x69 -> imm emu1 adc
        0x65 -> byVal emu1 zp adc
        0x75 -> byVal emu1 zpX adc
        0x6d -> byVal emu1 abs adc
        0x7d -> byVal emu1 absX adc
        0x79 -> byVal emu1 absY adc
        0x61 -> byVal emu1 xInd adc
        0x71 -> byVal emu1 indY adc

        0x29 -> imm emu1 and
        0x25 -> byVal emu1 zp and
        0x35 -> byVal emu1 zpX and
        0x2d -> byVal emu1 abs and
        0x3d -> byVal emu1 absX and
        0x39 -> byVal emu1 absY and
        0x21 -> byVal emu1 xInd and
        0x31 -> byVal emu1 indY and

        0x0a -> implied emu1 A asl
        0x06 -> inplace emu1 zp asl
        0x16 -> inplace emu1 zpX asl
        0x0e -> inplace emu1 abs asl
        0x1e -> inplace emu1 absX asl

        0x24 -> byVal emu1 zp bit
        0x2c -> byVal emu1 abs bit

        0x10 -> br emu1 negative False
        0x30 -> br emu1 negative True
        0x50 -> br emu1 overflow False
        0x70 -> br emu1 overflow True
        0x90 -> br emu1 carry False
        0xb0 -> br emu1 carry True
        0xd0 -> br emu1 zero False
        0xf0 -> br emu1 zero True

        0xc9 -> imm emu1 (cmp (readReg emu1 A))
        0xc5 -> byVal emu1 zp (cmp (readReg emu1 A))
        0xd5 -> byVal emu1 zpX (cmp (readReg emu1 A))
        0xcd -> byVal emu1 abs (cmp (readReg emu1 A))
        0xdd -> byVal emu1 absX (cmp (readReg emu1 A))
        0xd9 -> byVal emu1 absY (cmp (readReg emu1 A))
        0xc1 -> byVal emu1 xInd (cmp (readReg emu1 A))
        0xd1 -> byVal emu1 indY (cmp (readReg emu1 A))

        0xe0 -> imm emu1 (cmp (readReg emu1 X))
        0xe4 -> byVal emu1 zp (cmp (readReg emu1 X))
        0xec -> byVal emu1 abs (cmp (readReg emu1 X))

        0xc0 -> imm emu1 (cmp (readReg emu1 Y))
        0xc4 -> byVal emu1 zp (cmp (readReg emu1 Y))
        0xcc -> byVal emu1 abs (cmp (readReg emu1 Y))

        0xc6 -> inplace emu1 zp dec
        0xd6 -> inplace emu1 zpX dec
        0xce -> inplace emu1 abs dec
        0xde -> inplace emu1 absX dec
        0xca -> implied emu1 X dec
        0x88 -> implied emu1 Y dec

        0x49 -> imm emu1 eor
        0x45 -> byVal emu1 zp eor
        0x55 -> byVal emu1 zpX eor
        0x4d -> byVal emu1 abs eor
        0x5d -> byVal emu1 absX eor
        0x59 -> byVal emu1 absY eor
        0x41 -> byVal emu1 xInd eor
        0x51 -> byVal emu1 indY eor

        0x18 -> setFlag emu1 carry False
        0x38 -> setFlag emu1 carry True
        0x58 -> setFlag emu1 interruptEnable False
        0x78 -> setFlag emu1 interruptEnable True
        0xb8 -> setFlag emu1 overflow False
        0xd8 -> setFlag emu1 decimal False
        0xf8 -> setFlag emu1 decimal True

        0xe6 -> inplace emu1 zp inc
        0xf6 -> inplace emu1 zpX inc
        0xee -> inplace emu1 abs inc
        0xfe -> inplace emu1 absX inc
        0xea -> implied emu1 X inc
        0xc8 -> implied emu1 Y inc

        0x4c ->
            (T {cpu: cpu2, mem: mem2} addr) = fetchAddr emu1
            {
                cpu: {cpu2 & pc: addr},
                mem: mem2
            }
        0x6c ->
            (T emu2 addr) = fetchAddr emu1
            (T {cpu: cpu3, mem: mem3} memAddr) = readMemAddr emu2 addr
            {
                cpu: {cpu3 & pc: memAddr},
                mem: mem3
            }
        
        0x20 ->
            (T emu2 addr) = fetchAddr emu1
            jsr emu2 addr

        0xa9 -> imm emu1 (load A)
        0xa5 -> byVal emu1 zp (load A)
        0xb5 -> byVal emu1 zpX (load A)
        0xad -> byVal emu1 abs (load A)
        0xbd -> byVal emu1 absX (load A)
        0xb9 -> byVal emu1 absY (load A)
        0xa1 -> byVal emu1 xInd (load A)
        0xb1 -> byVal emu1 indY (load A)

        0xa2 -> imm emu1 (load X)
        0xa6 -> byVal emu1 zp (load X)
        0xb6 -> byVal emu1 zpY (load X)
        0xae -> byVal emu1 abs (load X)
        0xbe -> byVal emu1 absY (load X)

        0xa0 -> imm emu1 (load Y)
        0xa4 -> byVal emu1 zp (load Y)
        0xb4 -> byVal emu1 zpY (load Y)
        0xac -> byVal emu1 abs (load Y)
        0xbc -> byVal emu1 absY (load Y)

        0x4a -> implied emu1 A lsr
        0x46 -> inplace emu1 zp lsr
        0x56 -> inplace emu1 zpX lsr
        0x4e -> inplace emu1 abs lsr
        0x5e -> inplace emu1 absX lsr

        0x09 -> imm emu1 ora
        0x05 -> byVal emu1 zp ora
        0x15 -> byVal emu1 zpX ora
        0x0d -> byVal emu1 abs ora
        0x1d -> byVal emu1 absX ora
        0x19 -> byVal emu1 absY ora
        0x01 -> byVal emu1 xInd ora
        0x11 -> byVal emu1 indY ora

        0xaa -> transfer emu1 A X
        0x8a -> transfer emu1 X A
        0xa8 -> transfer emu1 A Y
        0x98 -> transfer emu1 Y A

        0x2a -> implied emu1 A rol
        0x26 -> inplace emu1 zp rol
        0x36 -> inplace emu1 zpX rol
        0x2e -> inplace emu1 abs rol
        0x3e -> inplace emu1 absX rol

        0x6a -> implied emu1 A ror
        0x66 -> inplace emu1 zp ror
        0x76 -> inplace emu1 zpX ror
        0x6e -> inplace emu1 abs ror
        0x7e -> inplace emu1 absX ror

        0x60 -> rts emu1

        0xe9 -> imm emu1 sbc
        0xe5 -> byVal emu1 zp sbc
        0xf5 -> byVal emu1 zpX sbc
        0xed -> byVal emu1 abs sbc
        0xfd -> byVal emu1 absX sbc
        0xf9 -> byVal emu1 absY sbc
        0xe1 -> byVal emu1 xInd sbc
        0xf1 -> byVal emu1 indY sbc

        0x85 -> byRef emu1 zp (store A)
        0x95 -> byRef emu1 zpX (store A)
        0x8d -> byRef emu1 abs (store A)
        0x9d -> byRef emu1 absX (store A)
        0x99 -> byRef emu1 absY (store A)
        0x81 -> byRef emu1 xInd (store A)
        0x91 -> byRef emu1 indY (store A)

        0x86 -> byRef emu1 zp (store X)
        0x96 -> byRef emu1 zpY (store X)
        0x8e -> byRef emu1 abs (store X)

        0x84 -> byRef emu1 zp (store Y)
        0x94 -> byRef emu1 zpY (store Y) # This line looks bugged, should it be zpX?
        0x8c -> byRef emu1 abs (store Y)

        0x9a -> transfer emu1 X SP
        0xba -> transfer emu1 SP X
        0x48 -> push emu1 (readReg emu1 A)
        0x68 ->
            (T emu2 byte) = pop emu1
            writeReg emu2 A byte
        0x08 -> push emu1 (Num.bitwiseOr (readReg emu1 Status) 0x10)
        0x28 ->
            (T emu2 byte) = pop emu1
            writeReg emu2 Status byte

        _ ->
            cpu1 = emu1.cpu
            {
                # Panic on unknown opcodes.
                cpu: {cpu1 & pc: 0xFF + 1},
                mem: emu1.mem
            }
