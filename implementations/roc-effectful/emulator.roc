app "emulator"
    packages { pf: "platform/main.roc" }
    imports [pf.Effect.{Effect, after, always, loop, map, readMem, writeMem}]
    provides [main] to pf

main =
    {cpu, cnt} <- loop {cpu: (newCPU 0x438b), cnt: 0}
    if cpu.pc != 0x640b then
        nextCpu <- step cpu |> map
        Step {cpu: nextCpu, cnt: cnt + 1}
    else
        always (Done (Num.toNat cnt))

Addr : U16
Byte : U8
Flag : Byte
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

readReg : CPU, Reg -> Byte
readReg = \cpu, reg ->
    when reg is
        A -> cpu.regA
        X -> cpu.regX
        Y -> cpu.regY
        SP -> cpu.sp
        Status -> cpu.status

writeReg : CPU, Reg, Byte -> CPU
writeReg = \cpu, reg, byte ->
    when reg is
        A -> { cpu & regA: byte }
        X -> { cpu & regX: byte }
        Y -> { cpu & regY: byte }
        SP -> { cpu & sp: byte }
        Status -> { cpu & status: byte }

fetch : CPU -> Effect {cpu: CPU, byte: U8}
fetch = \cpu ->
    byte <- after (readMem cpu.pc)
    always {cpu: { cpu & pc: Num.addWrap cpu.pc 1 }, byte}

toAddr : Byte, Byte -> Addr
toAddr = \lo, hi ->
    Num.bitwiseOr (Num.shiftLeftBy 8 (Num.toU16 hi)) (Num.toU16 lo)

fetchAddr : CPU -> Effect {cpu: CPU, addr: Addr}
fetchAddr = \cpu ->
    lo <- after (fetch cpu)
    hi <- after (fetch lo.cpu)
    always {cpu: hi.cpu, addr: toAddr lo.byte hi.byte}

readMemAddr : Addr -> Effect Addr
readMemAddr = \addr ->
    lo <- after (readMem addr)
    hi <- after (readMem (Num.addWrap addr 1))
    always (toAddr lo hi)

push : CPU, Byte -> Effect CPU
push = \cpu, byte ->
    ptr = Num.toU16 cpu.sp
    _ <- after (writeMem (Num.addWrap ptr 0x100) byte)
    always { cpu & sp: Num.subWrap (Num.toU8 ptr) 1 }

pushAddr : CPU, Addr -> Effect CPU
pushAddr = \cpu, addr ->
    hi = Num.toU8 (Num.shiftRightZfBy 8 addr)
    lo = Num.toU8 (Num.bitwiseAnd addr 0xFF)
    nextCpu <- after (push cpu hi)
    push nextCpu lo

pop : CPU -> Effect {cpu: CPU, byte: Byte}
pop = \cpu ->
    ptr = Num.toU16 (Num.addWrap cpu.sp 1)
    byte <- after (readMem (Num.addWrap ptr 0x100))
    always { cpu: { cpu & sp: Num.toU8 ptr }, byte }

popAddr : CPU -> Effect {cpu: CPU, addr: Addr}
popAddr = \cpu ->
    lo <- after (pop cpu)
    hi <- after (pop lo.cpu)
    always {cpu: hi.cpu, addr: toAddr lo.byte hi.byte}

getFlag : CPU, Flag -> Bool
getFlag = \cpu, flag ->
    Num.bitwiseAnd cpu.status flag != 0

setFlag : CPU, Flag, Bool -> CPU
setFlag = \cpu, flag, b ->
    status =
        if b then
            Num.bitwiseOr cpu.status flag
        else
            Num.bitwiseAnd cpu.status (Num.bitwiseXor flag 0xFF)
    { cpu & status }

carry = 0b0000_0001
zero = 0b0000_0010
interruptEnable = 0b0000_0100
decimal = 0b0000_1000
overflow = 0b0100_0000
negative = 0b1000_0000

PartialByteOp : {cpu: CPU, byte: Byte} -> Effect {cpu: CPU, byte: Byte}
ByteOp : {cpu: CPU, byte: Byte} -> Effect CPU
AddrOp : {cpu: CPU, addr: Addr} -> Effect CPU
Addressing : CPU -> Effect {cpu: CPU, addr: Addr}
imm : CPU, ByteOp -> Effect CPU
imm = \cpu, op ->
    after (fetch cpu) op

byVal : CPU, Addressing, ByteOp -> Effect CPU
byVal = \cpu, addressing, op ->
    next <- after (addressing cpu)
    byte <- after (readMem next.addr)
    op {cpu: next.cpu, byte}

byRef : CPU, Addressing, AddrOp -> Effect CPU
byRef = \cpu, addressing, op ->
    after (addressing cpu) op

inplace : CPU, Addressing, PartialByteOp -> Effect CPU
inplace = \cpu, addressing, op ->
    ref <- byRef cpu addressing
    readByte <- after (readMem ref.addr)
    out <- after (op {cpu: ref.cpu, byte: readByte})
    _ <- after (writeMem ref.addr out.byte)
    always out.cpu

implied : CPU, Reg, PartialByteOp -> Effect CPU
implied = \cpu, reg, op ->
    readByte = readReg cpu reg
    out <- after (op {cpu, byte: readByte})
    always (writeReg out.cpu reg out.byte)

zp : Addressing
zp = \cpu ->
    out <- after (fetch cpu)
    always {cpu: out.cpu, addr: (Num.toU16 out.byte)}

zpX : Addressing
zpX = \cpu ->
    zpOut <- after (zp cpu)
    byte = readReg zpOut.cpu X
    always {cpu: zpOut.cpu, addr: (Num.addWrap zpOut.addr (Num.toU16 byte))}

zpY : Addressing
zpY = \cpu ->
    zpOut <- after (zp cpu)
    byte = readReg zpOut.cpu Y
    always {cpu: zpOut.cpu, addr: (Num.addWrap zpOut.addr (Num.toU16 byte))}

abs : Addressing
abs = \cpu -> fetchAddr cpu

absX : Addressing
absX = \cpu ->
    absOut <- after (abs cpu)
    byte = readReg absOut.cpu X
    always {cpu: absOut.cpu, addr: (Num.addWrap absOut.addr (Num.toU16 byte))}

absY : Addressing
absY = \cpu ->
    absOut <- after (abs cpu)
    byte = readReg absOut.cpu Y
    always {cpu: absOut.cpu, addr: (Num.addWrap absOut.addr (Num.toU16 byte))}

xInd : Addressing
xInd = \cpu ->
    fetchOut <- after (fetch cpu)
    offset = readReg fetchOut.cpu X
    ref = Num.addWrap (Num.toU16 fetchOut.byte) (Num.toU16 offset)
    addr <- after (readMemAddr ref)
    always {cpu: fetchOut.cpu, addr}

indY : Addressing
indY = \cpu ->
    fetchOut <- after (fetch cpu)
    offset = readReg fetchOut.cpu Y
    base <- after (readMemAddr (Num.toU16 fetchOut.byte))
    addr = Num.addWrap base (Num.toU16 offset)
    always {cpu: fetchOut.cpu, addr}

signed : CPU, (Addr, Addr, Bool -> Addr), Addr, Addr -> {cpu: CPU, byte: Byte}
signed = \cpu, f, v1, v2 ->
    c0 = getFlag cpu carry
    result = f v1 v2 c0
    #TODO: BCD

    cpu
        |> setFlag overflow ((Num.bitwiseAnd result 0x80) != (Num.bitwiseAnd (Num.bitwiseAnd v1 v2) 0x80))
        |> setFlag carry (result >= 0x100)
        |> setFlag zero ((Num.bitwiseAnd result 0xFF) == 0x00)
        |> setFlag negative ((Num.bitwiseAnd result 0x80) == 0x80)
        |> \out -> {cpu: out, byte: (Num.toU8 result)}

adc : ByteOp
adc = \{cpu, byte} ->
    a = readReg cpu A
    signedOut = signed cpu
        (\v1, v2, c0 ->
            Num.addWrap (Num.addWrap v1 v2) (if c0 then 1 else 0)
        ) (Num.toU16 a) (Num.toU16 byte)
    always (writeReg signedOut.cpu A signedOut.byte)

sub : CPU, Addr, Addr -> {cpu: CPU, byte: Byte}
sub = \cpu, v1, v2 ->
    # Not sure why this doesn't use signed, but just following the javascript code.
    c0 = getFlag cpu carry
    extended = Num.subWrap (Num.subWrap v1 v2) (if c0 then 0 else 1)
    cpu
        |> setFlag overflow ((Num.bitwiseAnd extended 0x80) != (Num.bitwiseAnd (Num.bitwiseAnd v1 v2) 0x80))
        |> setFlag carry (extended >= 0x100)
        |> setFlag zero ((Num.bitwiseAnd extended 0xFF) == 0x00)
        |> setFlag negative ((Num.bitwiseAnd extended 0x80) == 0x80)
        |> \out -> {cpu: out, byte: (Num.toU8 extended)}

cmp : Byte -> ByteOp
cmp = \a ->
    \{cpu, byte} ->
        cpu
            |> setFlag carry True
            |> sub (Num.toU16 a) (Num.toU16 byte)
            |> \out -> always out.cpu

sbc : ByteOp
sbc = \{cpu, byte} ->
    a = readReg cpu A
    subOut = sub cpu (Num.toU16 a) (Num.toU16 byte)
    always (writeReg subOut.cpu A subOut.byte)

alu : (Addr -> Addr) -> PartialByteOp
alu = \f ->
    \{cpu, byte} ->
        result = f (Num.toU16 byte)
        cpu
            |> setFlag zero ((Num.bitwiseAnd result 0xFF) == 0x00)
            |> setFlag negative ((Num.bitwiseAnd result 0x80) == 0x80)
            |> \out -> always {cpu: out, byte: Num.toU8 result}

and : ByteOp
and = \{cpu, byte} ->
    a = readReg cpu A
    aluOut <- after ((alu (\v -> Num.bitwiseAnd (Num.toU16 byte) v)) {cpu, byte: a})
    always (writeReg aluOut.cpu A aluOut.byte)

eor : ByteOp
eor = \{cpu, byte} ->
    a = readReg cpu A
    aluOut <- after ((alu (\v -> Num.bitwiseXor (Num.toU16 byte) v)) {cpu, byte: a})
    always (writeReg aluOut.cpu A aluOut.byte)

ora : ByteOp
ora = \{cpu, byte} ->
    a = readReg cpu A
    aluOut <- after ((alu (\v -> Num.bitwiseOr (Num.toU16 byte) v)) {cpu, byte: a})
    always (writeReg aluOut.cpu A aluOut.byte)

shiftRot : (Bool, Byte -> {c: Byte, v: Byte}) -> PartialByteOp
shiftRot = \f ->
    \{cpu, byte: v} ->
        c = getFlag cpu carry
        cv = f c v
        cpu
            |> setFlag carry (cv.c != 0)
            |> setFlag zero (cv.v == 0x00)
            |> setFlag negative ((Num.bitwiseAnd cv.v 0x80) == 0x80)
            |> \out -> always {cpu: out, byte: cv.v}

asl : PartialByteOp
asl = shiftRot (\_c,v -> { c: Num.bitwiseAnd v 0x80, v: Num.shiftLeftBy 1 v })

lsr : PartialByteOp
lsr = shiftRot (\_c,v -> { c: Num.bitwiseAnd v 0x01, v: Num.shiftRightZfBy 1 v })

rol : PartialByteOp
rol = shiftRot (\c,v -> { c: Num.bitwiseAnd v 0x80, v: Num.bitwiseOr (Num.shiftLeftBy 1 v) (if c then 0x01 else 0x00) })

ror : PartialByteOp
ror = shiftRot (\c,v -> { c: Num.bitwiseAnd v 0x01, v: Num.bitwiseOr (Num.shiftRightZfBy 1 v) (if c then 0x80 else 0x00) })

bit : ByteOp
bit = \{cpu, byte: v} ->
    a = readReg cpu A
    cpu
        |> setFlag zero ((Num.bitwiseAnd a v) == 0x00)
        |> setFlag negative ((Num.bitwiseAnd v 0x80) == 0x80)
        |> setFlag overflow ((Num.bitwiseAnd v 0x40) == 0x40)
        |> always

br : CPU, Flag, Bool -> Effect CPU
br = \cpu, flag, target ->
    b = getFlag cpu flag
    {cpu: outCpu, byte: offset} <- after (fetch cpu)
    if b == target then
        nextPC =
                addResult = Num.addWrap outCpu.pc (Num.toU16 offset)
                if offset < 0x80 then
                    addResult
                else
                    Num.subWrap addResult 0x100
        always {outCpu & pc: nextPC}
    else
        always outCpu

dec : PartialByteOp
dec = alu \v -> Num.toU16 (Num.subWrap (Num.toU8 v) 1)

inc : PartialByteOp
inc = alu \v -> Num.toU16 (Num.subWrap (Num.toU8 v) 1)

load : Reg -> ByteOp
load = \reg ->
    \{cpu, byte: v} ->
        cpu
            |> setFlag zero (v == 0x00)
            |> setFlag negative ((Num.bitwiseAnd v 0x80) == 0x80)
            |> writeReg reg v
            |> always

store : Reg -> AddrOp
store = \reg ->
    \{cpu, addr} ->
        readByte = readReg cpu reg
        _ <- after (writeMem addr readByte)
        always cpu

jsr : AddrOp
jsr = \{cpu, addr} ->
    curr = cpu.pc
    out <- after (pushAddr cpu (Num.subWrap curr 1))
    always {out & pc: addr}

transfer : CPU, Reg, Reg -> Effect CPU
transfer = \cpu, from, to ->
    # This looks wrong, but I am just gonna copy what the js emulator does
    byte = readReg cpu from
    out <- after ((alu (\_ -> 0)) {cpu, byte})
    always (writeReg out.cpu to out.byte)

rts : CPU -> Effect CPU
rts = \cpu ->
    {cpu: outCpu, addr: outAddr} <- after (popAddr cpu)
    always {outCpu & pc: Num.addWrap outAddr 1}

step : CPU -> Effect CPU
step = \cpu ->
    {cpu: fetchCpu, byte: op} <- after (fetch cpu)
    when op is
        0x69 -> imm fetchCpu adc
        0x65 -> byVal fetchCpu zp adc
        0x75 -> byVal fetchCpu zpX adc
        0x6d -> byVal fetchCpu abs adc
        0x7d -> byVal fetchCpu absX adc
        0x79 -> byVal fetchCpu absY adc
        0x61 -> byVal fetchCpu xInd adc
        0x71 -> byVal fetchCpu indY adc

        0x29 -> imm fetchCpu and
        0x25 -> byVal fetchCpu zp and
        0x35 -> byVal fetchCpu zpX and
        0x2d -> byVal fetchCpu abs and
        0x3d -> byVal fetchCpu absX and
        0x39 -> byVal fetchCpu absY and
        0x21 -> byVal fetchCpu xInd and
        0x31 -> byVal fetchCpu indY and

        0x0a -> implied fetchCpu A asl
        0x06 -> inplace fetchCpu zp asl
        0x16 -> inplace fetchCpu zpX asl
        0x0e -> inplace fetchCpu abs asl
        0x1e -> inplace fetchCpu absX asl

        0x24 -> byVal fetchCpu zp bit
        0x2c -> byVal fetchCpu abs bit

        0x10 -> br fetchCpu negative False
        0x30 -> br fetchCpu negative True
        0x50 -> br fetchCpu overflow False
        0x70 -> br fetchCpu overflow True
        0x90 -> br fetchCpu carry False
        0xb0 -> br fetchCpu carry True
        0xd0 -> br fetchCpu zero False
        0xf0 -> br fetchCpu zero True

        0xc9 -> imm fetchCpu (cmp (readReg fetchCpu A))
        0xc5 -> byVal fetchCpu zp (cmp (readReg fetchCpu A))
        0xd5 -> byVal fetchCpu zpX (cmp (readReg fetchCpu A))
        0xcd -> byVal fetchCpu abs (cmp (readReg fetchCpu A))
        0xdd -> byVal fetchCpu absX (cmp (readReg fetchCpu A))
        0xd9 -> byVal fetchCpu absY (cmp (readReg fetchCpu A))
        0xc1 -> byVal fetchCpu xInd (cmp (readReg fetchCpu A))
        0xd1 -> byVal fetchCpu indY (cmp (readReg fetchCpu A))

        0xe0 -> imm fetchCpu (cmp (readReg fetchCpu X))
        0xe4 -> byVal fetchCpu zp (cmp (readReg fetchCpu X))
        0xec -> byVal fetchCpu abs (cmp (readReg fetchCpu X))

        0xc0 -> imm fetchCpu (cmp (readReg fetchCpu Y))
        0xc4 -> byVal fetchCpu zp (cmp (readReg fetchCpu Y))
        0xcc -> byVal fetchCpu abs (cmp (readReg fetchCpu Y))

        0xc6 -> inplace fetchCpu zp dec
        0xd6 -> inplace fetchCpu zpX dec
        0xce -> inplace fetchCpu abs dec
        0xde -> inplace fetchCpu absX dec
        0xca -> implied fetchCpu X dec
        0x88 -> implied fetchCpu Y dec

        0x49 -> imm fetchCpu eor
        0x45 -> byVal fetchCpu zp eor
        0x55 -> byVal fetchCpu zpX eor
        0x4d -> byVal fetchCpu abs eor
        0x5d -> byVal fetchCpu absX eor
        0x59 -> byVal fetchCpu absY eor
        0x41 -> byVal fetchCpu xInd eor
        0x51 -> byVal fetchCpu indY eor

        0x18 -> always (setFlag fetchCpu carry False)
        0x38 -> always (setFlag fetchCpu carry True)
        0x58 -> always (setFlag fetchCpu interruptEnable False)
        0x78 -> always (setFlag fetchCpu interruptEnable True)
        0xb8 -> always (setFlag fetchCpu overflow False)
        0xd8 -> always (setFlag fetchCpu decimal False)
        0xf8 -> always (setFlag fetchCpu decimal True)

        0xe6 -> inplace fetchCpu zp inc
        0xf6 -> inplace fetchCpu zpX inc
        0xee -> inplace fetchCpu abs inc
        0xfe -> inplace fetchCpu absX inc
        0xe8 -> implied fetchCpu X inc
        0xc8 -> implied fetchCpu Y inc

        0x4c ->
            {cpu: outCpu, addr} <- after (fetchAddr fetchCpu)
            always {outCpu & pc: addr}
        0x6c ->
            {cpu: outCpu, addr} <- after (fetchAddr fetchCpu)
            memAddr <- after (readMemAddr addr)
            always {outCpu & pc: memAddr}
        
        0x20 ->
            after (fetchAddr fetchCpu) jsr

        0xa9 -> imm fetchCpu (load A)
        0xa5 -> byVal fetchCpu zp (load A)
        0xb5 -> byVal fetchCpu zpX (load A)
        0xad -> byVal fetchCpu abs (load A)
        0xbd -> byVal fetchCpu absX (load A)
        0xb9 -> byVal fetchCpu absY (load A)
        0xa1 -> byVal fetchCpu xInd (load A)
        0xb1 -> byVal fetchCpu indY (load A)

        0xa2 -> imm fetchCpu (load X)
        0xa6 -> byVal fetchCpu zp (load X)
        0xb6 -> byVal fetchCpu zpY (load X)
        0xae -> byVal fetchCpu abs (load X)
        0xbe -> byVal fetchCpu absY (load X)

        0xa0 -> imm fetchCpu (load Y)
        0xa4 -> byVal fetchCpu zp (load Y)
        0xb4 -> byVal fetchCpu zpY (load Y)
        0xac -> byVal fetchCpu abs (load Y)
        0xbc -> byVal fetchCpu absY (load Y)

        0x4a -> implied fetchCpu A lsr
        0x46 -> inplace fetchCpu zp lsr
        0x56 -> inplace fetchCpu zpX lsr
        0x4e -> inplace fetchCpu abs lsr
        0x5e -> inplace fetchCpu absX lsr

        0x09 -> imm fetchCpu ora
        0x05 -> byVal fetchCpu zp ora
        0x15 -> byVal fetchCpu zpX ora
        0x0d -> byVal fetchCpu abs ora
        0x1d -> byVal fetchCpu absX ora
        0x19 -> byVal fetchCpu absY ora
        0x01 -> byVal fetchCpu xInd ora
        0x11 -> byVal fetchCpu indY ora

        0xaa -> transfer fetchCpu A X
        0x8a -> transfer fetchCpu X A
        0xa8 -> transfer fetchCpu A Y
        0x98 -> transfer fetchCpu Y A

        0x2a -> implied fetchCpu A rol
        0x26 -> inplace fetchCpu zp rol
        0x36 -> inplace fetchCpu zpX rol
        0x2e -> inplace fetchCpu abs rol
        0x3e -> inplace fetchCpu absX rol

        0x6a -> implied fetchCpu A ror
        0x66 -> inplace fetchCpu zp ror
        0x76 -> inplace fetchCpu zpX ror
        0x6e -> inplace fetchCpu abs ror
        0x7e -> inplace fetchCpu absX ror

        0x60 -> rts fetchCpu

        0xe9 -> imm fetchCpu sbc
        0xe5 -> byVal fetchCpu zp sbc
        0xf5 -> byVal fetchCpu zpX sbc
        0xed -> byVal fetchCpu abs sbc
        0xfd -> byVal fetchCpu absX sbc
        0xf9 -> byVal fetchCpu absY sbc
        0xe1 -> byVal fetchCpu xInd sbc
        0xf1 -> byVal fetchCpu indY sbc

        0x85 -> byRef fetchCpu zp (store A)
        0x95 -> byRef fetchCpu zpX (store A)
        0x8d -> byRef fetchCpu abs (store A)
        0x9d -> byRef fetchCpu absX (store A)
        0x99 -> byRef fetchCpu absY (store A)
        0x81 -> byRef fetchCpu xInd (store A)
        0x91 -> byRef fetchCpu indY (store A)

        0x86 -> byRef fetchCpu zp (store X)
        0x96 -> byRef fetchCpu zpY (store X)
        0x8e -> byRef fetchCpu abs (store X)

        0x84 -> byRef fetchCpu zp (store Y)
        0x94 -> byRef fetchCpu zpY (store Y) # This line looks bugged, should it be zpX?
        0x8c -> byRef fetchCpu abs (store Y)

        0x9a -> transfer fetchCpu X SP
        0xba -> transfer fetchCpu SP X
        0x48 -> push fetchCpu (readReg fetchCpu A)
        0x68 ->
            out <- after (pop fetchCpu)
            always (writeReg out.cpu A out.byte)
        0x08 -> push fetchCpu (Num.bitwiseOr (readReg fetchCpu Status) 0x10)
        0x28 ->
            out <- after (pop fetchCpu)
            always (writeReg out.cpu Status out.byte)

        _ ->
            always {fetchCpu & pc: 0x0 - 1}
