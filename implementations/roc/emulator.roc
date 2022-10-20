app "emulator"
    packages { pf: "platform/main.roc" }
    imports [pf.Effect.{Effect, after, always, loop, map, readMem}]
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
    byte <- readMem cpu.pc |> after
    always {cpu: { cpu & pc: Num.addWrap cpu.pc 1 }, byte}

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
overflow = 0b0100_0000
negative = 0b1000_0000

ByteOp : {cpu: CPU, byte: Byte} -> Effect CPU
Addressing : CPU -> Effect {cpu: CPU, addr: Addr}
imm : CPU, ByteOp -> Effect CPU
imm = \cpu, op ->
    after (fetch cpu) op

byVal : CPU, Addressing, ByteOp -> Effect CPU
byVal = \cpu, addressing, op ->
    next <- addressing cpu |> after
    byte <- readMem next.addr |> after
    op {cpu: next.cpu, byte}

zp : Addressing
zp = \cpu ->
    out <- fetch cpu |> after
    always {cpu: out.cpu, addr: (Num.toU16 out.byte)}

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

step : CPU -> Effect CPU
step = \cpu ->
    {cpu: fetchCpu, byte: op} <- fetch cpu |> after
    when op is
        0x69 -> imm fetchCpu adc
        _ -> byVal fetchCpu zp adc
