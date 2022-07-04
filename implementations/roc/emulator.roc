app "emulator"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

main = \mem ->
    emu = newEmulator 0x438b mem
    if List.len mem == 65536 then
        4142
    else
        -7

Emulator : {
    cpu: CPU,
    mem: Mem,
}

newEmulator : Addr, Mem -> Emulator
newEmulator = \pc, mem ->
    {
        cpu: newCPU pc,
        mem
    }

Addr : U16
Byte : U8
Mem : List U8

CPU : {
    regA: Byte,
    regX: Byte,
    regY: Byte,
    status: Byte,
    sp: Byte,
    pc: Addr,
}

newCPU : Addr -> CPU
newCPU = \pc ->
    {
        regA: 0x00,
        regX: 0x00,
        regY: 0x00,
        status: 0x00,
        sp: 0xFF,
        pc
    }
