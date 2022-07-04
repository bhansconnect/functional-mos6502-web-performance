platform "emulator"
    requires {} { main : List U8 -> Nat }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : List U8 -> Nat
mainForHost = \data -> main data
