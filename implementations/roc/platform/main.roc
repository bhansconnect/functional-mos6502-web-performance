platform "emulator"
    requires {} { main : List U8 -> I32 }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : List U8 -> I32
mainForHost = \data -> main data
