platform "emulator"
    requires {} { main : I32 -> I32 }
    exposes []
    packages {}
    imports []
    provides [mainForHost]

mainForHost : I32 -> I32
mainForHost = main
