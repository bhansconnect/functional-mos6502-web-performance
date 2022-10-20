hosted Effect
    exposes [Effect, after, map, always, forever, loop, readMem, writeMem]
    imports []
    generates Effect with [after, map, always, forever, loop]

readMem : U16 -> Effect U8

writeMem : U16, U8 -> Effect {}
