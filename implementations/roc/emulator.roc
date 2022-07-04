app "emulator"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

main = \x ->
    if List.len x == 65536 then
        4142
    else
        -7
