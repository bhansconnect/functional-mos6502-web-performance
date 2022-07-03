app "emulator"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

main = "Roc <3 Web Assembly!\n"
