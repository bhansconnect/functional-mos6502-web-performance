app "emulator"
    packages { pf: "platform/main.roc" }
    imports []
    provides [main] to pf

main = \cnt -> cnt
