platform "emulator"
    requires {} { main : Effect Nat }
    exposes []
    packages {}
    imports [Effect.{ Effect }]
    provides [mainForHost]

mainForHost : Effect Nat as Fx
mainForHost = main
