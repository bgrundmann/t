module Id exposing (Generator, Id, generate, init)


type alias Id =
    Int


type Generator
    = Generator Id


init : Generator
init =
    Generator 0


generate : Generator -> ( Id, Generator )
generate (Generator n) =
    ( n, Generator (n + 1) )
