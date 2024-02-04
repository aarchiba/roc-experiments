app "generatorDemo"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br",
    }
    imports [pf.Stdout, pf.Utc, pf.Task.{ Task }, Stream.{ Stream, unstream, countFrom, concatMap, repeat, take }]
    provides [main] to pf

demoYield : Nat -> YieldValue Nat
demoYield = \n ->
    {} <- YieldValue n
    {} <- YieldValue (n + 1)
    {} <- YieldValue (n + 2)
    {} <- YieldValue (n + 3)
    Stop

demoYield3 = \n ->
    {} <- YieldValue n
    demoYield n

# walkYield converts function producing YieldValues into a list
# directly; this isn't really meant to be used - apply generator,
# below, to get a stream and then do things with that - but it's
# useful for testing.
walkYield : List Nat, YieldValue Nat -> List Nat
walkYield = \list, thing ->
    when thing is
        YieldValue n f -> walkYield (List.append list n) (f {})
        Stop -> list

expect (walkYield [] (demoYield 1)) == [1, 2, 3, 4]
expect (walkYield [] (demoYield3 1)) == [1, 1, 2, 3, 4]

expect (streamYield (demoYield 1) |> unstream) == [1, 2, 3, 4]

# YieldValue functions work recursively
demoYield4 = \n ->
    {} <- YieldValue n
    demoYield4 (n + 2)

expect (streamYield (demoYield4 1) |> take 4 |> unstream) == [1, 3, 5, 7]

# Test generator yielding the contents of a list
# no real reason to use this instead of stream
yieldFromList : List a -> YieldValue a
yieldFromList = \list ->
    when list is
        [] -> Stop
        [x, .. as xs] ->
            {} <- YieldValue x
            yieldFromList xs

expect (streamYield (yieldFromList [1, 2, 3, 4]) |> unstream) == [1, 2, 3, 4]

demoYield5 = \n -> generator
        (
            {} <- YieldValue n
            {} <- YieldValue (n + 1)
            {} <- YieldValue (n + 2)
            {} <- YieldValue (n + 3)
            Stop
        )

expect (demoYield5 1 |> unstream) == [1, 2, 3, 4]

# demoYield7 : {} -> YieldValue [Red, Blue]
# demoYield7 = \{} ->
#     {} <- YieldValue Red
#     {} <- YieldValue Blue
#     yieldFrom (streamYield (demoYield2 {}))

# expect streamYield (demoYield7 {}) |> take 5 |> unstream == [Red, Blue, Red, Blue, Red]

main =
    startTime <- Utc.now |> Task.await

    line =
        Stream.countFrom 0
        |> Stream.concatMap (\n -> Stream.repeat n |> Stream.take n)
        |> Stream.map Num.toStr
        |> Stream.take 10
        |> Stream.unstream
        |> Str.joinWith ", "
    {} <- Stdout.line "line: $(line)" |> Task.await

    endTime <- Utc.now |> Task.await

    runTime = Utc.deltaAsMillis startTime endTime |> Num.toStr
    Stdout.line "Run time: $(runTime) ms"
