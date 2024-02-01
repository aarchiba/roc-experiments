app "mergesort"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br",
    }
    imports [pf.Stdout, pf.Stderr, pf.Utc, pf.Task.{ Task }]
    provides [main] to pf

mergesort = \list ->
    n = List.len list
    r = List.split list (n//2)
    when n is
        0 -> list
        1 -> list
        _ -> mergeSorted (List.withCapacity n) (mergesort r.before) (mergesort r.others)

mergeSorted = \accum, left, right ->
    when left is
        [] -> List.concat right (List.reverse accum)
        [.. as leftRest, leftLast] ->
            when right is
                [] -> List.concat left (List.reverse accum)
                [.. as rightRest, rightLast] ->
                    if leftLast > rightLast then
                        mergeSorted (List.append accum leftLast) leftRest right
                    else
                        mergeSorted (List.append accum rightLast) left rightRest

repeat = \f, thing, n ->
    when n is
        0 -> thing
        _ -> repeat f (f thing) (n - 1)

isSorted: List (Num a) -> Bool
isSorted = \list ->
    when list is
        [] -> Bool.true
        [_] -> Bool.true
        [a, b, ..] if b < a -> Bool.false
        [_, .. as rest] -> isSorted rest


startingList = List.range { start: At 1, end: At 10000 } |> List.reverse

expect Bool.not (startingList |> isSorted)
expect mergesort startingList |> isSorted

run =
    iterations = 100

    {} <- Stdout.line "Running $(Num.toStr iterations) iterations on a list of size $(List.len startingList |> Num.toStr)" |> Task.await

    # Get time since [Unix Epoch](https://en.wikipedia.org/wiki/Unix_time)
    startTime <- Utc.now |> Task.await

    pass = repeat mergesort startingList iterations |> isSorted

    endTime <- Utc.now |> Task.await
    runTime = Utc.deltaAsMillis startTime endTime |> Num.toStr

    {} <- Stdout.line (if pass then "Passed" else "Failed") |> Task.await


    {} <- Stdout.line "Run time: $(runTime) ms" |> Task.await

    # Final task doesn't need to be awaited
    Stdout.line "Done"

main : Task {} *
main =
    run
    |> Task.onErr handleErr

handleErr = \err ->
    usage = "HELLO=1 roc main.roc -- \"https://www.roc-lang.org\" roc.html"

    errorMsg =
        when err is
            FailedToReadArgs -> "Failed to read command line arguments, usage: $(usage)"
            FailedToFetchHtml httpErr -> "Failed to fetch URL $(httpErr), usage: $(usage)"
            FailedToListCwd -> "Failed to list contents of current directory, usage: $(usage)"

    Stderr.line "Error: $(errorMsg)"
