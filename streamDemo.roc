app "streamDemo"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br",
    }
    imports [pf.Stdout, pf.Utc, pf.Task.{ Task }, Stream.{ Stream, unstream, countFrom, concatMap, repeat, take }]
    provides [main] to pf

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
