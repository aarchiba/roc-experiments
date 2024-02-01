app "streams"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br",
    }
    imports [pf.Stdout, pf.Utc, pf.Task.{ Task }]
    provides [main] to pf

Step a s : [Stop, Skip s, Yield a s]
Stream a s : [Stream (s -> Step a s) s]

stream = \ls ->
    listNext = \{ list, n } ->
        when List.get list n is
            Ok val -> Yield val { list, n: n + 1 }
            Err _ -> Stop
    Stream listNext { list: ls, n: 0 }

unstream : Stream a s -> List a
unstream = \Stream next s ->
    loop : List a, s -> List a
    loop = \list, s1 ->
        when next s1 is
            Yield val s2 -> loop (List.append list val) s2
            Skip s2 -> loop list s2
            Stop -> list
    loop [] s

map : Stream a s, (a -> b) -> Stream b s
map = \Stream next s, f ->
    next1 = \s1 ->
        when next s1 is
            Yield val s2 -> Yield (f val) s2
            Skip s2 -> Skip s2
            Stop -> Stop
    Stream next1 s

filter : Stream a s, (a -> Bool) -> Stream a s
filter = \Stream next s, f ->
    next1 = \s1 ->
        when next s1 is
            Yield val s2 -> if f val then Yield val s2 else Skip s2
            Skip s2 -> Skip s2
            Stop -> Stop
    Stream next1 s

fold = \Stream next s, b, f ->
    loop : b, s -> b
    loop = \acc, s1 ->
        when next s1 is
            Yield val s2 -> loop (f acc val) s2
            Skip s2 -> loop acc s2
            Stop -> acc
    loop b s

return = \a ->
    loop = \state ->
        when state is
            NotEmitted -> Yield a Emitted
            Emitted -> Stop
    Stream loop NotEmitted

repeat = \a -> Stream (\{} -> Yield a {}) {}

take = \Stream next s0, n0 ->
    next1 = \{ s, n } ->
        if n > 0 then
            when next s is
                Yield val s1 -> Yield val { s: s1, n: n - 1 }
                Skip s1 -> Skip { s: s1, n }
                Stop -> Stop
        else
            Stop
    Stream next1 { s: s0, n: n0 }

zip = \Stream nextLeft left0, Stream nextRight right0 ->
    loop = \{ left, right, v } ->
        when v is
            NoValue ->
                when nextLeft left is
                    Yield val left2 -> Skip { left: left2, right, v: Value1 val }
                    Skip left2 -> Skip { left: left2, right, v }
                    Stop -> Stop

            Value1 val1 ->
                when nextRight right is
                    Yield val2 right2 -> Yield (Pair val1 val2) { left, right: right2, v: NoValue }
                    Skip right2 -> Skip { left, right: right2, v }
                    Stop -> Stop
    Stream loop { left: left0, right: right0, v: NoValue }

countFrom = \n ->
    loop = \n1 ->
        Yield n1 (n1 + 1)
    Stream loop n

enumerate = \s -> zip (countFrom 0) s

concatMap = \Stream next s0, f ->
    loop = \{ s, inner } ->
        when inner is
            NoInner ->
                when next s is
                    Yield val s1 -> when (f val) is
                        Stream innerNext innerS -> Skip { s: s1, inner: Inner innerNext innerS}
                    Skip s1 -> Skip { s: s1, inner: NoInner }
                    Stop -> Stop
            Inner innerNext innerS ->
                when innerNext innerS is
                    Yield val innerS1 -> Yield val { s, inner: Inner innerNext innerS1 }
                    Skip innerS1 -> Skip { s, inner: Inner innerNext innerS1 }
                    Stop -> Skip { s, inner: NoInner }
    Stream loop { s: s0, inner: NoInner }

main =
    startTime <- Utc.now |> Task.await

    line =
        countFrom 0
        |> concatMap (\n -> repeat n |> take n)
        |> map Num.toStr
        |> take 10
        |> unstream
        |> Str.joinWith ", "
    {} <- Stdout.line "line: $(line)" |> Task.await

    endTime <- Utc.now |> Task.await

    runTime = Utc.deltaAsMillis startTime endTime |> Num.toStr
    {} <- Stdout.line "Run time: $(runTime) ms" |> Task.await

    # Final task doesn't need to be awaited
    Stdout.line "Done"
