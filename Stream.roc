interface Stream exposes [
        stream,
        unstream,
        map,
        filter,
        foldl,
        foldr,
        return,
        append,
        repeat,
        take,
        zip,
        countFrom,
        enumerate,
        concatMap,
        drop,
        head,
        take,
        unstreamOntoEnd,
        unsstreamWithCapacity,
        either,
        thunkStream,
    ] imports []

# ## Functions from Coutts et al.

# ### list conversion to/from
# It would be great if unstream then stream could be elided

Step a s : [Yield a s, Skip s, Stop]
Stream a s : [Stream (s -> Step a s) s]

ListIterationState a : { list : List a, n : Nat }

# fromList
stream : List a -> Stream a (ListIterationState a)
stream = \ls ->
    listNext : ListIterationState a -> Step a (ListIterationState a)
    listNext = \{ list, n } ->
        when List.get list n is
            Ok val -> Yield val { list, n: n + 1 }
            Err _ -> Stop

    start : ListIterationState a
    start = { list: ls, n: 0nat }
    Stream listNext start

# toListAppend
unstreamIntoList : Stream a s, List a -> List a
unstreamIntoList = \Stream next s, list ->
    loop : List a, s -> List a
    loop = \list1, s1 ->
        when next s1 is
            Yield val s2 -> loop (List.append list1 val) s2
            Skip s2 -> loop list1 s2
            Stop -> list1
    loop list s

# toList
unstream : Stream a s -> List a
unstream = \Stream next s ->
    unstreamIntoList (Stream next s) []

expect ([Red, Fish, Blue, Fish] |> stream |> unstream) == [Red, Fish, Blue, Fish]

# ### From Figure 1 & 2

filter : Stream a s, (a -> Bool) -> Stream a s
filter = \Stream next s, f ->
    next1 = \s1 ->
        when next s1 is
            Yield val s2 -> if f val then Yield val s2 else Skip s2
            Skip s2 -> Skip s2
            Stop -> Stop
    Stream next1 s

return = \a ->
    loop = \state ->
        when state is
            NotEmitted -> Yield a Emitted
            Emitted -> Stop
    Stream loop NotEmitted

enumFromTo = \from, to1 ->
    loop = \n ->
        if n <= to1 then
            Yield n (n + 1)
        else
            Stop
    Stream loop from

expect (enumFromTo 1 5 |> unstream) == [1, 2, 3, 4, 5]

foldr = \Stream next s, b, f ->
    loop = \acc, s1 ->
        when next s1 is
            Yield val s2 -> Yield (loop acc s2 |> f val) s2
            Skip s2 -> loop acc s2
            Stop -> acc
    loop b s

foldl = \Stream next s, b, f ->
    loop : b, s -> b
    loop = \acc, s1 ->
        when next s1 is
            Yield val s2 -> loop (f acc val) s2
            Skip s2 -> loop acc s2
            Stop -> acc
    loop b s

append = \Stream nextLeft left0, Stream nextRight right0 ->
    loop = \running ->
        when running is
            Left left1 ->
                when nextLeft left1 is
                    Yield val left2 -> Yield val (Left left2)
                    Skip left2 -> Skip (Left left2)
                    Stop -> Skip (Right right0)

            Right right1 ->
                when nextRight right1 is
                    Yield val right2 -> Yield val (Right right2)
                    Skip right2 -> Skip (Right right2)
                    Stop -> Stop
    Stream loop (Left left0)

expect ([Red, Fish] |> stream |> append ([Blue, Fish] |> stream) |> unstream) == [Red, Fish, Blue, Fish]

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
                    Yield val2 right2 -> Yield (val1, val2) { left, right: right2, v: NoValue }
                    Skip right2 -> Skip { left, right: right2, v }
                    Stop -> Stop
    Stream loop { left: left0, right: right0, v: NoValue }

expect zip ([Red, Fish] |> stream) ([Blue, Fish] |> stream) |> unstream == [(Red, Blue), (Fish, Fish)]

concatMap = \Stream next s0, f ->
    loop = \{ s, inner } ->
        when inner is
            NoInner ->
                when next s is
                    Yield val s1 ->
                        when f val is
                            Stream innerNext innerS -> Skip { s: s1, inner: Inner innerNext innerS }

                    Skip s1 -> Skip { s: s1, inner: NoInner }
                    Stop -> Stop

            Inner innerNext innerS ->
                when innerNext innerS is
                    Yield val innerS1 -> Yield val { s, inner: Inner innerNext innerS1 }
                    Skip innerS1 -> Skip { s, inner: Inner innerNext innerS1 }
                    Stop -> Skip { s, inner: NoInner }
    Stream loop { s: s0, inner: NoInner }

expect concatMap ([Red, Fish] |> stream) (\a -> [a, a] |> stream) |> unstream == [Red, Red, Fish, Fish]

# ## Other useful functions

countFrom = \n ->
    loop = \n1 ->
        Yield n1 (n1 + 1)
    Stream loop n

enumerate = \s -> zip (countFrom 0) s

map : Stream a s, (a -> b) -> Stream b s
map = \Stream next s, f ->
    next1 = \s1 ->
        when next s1 is
            Yield val s2 -> Yield (f val) s2
            Skip s2 -> Skip s2
            Stop -> Stop
    Stream next1 s

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

drop = \Stream next s0, n0 ->
    next1 = \{ s, n } ->
        if n > 0 then
            when next s is
                Yield _ s1 -> Skip { s: s1, n: n - 1 }
                Skip s1 -> Skip { s: s1, n }
                Stop -> Stop
        else
            when next s is
                Yield val s1 -> Yield val { s: s1, n }
                Skip s1 -> Skip { s: s1, n }
                Stop -> Stop
    Stream next1 { s: s0, n: n0 }

head = \Stream next s ->
    when next s is
        Yield val _ -> Ok val
        Skip s1 -> head (Stream next s1)
        Stop -> Err OutOfBounds

unstreamOntoEnd : Stream a s, List a -> List a
unstreamOntoEnd = \Stream next s, list ->
    loop = \list1, s1 ->
        when next s1 is
            Yield val s2 -> loop (List.append list1 val) s2
            Skip s2 -> loop list1 s2
            Stop -> list1
    loop list s

unsstreamWithCapacity : Stream a s, Nat -> List a
unsstreamWithCapacity = \Stream next s, capacity ->
    unstreamOntoEnd (Stream next s) (List.withCapacity capacity)

thunkStream : Stream a s -> Stream a ({} -> Step a s)
thunkStream = \Stream next s ->
    loop = \f ->
        when f {} is
            Yield val s1 -> Yield val (\{} -> next s1)
            Skip s1 -> Skip (\{} -> next s1)
            Stop -> Stop
    Stream loop (\{} -> next s)

expect (countFrom 0 |> take 5 |> unstream) == (countFrom 0 |> thunkStream |> take 5 |> unstream)

either = \Stream nextLeft sLeft, Stream nextRight sRight, which ->
    when which is
        Left -> thunkStream (Stream nextLeft sLeft)
        Right -> thunkStream (Stream nextRight sRight)
