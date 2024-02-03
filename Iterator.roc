interface Iterator exposes [
        Iterable,
        countFrom,
        toList,
        toListAppend,
        # take
    ] imports []

Step a s : [Yield a s, Skip s, Stop]

NextFunction a : [Func {} -> Step a (NextFunction a), Nothing]

countFrom2 = \n -> Yield n (\{} -> countFrom2 (n + 1)) 

Iterable implements
    next : s -> Step a s where s implements Iterable

# Let's sort out iterators first

CountFrom := U64 implements [Iterable { next: countFromNext }]
countFromNext = \@CountFrom n -> Yield n (@CountFrom (n + 1))
countFrom = \n -> @CountFrom n

expect
    when countFrom 1 is
        @CountFrom 1 -> Bool.true
        _ -> Bool.false
expect
    when next (countFrom 1) is
        Yield 1 _ -> Bool.true
        _ -> Bool.false

toListAppend = \iterable, list ->
    when next iterable is
        Yield value state ->
            toListAppend state (List.append list value)

        Skip state ->
            toListAppend state list

        Stop ->
            list

toList = \iterator -> toListAppend iterator []

Return a := [Value a, Done] implements [Iterable { next: returnNext }]
returnNext = \@Return r ->
    when r is
        Value value -> Yield value (@Return Done)
        Done -> Stop
return = \value -> @Return (Value value)

# expect return Thing |> toList == [Thing]

Take a := { n : U64, iterable : a } where a implements Iterable
    implements [Iterable { next: takeNext }]
takeNext : Take b -> Step a (Take b)
takeNext = \@Take state ->
    if state.n == 0 then
        Stop
    else
        when next state.iterable is
            Yield value nextIterable ->
                Yield value (@Take { n: (state.n - 1), iterable: nextIterable })

            Skip nextIterable ->
                Skip (@Take { n: state.n, iterable: nextIterable })

            Stop ->
                Stop
take = \iterable, n -> @Take { n, iterable }

expect return Thing |> take 10 |> toList == [Thing]
expect return Thing |> take 1 |> toList == [Thing]

# This crashes the compiler
# expect return "a" |> take 0 |> toList == []

# These crash the compiler too
# expect countFrom 1 |> take 10 |> toList == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
# expect countFrom 1 |> take 3 |> take 10 |> toList == [1, 2, 3]
# expect countFrom 1 |> take 0 |> toList == []

countOrReturn = \which ->
    when which is
        Return n -> return n
        Count n -> countFrom n

# Map a b c := { f : a -> b, iterable : c } where c implements Iterable
#     implements [Iterable { next: mapNext }]
# # mapNext : Map a b c -> (Step a (Map a b c)) where c implements Iterable
# mapNext = \state ->
#     when next state.iterable is
#         Yield value nextIterable ->
#             Yield (state.f value) (@Map { f: state.f, iterable: nextIterable })

#         Skip nextIterable ->
#             Skip (@Map { f: state.f, iterable: nextIterable })

#         Stop ->
#             Stop
# # map : (a -> b), c -> Map a b c where c implements Iterable
# map = \f, iterable -> @Map { f, iterable }

# expect countFrom 1 |> map (\x -> x + 1) |> take 3 |> toList == [2, 3, 4]

# # Now random numbers!

# xorAndShiftLeft = \state, bits -> Num.bitwiseXor state (Num.shiftLeftBy state bits)
# xorAndShiftRight = \state, bits -> Num.bitwiseXor state (Num.shiftRightBy state bits)

# # xorshift64Star : U64 -> Just { state : U64, value : U32 }
# xorshift64Star = \state ->
#     newState = state |> xorAndShiftRight 12 |> xorAndShiftLeft 25 |> xorAndShiftRight 27 |> Num.bitwiseXor 0x2545F4914F6CDD1Du64
#     Just { state: newState, value: Num.shiftRightBy newState 32 |> Num.intCast }

# seedXorshift64Star : U32 -> U64
# seedXorshift64Star = \seed -> (Num.intCast seed |> Num.shiftLeftBy 32 |> Num.bitwiseOr 1)

