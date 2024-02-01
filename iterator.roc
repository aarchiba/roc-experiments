app "iterator"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br",
    }
    imports [pf.Stdout, pf.Utc, pf.Task.{ Task }]
    provides [main] to pf


# Let's sort out iterators first

next = \iterator ->
    when iterator.nextState iterator.state is
        Just { state, value } ->
            Just { iterator: {nextState: iterator.nextState, state}, value }

        Nothing ->
            Nothing

# An iterator needs a state and a function that takes the state and returns a value and a new state
toList = \iterator ->
    toListInternal = \list, iter ->
        when next iter is
            Just { iterator: forward, value } ->
                List.append list value |> toListInternal forward

            Nothing ->
                list
    toListInternal [] iterator

makeIterator = \stateTransformer ->
    \state ->
        { nextState: stateTransformer, state: state }

countDown = makeIterator (\n ->
    when n is
        0 ->            Nothing
        _ -> Just { state: (n - 1), value: n }
)

countUp = makeIterator (\n ->
    Just { state: (n + 1), value: n }
)

take = \iterator, n ->
    (makeIterator (\state ->
        if state.n == 0 then 
            Nothing
        else
            when next state.iterator is
                Just { iterator: nextIterator, value } ->
                    Just { state:{n: (state.n-1), iterator: nextIterator}, value }

                Nothing ->
                    Nothing
    ))
    { n, iterator }

expect countDown 10 |> toList == [10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
expect countDown 10 |> take 3 |> toList == [10, 9, 8]
expect countUp 1 |> take 4 |> toList == [1, 2, 3, 4]

# Now random numbers!

xorAndShiftLeft = \state, bits -> Num.bitwiseXor state (Num.shiftLeftBy state bits)
xorAndShiftRight = \state, bits -> Num.bitwiseXor state (Num.shiftRightBy state bits)

# xorshift64Star : U64 -> Just { state : U64, value : U32 }
xorshift64Star = \state ->
    newState = state |> xorAndShiftRight 12 |> xorAndShiftLeft 25 |> xorAndShiftRight 27 |> Num.bitwiseXor 0x2545F4914F6CDD1Du64
    Just { state: newState, value: Num.shiftRightBy newState 32 |> Num.intCast }

seedXorshift64Star : U32 -> U64
seedXorshift64Star = \seed -> (Num.intCast seed |> Num.shiftLeftBy 32 |> Num.bitwiseOr 1)

random = \seed ->
    seedXorshift64Star seed |> (makeIterator xorshift64Star)

map = \iterator, f ->
    (makeIterator (\state ->
        when next state.iterator is
            Just { iterator: nextIterator, value } ->
                Just { state: {iterator: nextIterator}, value: f value }

            Nothing ->
                Nothing
    ))
    { iterator }

# filter = \iterator, f ->
#     (makeIterator (\state ->
#         when next state.iterator is
#             Just { iterator: nextIterator, value } ->
#                 if f value then
#                     Just { state: {iterator: nextIterator}, value }
#                 else
#                     # hmm, need to walk forward until we get a value
#                     filter nextIterator f

#             Nothing ->
#                 Nothing
#     ))
#     { iterator }

# Can we make a sort of "yield" syntax work? With yield as something like a Task?

expect random 0 |> take 10 |> toList |> List.len == 10
expect countUp 1 |> map (\n -> n * 2) |> take 4 |> toList == [2, 4, 6, 8]
# expect countUp 1 |> filter (\n -> n % 2 == 0) |> take 4 |> toList == [2, 4, 6, 8]

main =
    startTime <- Utc.now |> Task.await

    l = random 0 |> take 10 |> toList |> List.map Num.toStr |> Str.joinWith ", "
    {} <- Stdout.line "random: $(l)" |> Task.await

    endTime <- Utc.now |> Task.await

    runTime = Utc.deltaAsMillis startTime endTime |> Num.toStr
    {} <- Stdout.line "Run time: $(runTime) ms" |> Task.await

    # Final task doesn't need to be awaited
    Stdout.line "Done"
