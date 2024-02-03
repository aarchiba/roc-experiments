interface MinimalBug3 exposes [
        Iterable,
    ] imports []

Step a s : [Yield a s, Skip s, Stop]

Iterable implements
    next : s -> (Step a s) where s implements Iterable


# Let's sort out iterators first


toListAppend = \iterable, list ->
    when next iterable is
        Yield value state ->
            toListAppend state (List.append list value)
        Skip state ->
            toListAppend state list
        Stop ->
            list

Return a := [Value a, Done] implements [Iterable { next : returnNext }]
returnNext = \@Return r-> when r is
    Value value -> Yield value (@Return Done)
    Done -> Stop
return = \value -> @Return (Value value)

Take a := { n : U64, iterable : a } where a implements Iterable implements [Iterable { next : takeNext }] 
takeNext : Take b -> (Step a (Take b))
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

# This crashes the compiler
# expect return "a" |> toListAppend [] == []
expect return "a" |> take 1 |> toListAppend [] == []

