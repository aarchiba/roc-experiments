interface MinimalBug2 exposes [
        Iterable,
        toListAppend,
    ] imports []

Step a s : [Yield a s, Skip s, Stop]

Iterable implements
    next : s -> (Step a s) where s implements Iterable

# Let's sort out iterators first

toListAppend = \iterable, list ->
    when next iterable is
        Yield value nextIterable ->
            toListAppend nextIterable (List.append list value)
        Skip nextIterable ->
            toListAppend nextIterable list
        Stop ->
            list


Return a := [Value a, Done] implements [Iterable { next : returnNext }]
returnNext = \@Return r-> when r is
    Value value -> Yield value (@Return Done)
    Done -> Stop
return = \value -> @Return (Value value)

expect return Thing |> toListAppend [] == [Thing]
expect return "a" |> toListAppend ["b"] == ["b", "a"]
expect return Thing |> toListAppend [OtherThing] == [OtherThing, Thing]

