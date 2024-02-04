interface Stream exposes [
        # Basic types and tools
        Stream,
        Step,
        fromList,
        fromListReversed,
        toList,
        toListAppend,
        empty,
        # Just like List
        isEmpty,
        append,
        prepend,
        concat,
        single,
        repeat,
        join,
        contains,
        walk,
        walkWithIndex,
        sum,
        product,
        any,
        all,
        keepIf,
        dropIf,
        countIf,
        keepOks,
        keepErrs,
        map,
        first,
        takeFirst,
        dropFirst,
        min,
        max,
        joinMap,
        split,
        chunksOf,
        # Not in List but useful
        splitList,
        firstRest,
        enumFromTo,
        closureStream,
        either,
        dropRepeats,
        dropRepeatsComparison,
        mapMaybe,
        readFromList,
        writeToList,
        mergeSorted,
        # Dictionary-related operations
        fromDictKeys,
        fromDictValues,
        fromDictItems,
        toDict,
        readFromDict,
        readFromDictWithKeys,
        writeToDict,
    ] imports []

# ## Functions from Coutts et al.

# ### list conversion to/from
# It would be great if fromList then toList could be elided

## Type for representing a stream of values.
##
## At a low level, values are produced by a step function.
## In practice, the hope is that one can normally manipulate streams
## using the tools in this module.
Stream valType stateType : [Stream (stateType -> Step valType stateType) stateType]

## Instructions for what the stream should do
##
## Typically a step function will take a state and return a new state and a value.
## It may also return a tag to indicate that this is the end of the stream.
## Finally it may also return a tag to indicate that, for internal convenience,
## the state should be updated but that no value should be emitted yet.
Step valType stateType : [Yield valType stateType, Skip stateType, Stop]

ListIterationState a : { list : List a, n : Nat }

## Walk the elements of a list, in order.
fromList : List a -> Stream a (ListIterationState a)
fromList = \ls ->
    listNext : ListIterationState a -> Step a (ListIterationState a)
    listNext = \{ list, n } ->
        when List.get list n is
            Ok val -> Yield val { list, n: n + 1 }
            Err _ -> Stop

    start : ListIterationState a
    start = { list: ls, n: 0nat }
    Stream listNext start

## Walk the elements of a list, in reverse order.
##
## This does not construct a reversed list.
fromListReversed : List a -> Stream a (ListIterationState a)
fromListReversed = \ls ->
    listNext : ListIterationState a -> Step a (ListIterationState a)
    listNext = \{ list, n } ->
        if n == 0nat then
            Stop
        else
            nn = n - 1
            when List.get list nn is
                Ok val -> Yield val { list, n: nn }
                Err _ -> crash "index somehow invalid?"

    start : ListIterationState a
    start = { list: ls, n: List.len ls }
    Stream listNext start

## Convert a stream to a list, appending to an existing list.
##
## This is particularly useful for "toListAppend stream (withCapacity n)".
toListAppend : Stream a s, List a -> List a
toListAppend = \Stream next s, list ->
    loop : List a, s -> List a
    loop = \list1, s1 ->
        when next s1 is
            Yield val s2 -> loop (List.append list1 val) s2
            Skip s2 -> loop list1 s2
            Stop -> list1
    loop list s

## Convert a stream to a list.
toList : Stream a s -> List a
toList = \Stream next s ->
    toListAppend (Stream next s) []

expect ([Red, Fish, Blue, Fish] |> fromList |> toList) == [Red, Fish, Blue, Fish]
expect ([Red, Fish, Blue, Fish] |> fromListReversed |> toList) == [Fish, Blue, Fish, Red]

## The empty stream.
empty = Stream (\{} -> Stop) {}

# ## Replicating selected functions from List

## Test if a stream is empty.
##
## Note that this may require processing many elements of the stream
## to determine if it is empty, and this function does not return the
## processed stream, so other functions may need to repeat the processing.
##
## If you may want to use the stream again, consider using `head` instead.
isEmpty : Stream a s -> Bool
isEmpty = \Stream next s ->
    when next s is
        Yield _ _ -> Bool.false
        Skip s1 -> isEmpty (Stream next s1)
        Stop -> Bool.true

# List.get: doesn't really make sense to implement
# List.replace: doesn't really make sense to implement
# List.set: doesn't really make sense to implement
# List.update: doesn't really make sense to implement

append = \stream, a ->
    concat stream (single a)

# List.appendIfOk: later

prepend = \a, stream ->
    concat (single a) stream

# List.prependIfOk: later
# List.len: definitely don't implement
# List.withCapacity: nope
# List.reserve: nope
# List.releaseExcessCapacity: nope

## Concatenate two streams.
concat = \Stream nextLeft left0, Stream nextRight right0 ->
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

expect ([Red, Fish] |> fromList |> concat ([Blue, Fish] |> fromList) |> toList) == [Red, Fish, Blue, Fish]

# List.last: later

## Create a stream that produces the single provided value and then stops.
single = \a ->
    loop = \state ->
        when state is
            NotEmitted -> Yield a Emitted
            Emitted -> Stop
    Stream loop NotEmitted

## Create a stream that produces the provided value forever.
repeat = \a -> Stream (\{} -> Yield a {}) {}

# List.reverse: doesn't make sense to implement; see fromListReversed

## Flatten a stream of streams into a single stream.
join = \stream ->
    joinMap stream (\x -> x)

## Return whether the stream contains the given value.
contains = \Stream next s, a ->
    loop = \s1 ->
        when next s1 is
            Yield val s2 -> if val == a then Bool.true else loop s2
            Skip s2 -> loop s2
            Stop -> Bool.false
    loop s

## Walk the elements of a stream, accumulating a result.
##
## Also known as "fold" in other languages.
walk = \stream, acc, f ->
    walkUntil stream acc (\acc1, x -> Continue (f acc1 x))

## Walk the elements of a stream and their indices, accumulating a result.
##
## Each element is paired with its index, starting from 0, in a tuple, like
## (index, element).
walkWithIndex = \stream, acc, f ->
    enumerate stream |> walk acc f

# List.walkWithIndexUntil: just use enumerate and walkUntil
# List.walkBackwards: probably not.

## Walk as long as the function says to continue.
##
## The function should return Continue acc or Break acc.
walkUntil = \Stream next s, acc, f ->
    loop = \s1, acc1 ->
        when next s1 is
            Yield val s2 ->
                when f acc1 val is
                    Continue acc2 -> loop s2 acc2
                    Break acc2 -> acc2

            Skip s2 -> loop s2 acc1
            Stop -> acc1
    loop s acc

# List.walkBackwardsUntil: probably not.
# List.walkFrom: just use drop, or fromList with a slice
# List.walkFromUntil: just use drop, or fromList with a slice

sum = \stream ->
    walk stream 0 Num.add

product = \stream ->
    walk stream 1 Num.mul

any = \stream, f ->
    walkUntil stream Bool.false (\_, x -> if f x then Break Bool.true else Continue Bool.false)

all = \stream, f ->
    walkUntil stream Bool.true (\_, x -> if f x then Continue Bool.true else Break Bool.false)

## Keep only values that satisfy the predicate.
keepIf : Stream a s, (a -> Bool) -> Stream a s
keepIf = \stream, f ->
    mapMaybe stream (\x -> if f x then Keep x else Drop)

## Drop values that satisfy the predicate.
dropIf : Stream a s, (a -> Bool) -> Stream a s
dropIf = \stream, f ->
    mapMaybe stream (\x -> if f x then Drop else Keep x)

countIf = \stream, f ->
    walk stream 0 (\x, acc -> if f x then acc + 1 else acc)

keepOks = \stream ->
    m = \x ->
        when x is
            OK val -> Keep val
            Err _ -> Drop
    mapMaybe stream m

keepErrs = \stream ->
    m = \x ->
        when x is
            OK _ -> Drop
            Err val -> Keep val
    mapMaybe stream m

## Return a stream whose values have been passed through a function.
map : Stream a s, (a -> b) -> Stream b s
map = \Stream next s, f ->
    next1 = \s1 ->
        when next s1 is
            Yield val s2 -> Yield (f val) s2
            Skip s2 -> Skip s2
            Stop -> Stop
    Stream next1 s

# List.map2: use zip
# List.map3: use zipList
# List.map4: use zipList
# List.mapWithIndex: soon
# List.range: support steps and never ending
# List.sortWith: not exactly
# List.sortAsc: not exactly
# List.sortDesc: not exactly
# List.swap: no

## Return the first element and discard the rest.
first = \stream ->
    when firstRest stream is
        Ok (val, _) -> Ok val
        Err _ -> Err StreamWasEmpty

## Return a stream that produces the first n values.
takeFirst = \Stream next s0, n0 ->
    next1 = \{ s, n } ->
        if n > 0 then
            when next s is
                Yield val s1 -> Yield val { s: s1, n: n - 1 }
                Skip s1 -> Skip { s: s1, n }
                Stop -> Stop
        else
            Stop
    Stream next1 { s: s0, n: n0 }

# List.takeLast: maybe
# queue would be nice

## Drop the first n values.
##
## If the stream has fewer than n values, the result is an empty stream.
dropFirst = \Stream next s0, n0 ->
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

# List.dropLast: maybe
# List.dropAt: maybe

min = \stream ->
    when firstRest stream is
        Ok (val, rest) ->
            Ok walk rest val (\acc, x -> if x < acc then x else acc)

        Err _ -> Err StreamWasEmpty

max = \stream ->
    when firstRest stream is
        Ok (val, rest) ->
            Ok walk rest val (\acc, x -> if x > acc then x else acc)

        Err _ -> Err StreamWasEmpty

## Apply a function to each element of a stream and concatenate the results.
joinMap = \Stream next s0, f ->
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

expect joinMap ([Red, Fish] |> fromList) (\a -> [a, a] |> fromList) |> toList == [Red, Red, Fish, Fish]

# List.findFirst: soon
# List.findLast: soon
# List.findFirstIndex: probably not
# List.findLastIndex: probably not
# List.sublist: drop and take
# List.intersperse: joinMap and zip
# List.startsWith: soon, should return the rest of the stream
# List.endsWith: maybe

## Split off the first n elements into another stream.
##
## The first n elements must be stored in a list, internally.
## Consider splitList to access this first list directly,
## for example for use in destructuring.
split = \stream, n ->
    when splitList stream n is
        { before, after } -> { before: fromList before, after }

# List.splitFirst: soon
# List.splitLast: might as well use lists

## Batch a stream into chunks of size n.
##
## The last chunk, if any, may have fewer than n elements,
## but will not be empty.
chunksOf = \Stream next s, n ->
    loop = \{ acc, si } ->
        if List.len acc == n then
            Yield acc { acc: [], si }
        else
            when next si is
                Yield val si1 -> Skip { acc: List.append acc val, si: si1 }
                Skip si1 -> Skip { acc, si: si1 }
                Stop -> if List.isEmpty acc then Stop else Yield acc { acc: [], si }
    Stream loop { acc: [], si: s }

# List.mapTry: needs to accumulate everything until success/failure is known
# List.walkTry: ditto

# ## Not in List but useful

## Extract the first element of a stream and the rest.
firstRest : Stream a s -> Result (a, Stream a s) [StreamWasEmpty]
firstRest = \Stream next s ->
    when next s is
        Yield val s1 -> Ok (val, Stream next s1)
        Skip s1 -> firstRest (Stream next s1)
        Stop -> Err StreamWasEmpty

## Split off the first n elements of a stream into a list.
##
## If there aren't actually n elements, the list will contain fewer.
## This is possibly useful for destructuring.
splitList : Stream a s, Nat -> { before : List a, after : Stream a s }
splitList = \Stream next s0, n0 ->
    loop = \{ s, n, acc } ->
        if n > 0 then
            when next s is
                Yield val s1 -> loop { s: s1, n: n - 1, acc: List.append acc val }
                Skip s1 -> loop { s: s1, n: n - 1, acc }
                Stop -> { before: acc, after: Stream next s }
        else
            { before: acc, after: Stream next s }
    loop { s: s0, n: n0, acc: List.withCapacity n0 }

enumFromTo = \from, to1 ->
    loop = \n ->
        if n <= to1 then
            Yield n (n + 1)
        else
            Stop
    Stream loop from

expect (enumFromTo 1 5 |> toList) == [1, 2, 3, 4, 5]

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

expect zip ([Red, Fish] |> fromList) ([Blue, Fish] |> fromList) |> toList == [(Red, Blue), (Fish, Fish)]

# ## Other useful functions

## Count by ones, forever.
countFrom = \n ->
    loop = \n1 ->
        Yield n1 (n1 + 1)
    Stream loop n

## Attach an index to each element of a stream.
##
## This uses zip under the hood.
enumerate = \s -> zip (countFrom 0) s

## Wrap a stream's state in callables.
##
## Normally a stream's type includes the type of its
## internal state. This is necessary so that roc can
## know what code to run to manipulate it. Unfortunately,
## this means that you cannot normally do something like
## if condition then stream1 else stream2; the incompatible
## types of the two streams' states will cause a type error.
## This function takes a stream with any internal state
## and returns an otherwise identical state whose type
## is independent of the internal state. The performance
## of this wrapped stream may be less than the original,
## because the wrapping involves creating a closure at
## each step.
closureStream : Stream a s -> Stream a ({} -> Step a s)
closureStream = \Stream next s ->
    loop = \f ->
        when f {} is
            Yield val s1 -> Yield val (\{} -> next s1)
            Skip s1 -> Skip (\{} -> next s1)
            Stop -> Stop
    Stream loop (\{} -> next s)

expect (countFrom 0 |> takeFirst 5 |> toList) == (countFrom 0 |> closureStream |> takeFirst 5 |> toList)

## Select between two streams based on a condition.
##
## This requires wrapping in closureStream.
either = \Stream nextLeft sLeft, Stream nextRight sRight, which ->
    when which is
        Left -> closureStream (Stream nextLeft sLeft)
        Right -> closureStream (Stream nextRight sRight)

## Drop repeated values.
##
## Values are considered equal if the provided function returns true.
## Any value that compares equal to the previous value is dropped.
dropRepeatsComparison : Stream a s, (a, a -> Bool) -> Stream a (s, [NoValue, Some a])
dropRepeatsComparison = \Stream next s, equal ->
    loop = \(s1, last) ->
        when next s1 is
            Yield val s2 ->
                when last is
                    NoValue -> Yield val (s2, Some val)
                    Some lastVal -> if equal lastVal val then Skip (s2, Some val) else Yield val (s2, Some val)

            Skip s2 -> Skip (s2, last)
            Stop -> Stop
    Stream loop (s, NoValue)

## Drop repeated values.
dropRepeats = \Stream next s -> dropRepeatsComparison (Stream next s) (\x, y -> x == y)

expect countFrom 0 |> dropRepeats |> takeFirst 5 |> toList == [0, 1, 2, 3, 4]
expect [Red, Red, Fish, Blue, Blue] |> fromList |> dropRepeats |> toList == [Red, Fish, Blue]

## Read a sequence of values from a list.
##
## Takes a stream of indices and produces a stream of values.
## Invalid indices are skipped.
readFromList = \stream, list ->
    pick = \i ->
        when List.get list i is
            Ok val -> Keep val
            Err _ -> Drop
    mapMaybe stream pick

## Write a sequence of values to a list.
##
## Takes pairs of index, element and sets the corresponding
## element in the list. Invalid indices are skipped.
##
## Combined with readFromList, this can be used to shuffle
## and transform selected elements of a list.
writeToList = \stream, list ->
    walk stream list (\list1, (i, val) -> List.set list1 i val)

## Map over a stream, dropping unwanted values.
##
## The function can return Keep val1 or Drop.
mapMaybe = \Stream next s, f ->
    loop = \s1 ->
        when next s1 is
            Yield val s2 ->
                when f val is
                    Keep val1 -> Yield val1 s2
                    Drop -> Skip s2

            Skip s2 -> Skip s2
            Stop -> Stop
    Stream loop s

MergeSortedState a sLeft sRight : { left : [NoValue, Value a, Done], right : [NoValue, Value a, Done], leftNow : sLeft, rightNow : sRight }
mergeSorted : Stream a sLeft, Stream a sRight, (a, a -> Bool) -> Stream a (MergeSortedState a sLeft sRight)
mergeSorted = \Stream nextLeft left0, Stream nextRight right0, lessThan ->
    loop : MergeSortedState a sLeft sRight -> Step a (MergeSortedState a sLeft sRight)
    loop = \{ left, right, leftNow, rightNow } ->
        when (left, right) is
            (Done, Done) -> Stop
            (NoValue, _) ->
                when nextLeft leftNow is
                    Yield val leftNext -> Skip { left: Value val, leftNow: leftNext, right, rightNow }
                    Skip leftNext -> Skip { left, right, leftNow: leftNext, rightNow }
                    Stop -> Skip { left: Done, right, leftNow, rightNow }

            (_, NoValue) ->
                when nextRight rightNow is
                    Yield val rightNext -> Skip { left, right: Value val, leftNow, rightNow: rightNext }
                    Skip rightNext -> Skip { left, right, leftNow, rightNow: rightNext }
                    Stop -> Skip { left, right: Done, leftNow, rightNow }

            (Value left1, Value right1) ->
                if lessThan left1 right1 then
                    Yield left1 { left: NoValue, right, leftNow, rightNow }
                else
                    Yield right1 { left, right: NoValue, leftNow, rightNow }

            (Value left1, Done) -> Yield left1 { left: NoValue, right, leftNow, rightNow }
            (Done, Value right1) -> Yield right1 { left, right: NoValue, leftNow, rightNow }

    Stream loop { left: NoValue, right: NoValue, leftNow: left0, rightNow: right0 }

expect mergeSorted ([1, 2, 5] |> fromList) ([3, 4, 6] |> fromList) (\x, y -> x < y) |> toList == [1, 2, 3, 4, 5, 6]

# ## Dictionary-related operations

## Walk a dictionary's keys.
##
## Unfortunately this requires creating a list.
fromDictKeys = \dict ->
    fromList (Dict.keys dict)

## Walk a dictionary's values.
##
## Unfortunately this requires creating a list.
fromDictValues = \dict ->
    fromList (Dict.values dict)

## Walk a dictionary's key, value pairs.
##
## Unfortunately this requires creating a list.
fromDictItems = \dict ->
    fromList (Dict.toList dict)

## Create a dict from a stream of key, value pairs.
toDict = \stream ->
    writeToDict stream (Dict.empty {})

## Read a sequence of values from a dict.
readFromDict = \stream, dict ->
    pick = \key ->
        when Dict.get dict key is
            Ok val -> Keep val
            Err _ -> Drop
    mapMaybe stream pick

## Read a sequence of (key, value) pairs from a dict.
readFromDictWithKeys = \stream, dict ->
    pick = \key ->
        when Dict.get dict key is
            Ok val -> Keep (key, val)
            Err _ -> Drop
    mapMaybe stream pick

## Write a sequence of values to a dict.
writeToDict = \stream, dict ->
    walk stream dict (\dict1, (key, val) -> Dict.insert dict1 key val)

