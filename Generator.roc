interface Generator exposes [
        Yield2,
        generator,
        # yieldFrom,
    ] imports [Stream]

# ## Generator syntax for making streams
#
# The idea here is that it might be easier to make streams if we could
# do it by writing generator functions: functions that use a "yield"
# keyword to produce a value for the stream and then continue if
# another value is needed.
#
# There are three parts to this:
#
# Yield x - a Task-like type that is used to yield a value
# generator - a function that wraps up a function that produces YieldValues
#         into a Stream
# yieldFrom - a function that takes a Stream and yields all the values in it
#
# These are not necessarily well named.

# A Yield captures a value and a continuation
# A function that wants to be inside a generator should
# return a Yield for the first value to be yielded
# and then a continuation that will be called with {} to
# get the next value to be yielded, or stop.
Yield2 valType : [Yield2 valType ({} -> Yield2 valType), Stop]

# Convert a Yield into a Stream
generator : Yield2 a -> Stream.Stream a (Yield2 a)
generator = \g ->
    loop = \yv ->
        when yv is
            Yield2 val f -> Yield val (f {})
            Stop -> Stop
    Stream loop g

# yieldFrom should take a Stream and produce a YieldValue
# for each element in the Stream.
# having some trouble with this one
# yieldFrom : Stream.Stream valType stateType -> Yield2 valType
# yieldFrom = \Stream next state ->
#     inner : valType, stateType -> Yield2 valType
#     inner = \val2, nextState2 ->
#         {} <- Yield2 val2
#         yieldFrom (Stream next nextState2)
#     nextStateResult : Stream.Step valType stateType
#     nextStateResult = next state
#     when nextStateResult is
#         Yield val nextState -> inner val nextState
#         Skip nextState -> yieldFrom (Stream next nextState)
#         Stop -> Stop

