interface ListFusible exposes [get, set, update, append, appendIfOk, prepend] imports [Stream]

# isEmpty not needed

# get is worth fusing as it can save computing
# the rest of the list
get = \list, index ->
    Stream.stream list |>
    Stream.drop index |>
    Stream.head

# replace

set = \list, index, value ->
    Stream.stream list |>
    Stream.enumerate |>
    Stream.map (\(i, x) -> if i == index then value else x) |>
    Stream.unstream

update = \list, index, f ->
    Stream.stream list |>
    Stream.enumerate |>
    Stream.map (\(i, x) -> if i == index then f x else x) |>
    Stream.unstream

append = \list, value ->
    Stream.stream list |>
    Stream.append (Stream.return value) |>
    Stream.unstream

# unclear whether we can get away with not streaming in one case
appendIfOk = \list, result ->
    when result is
        Ok value ->
            Stream.stream list |>
            Stream.append (Stream.return value) |>
            Stream.unstream
        Err _ ->
            list

prepend = \list, value ->
    Stream.stream list |>
    (\x -> Stream.append (Stream.return value) x) |>
    Stream.unstream

prependIfOk = \list, result ->
    when result is
        Ok value ->
            Stream.stream list |>
            (\x -> Stream.append (Stream.return value) x) |>
            Stream.unstream
        Err _ ->
            list

# len

# withCapacity

# reserve

# ...

# reverse can use streamReversed

# ...

# walk is foldl

# ...

# joinMap is concatMap

# ...

# sublist could use streamSublist or an indexing operator

# ...

# split is interesting, can one avoid walking the first part of the stream twice? make before a list?




### additional functions
