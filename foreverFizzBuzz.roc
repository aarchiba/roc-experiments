app "foreverFizzBuzz"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.7.1/Icc3xJoIixF3hCcfXrDwLCu4wQHtNdPyoJkEbkgIElA.tar.br",
    }
    imports [pf.Stdout, pf.Sleep, pf.Task.{ Task }]
    provides [main] to pf

applyFizzBuzz : U64 -> Str
applyFizzBuzz = \n ->
    when (Num.rem n 3, Num.rem n 5) is
        (0, 0) -> "FizzBuzz"
        (0, _) -> "Fizz"
        (_, 0) -> "Buzz"
        _ -> Num.toStr n

runFizzBuzz : U64 -> Task.Task {} d
runFizzBuzz = \n ->
    {} <- Stdout.line (applyFizzBuzz n) |> Task.await
    {} <- Sleep.millis 1000 |> Task.await
    runFizzBuzz (n + 1)

main =
    runFizzBuzz 1
