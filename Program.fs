open Test

[<EntryPoint>]
let main argv =
    argv |> RunTest
    0 // return an integer exit code
