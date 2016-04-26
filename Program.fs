[<EntryPoint>]
let main argv =
    let args = Array.toList argv
    match args with
    | [] ->
        Test.RunTests args
        0
    | [a; b] ->
        App.Run a b
        0
    | x -> 1
