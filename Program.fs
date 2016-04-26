[<EntryPoint>]
let main argv =
    let args = Array.toList argv
    match args with
    | [] ->
        Test.RunTests args
        0
    | ["b"; a; b] ->
        App.Run b a
        0
    | ["d"; a; b] ->
        App.RunDecimal b a
        0
    | x -> 1
