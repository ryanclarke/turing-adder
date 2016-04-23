module App
    open Stack

    let Explode (s:string) =
        [for c in s -> c.ToString() |> System.Int32.Parse]

    let AppRun args =
        Explode args

    let Test input f test desc =
        input
        |> AppRun
        |> f
        |> fun x -> x = test
        |> fun x -> printfn "%b- %s" x (sprintf "%A -> %s -> %A" input desc test)

    let TestRun args =
        Test "" id [] "id"
        Test "0" id [0] "id"
        Test "1" id [1] "id"
        Test "1001001" id [I;O;O;I;O;O;I] "id"
        Test "0" Pop (O, []) "Pop"
        Test "0101" Pop (O,[I;O;I]) "Pop"
        Test "101" (Push I) [I;I;O;I] "Push 1"
