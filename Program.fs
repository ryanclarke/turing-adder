module EndlessStack
    open System

    let I = 1
    let O = 0
    
    let emptyValue = O
    let emptyStack = []

    let New = emptyStack

    let Push x stack = x::stack
    let Pop stack =
        match stack with
        | top::rest -> (top, rest)
        | [] -> (emptyValue, emptyStack)

    let AppRun args =
        args

    let Explode (s:string) =
        [for c in s -> c.ToString() |> Int32.Parse]

    let Test input test =
        input
        |> Explode
        |> AppRun
        |> test
        |> printfn "%b"

    let TestRun args =
        Test "" (List.isEmpty)
        Test "0" (fun x -> x |> List.length = 1)
        Test "1" (fun x -> x |> List.length = 1)
        Test "0" (fun x -> Pop x = (O, emptyStack))
        Test "0101" (fun x -> Pop x = (O, [I;O;I]))

    [<EntryPoint>]
    let main argv =
        argv |> TestRun
        0 // return an integer exit code
