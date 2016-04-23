module Stack
    let I = 1
    let O = 0
    
    let emptyValue = O
    let emptyStack = []

    let New = emptyStack

    let RPush x stack = x::stack
    let RPop stack =
        match stack with
        | top::rest -> (top, rest)
        | [] -> (emptyValue, emptyStack)

    let LPush x stack =
        stack
        |> List.rev
        |> RPush x
        |> List.rev

    let LPop stack =
        let head, left =
            match List.rev stack with
            | top::rest -> (top, rest)
            | [] -> (emptyValue, emptyStack)
        (head, List.rev left)
