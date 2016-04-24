module Stack
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
