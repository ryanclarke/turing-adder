module Stack
    let Default = 0
    let New = []

    let Push x stack = x::stack
    let Pop stack =
        match stack with
        | top::rest -> (top, rest)
        | [] -> (Default, New)
