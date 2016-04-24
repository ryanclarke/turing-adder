module Tape
    open Stack

    [<StructuredFormatDisplay("{L}'{H}'{R}")>]
    type Tape = {
        L: int list;
        H: int;
        R: int list}

    let Empty = {L=Stack.New; H=Stack.emptyValue; R=Stack.New}

    let Display tape =
        let left = tape.L |> List.rev |> List.map (fun x -> sprintf "%A" x) |> String.concat ""
        let head = tape.H
        let right = tape.R |> List.map (fun x -> sprintf "%A" x) |> String.concat ""
        sprintf "%s(%d)%s" left head right

    let New input =
        let (h, l) = input|> List.rev |> Pop
        {L=l; H=h; R=Stack.New}

    let GoLeft tape : Tape =
        let (h, l) = Pop tape.L
        let r = Push tape.H tape.R
        {L=l; H=h; R=r}

    let GoRight tape : Tape =
        let (h, r) = Pop tape.R
        let l = Push tape.H tape.L
        {L=l; H=h; R=r}

