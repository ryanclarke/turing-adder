module Tape
    open Stack

    [<StructuredFormatDisplay("{L}'{H}'{R}")>]
    type Tape = {
        L: int list;
        H: int;
        R: int list}

    let Empty = {L=Stack.New; H=Stack.emptyValue; R=Stack.New}

    let Display tape =
        let stringify(l:int list) =
            List.map (fun x -> sprintf "%A" x) l
            |> String.concat ""
        let left = tape.L |> List.rev |> stringify
        let head = tape.H
        let right = tape.R |> stringify
        sprintf "%s(%d)%s" left head right

    let RunDisplay tape isRead =
        let stringify(l:int list) =
            List.map (fun x -> sprintf "%A" x) l
            |> String.concat "  "
        let left = tape.L |> List.rev |> stringify
        let head = if isRead then "(" + tape.H.ToString() + ")" else "(-)"
        let right = tape.R |> stringify
        let leftPad = if left.Length > 0 then " " else ""
        let rightPad = if right.Length > 0 then " " else ""
        sprintf "%s%s%s%s %s%s" leftPad left leftPad head right rightPad

    let PrintTape state tape isRead=
        printfn "%s: %s" state (RunDisplay tape isRead)

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

