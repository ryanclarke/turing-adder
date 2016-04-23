module Tape
    open Stack

    [<StructuredFormatDisplay("{L}'{H}'{R}")>]
    type Tape = {
        L: int list;
        H: int;
        R: int list}

    let Empty = {L=Stack.New; H=Stack.emptyValue; R=Stack.New}

    let New input =
        let (h, l) = input |> List.rev |> LPop
        {L=l; H=h; R=Stack.New}

    let GoLeft tape : Tape =
        let (h, l) = LPop tape.L
        let r = RPush tape.H tape.R
        {L=l; H=h; R=r}

    let GoRight tape : Tape =
        let (h, r) = RPop tape.R
        let l = LPush tape.H tape.L
        {L=l; H=h; R=r}

