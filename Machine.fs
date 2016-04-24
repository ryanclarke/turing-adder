module Machine
    open Tape
    open System

    type Direction =
        | Left
        | Right

    type Rule = {
        State: Guid;
        Next: Guid;
        Value: int;
        Write: int;
        Move: Tape -> Tape}

    let DefaultRule = {
        State=Guid.NewGuid();
        Next=Guid.NewGuid();
        Value=0;
        Write=0;
        Move=GoLeft}

    let NewRule value write move =
        {DefaultRule with
            Value=value;
            Write=write;
            Move=move}

    let rec Run acceptStates (rules:Rule list) state (tape:Tape) =
        match acceptStates |> List.contains state with
        | true -> (true, tape)
        | false ->
            let rs = rules |> List.tryFind (fun x -> x.Value = tape.H && x.State = state)
            match rs with
            | None -> (false, tape)
            | Some rule ->
                Run acceptStates rules rule.Next (rule.Move {tape with H=rule.Write})

