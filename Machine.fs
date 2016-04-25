module Machine
    open Tape
    open System

    type Rule = {
        State: string;
        Next: string;
        Value: int;
        Write: int;
        Move: Tape -> Tape}

    let CreateRule rule =
        let state, next, value, write, move = rule
        {    State=state;
            Next=next;
            Value=value;
            Write=write;
            Move=move}

    let CreateRules rules : Rule list =
        List.map CreateRule rules

    let rec Run acceptStates rules state tape =
        match acceptStates |> List.contains state with
        | true -> (true, tape)
        | false ->
            let rs = rules |> List.tryFind (fun x -> x.Value = tape.H && x.State = state)
            match rs with
            | None -> (false, tape)
            | Some rule ->
                Run acceptStates rules rule.Next (rule.Move {tape with H=rule.Write})

