module Machine
    open Tape
    open System

    type Direction =
        | Left
        | Right

    type RuleType =
        | Read
        | Write

    type Rule = {
        State: string;
        Next: string;
        Value: int;
        Write: int;
        Move: Tape -> Tape}

    let DefaultRule = {
        State=(Guid.NewGuid().ToString());
        Next=(Guid.NewGuid().ToString());
        Value=0;
        Write=0;
        Move=GoLeft}

    let NewRule value write move =
        {DefaultRule with
            Value=value;
            Write=write;
            Move=move}

    let CreateRule rule =
        let action, value, state, next = rule
        let basic = {
            State=state;
            Next=next;
            Value=value;
            Write=value;
            Move=GoLeft}
        match action with
        | Read -> basic
        | Write -> {basic with Value=0}

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

