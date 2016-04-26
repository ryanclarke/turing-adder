module Machine
    open Tape
    open System
    
    [<StructuredFormatDisplay("({Sate}, {Next}, {Value}, {Write}, {Move})")>]
    type Rule = {
        State: string;
        Next: string;
        Value: int;
        Write: int;
        Move: Tape -> Tape}

    let CreateRule rule =
        let state, next, value, write, move = rule
        {State=state;
            Next=next;
            Value=value;
            Write=write;
            Move=move}

    let CreateRules rules : Rule list =
        List.map CreateRule rules

    let RunMachine display acceptStates rules state tape =
        let isAccepted (state:string) =
            acceptStates
            |> List.contains state
        let findRules state tape =
            rules
            |> List.tryFind (fun x -> x.Value = tape.H && x.State = state)
        let rec execute state tape =
            match state |> isAccepted with
            | true -> (true, tape)
            | false ->
                match findRules state tape with
                | None -> (false, tape)
                | Some rule ->
                    display state tape (tape.H = rule.Write)
                    execute rule.Next (rule.Move {tape with H=rule.Write})
        execute state tape

    let Run = RunMachine (fun x y z -> ())
    let RunVerbose = RunMachine PrintTape
