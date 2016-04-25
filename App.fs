module App
    open Tape
    open Machine

    let RulesetAdder1Bit () =
        [
            ("a0", "b0", 0, 0, GoLeft);
            ("a0", "b1", 1, 1, GoLeft);
            ("a1", "b1", 0, 0, GoLeft);
            ("a1", "b2", 1, 1, GoLeft);
            ("b0", "c0", 0, 0, GoLeft);
            ("b0", "c1", 1, 1, GoLeft);
            ("b1", "c1", 0, 0, GoLeft);
            ("b1", "c2", 1, 1, GoLeft);
            ("b2", "c2", 0, 0, GoLeft);
            ("b2", "c3", 1, 1, GoLeft);
            ("c0", "a0", 0, 0, GoLeft);
            ("c1", "a0", 0, 1, GoLeft);
            ("c2", "a1", 0, 0, GoLeft);
            ("c3", "a1", 0, 1, GoLeft);
        ]
        |> CreateRules

    let CreateNBitAdderRuleset (data:string) =
        let bits = data.Length / 3
        [0..bits] |> List.map (fun bit ->
            RulesetAdder1Bit () |> List.map (fun rule ->
                match rule.State.Substring(0, 1) with
                | "c" ->
                    match bit = bits && rule.Next = "a0" with
                    | false ->
                        {rule with
                            State=(sprintf "%s%d" rule.State bit);
                            Next=(sprintf "%s%d" rule.Next (bit + 1))}
                    | true ->
                        {rule with
                            State=(sprintf "%s%d" rule.State bit);
                            Next="done"}
                | x ->
                    {rule with
                        State=(sprintf "%s%d" rule.State bit);
                        Next=(sprintf "%s%d" rule.Next bit)}))
        |> List.concat

    let Explode (s:string) =
        [for c in s -> c.ToString() |> System.Int32.Parse]

    let AppRun args =
        args
        |> Explode
        |> Tape.New

