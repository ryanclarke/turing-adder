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

    let CreateNBitAdderRuleset (data:Tape) =
        let bits = (data.L.Length + 1) / 3
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

    let ToTape s = Explode s |> Tape.New

    let CreateTape (a:string) (b:string) =
        let doit acc x y = sprintf "%s0%c%c" acc x y
        let toList (s:string) = s.ToCharArray() |> Array.toList
        List.fold2 doit "" (toList a) (toList b)
        |> ToTape

    let Run a b =
        let tape = CreateTape a b
        let rules = CreateNBitAdderRuleset tape
        let (success, finalTape) = RunVerbose ["done"] rules "a00" {tape with L=(List.append tape.L [0;0;0])}
        let t = finalTape.R |> List.chunkBySize 3
        printfn "1st:        %s" (t |> List.map (fun x -> (x.Item 2).ToString()) |> String.concat "        ")
        printfn "2nd:     %s" (t |> List.map (fun x -> (x.Item 1).ToString()) |> String.concat "        ")
        printfn "ans:  %s" (t |> List.map (fun x -> (x.Item 0).ToString()) |> String.concat "        ")
        printfn "Success: %b" success
