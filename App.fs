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
        let hex (i:int) = System.Convert.ToString(i, 16)
        let bits = (data.L.Length + 1) / 3
        [0..bits] |> List.map (fun bit ->
            RulesetAdder1Bit () |> List.map (fun rule ->
                match rule.State.Substring(0, 1) with
                | "c" ->
                    match bit = bits && rule.Next = "a0" with
                    | false ->
                        {rule with
                            State=(sprintf "%s%s" rule.State (hex bit));
                            Next=(sprintf "%s%s" rule.Next (hex (bit + 1)))}
                    | true ->
                        {rule with
                            State=(sprintf "%s%s" rule.State (hex bit));
                            Next="done"}
                | x ->
                    {rule with
                        State=(sprintf "%s%s" rule.State (hex bit));
                        Next=(sprintf "%s%s" rule.Next (hex bit))}))
        |> List.concat

    let Explode (s:string) =
        [for c in s -> c.ToString() |> int]

    let ToTape s = Explode s |> Tape.New

    let CreateTape (a:string) (b:string) =
        let doit acc x y = sprintf "%s0%c%c" acc x y
        let toList (s:string) = s.ToCharArray() |> Array.toList
        List.fold2 doit "" (toList a) (toList b)
        |> ToTape

    let Run (a:string) (b:string) =
        let length = max (a.Length) (b.Length)
        let eqized (s:string) = s.PadLeft(length, '0')
        let tape = CreateTape (eqized a) (eqized b)
        let rules = CreateNBitAdderRuleset tape
        let (success, finalTape) = RunVerbose ["done"] rules "a00" {tape with L=(List.append tape.L [0;0;0])}
        let t = finalTape.R |> List.chunkBySize 3

        let extract i = t |> List.map (fun x -> (x.Item i).ToString())
        let bin is = is |> String.concat ""
        let dec is = System.Convert.ToInt64(bin is, 2).ToString()
        let first = extract 2
        let second = extract 1
        let answer = extract 0
        printfn "end:  %s\n" (finalTape.R |> List.map (fun x -> sprintf "%d" x) |> String.concat "  ")
        printfn "1st:        %s" (first |> String.concat "        ")
        printfn "2nd:     %s" (second |> String.concat "        ")
        printfn "ans:  %s" (answer |> String.concat "        ")
        printfn "\nbin:  %s + %s = %s" (bin first) (bin second) (bin answer)
        printfn "dec:  %s + %s = %s" (dec first) (dec second) (dec answer)
        printfn "\nSuccess: %b" success

    let RunDecimal a b =
        let toBinaryString (x:string) = System.Convert.ToString(int x, 2)
        Run (toBinaryString a) (toBinaryString b)
        
