module App
    open Tape
    open Machine

    let RulsetAdder1Bit () =
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
            ("c0", "a0x", 0, 0, GoLeft);
            ("c1", "a0x", 0, 1, GoLeft);
            ("c2", "a1", 0, 0, GoLeft);
            ("c3", "a1", 0, 1, GoLeft);
        ]
        |> CreateRules

    let Explode (s:string) =
        [for c in s -> c.ToString() |> System.Int32.Parse]

    let AppRun args =
        args
        |> Explode
        |> Tape.New

