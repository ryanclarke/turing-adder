module App
    open Tape

    let Explode (s:string) =
        [for c in s -> c.ToString() |> System.Int32.Parse]

    let AppRun args =
        args
        |> Explode
        |> Tape.New

