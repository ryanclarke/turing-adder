module App
    open Stack
    open Tape

    let Explode (s:string) =
        [for c in s -> c.ToString() |> System.Int32.Parse]

    let AppRun args =
        args
        |> Explode
        |> Tape.New

    let TestRunner setup input f test desc =
        let out = setup input |> f
        match out = test with
        | true -> printfn "<T> - %s" (sprintf "%A -> %s -> %A" input desc test)
        | false -> printfn "<F>>> %s" (sprintf "%A -> %s -> %A <<< %A >>>" input desc out test)

    let StackTest input f test desc =
        TestRunner Explode input f test desc

    let MachineTest input f test desc =
        TestRunner AppRun input f test desc

    let RunStackTests args =
        printfn "\n%s" "Stack Tests"
        StackTest "" id [] "id"
        StackTest "0" id [0] "id"
        StackTest "1" id [1] "id"
        StackTest "1001001" id [I;O;O;I;O;O;I] "id"
        StackTest "0" RPop (O,[]) "RPop"
        StackTest "0101" RPop (O,[I;O;I]) "RPop"
        StackTest "101" (RPush I) [I;I;O;I] "RPush 1"
        StackTest "0" LPop (O,[]) "LPop"
        StackTest "0101" LPop (I,[O;I;O]) "LPop"
        StackTest "101" (LPush I) [I;O;I;I] "LPush 1"

    let RunMachineTests args =
        printfn "\n%s" "Machine Tests"
        MachineTest "" id {L=[]; H=0; R=[]} "id"
        MachineTest "" GoLeft {L=[]; H=0; R=[0]} "GoLeft"
        MachineTest "" GoRight {L=[0]; H=0; R=[]} "GoRight"
        MachineTest "1001" id {L=[1;0;0]; H=1; R=[]} "id"
        MachineTest "1001" (GoLeft) {L=[1;0]; H=0; R=[1]} "GoLeft"
        MachineTest "1001" (GoLeft >> GoLeft) {L=[1]; H=0; R=[0;1]} "GoLeft x2"
        MachineTest "1001" (GoRight) {L=[1;0;0;1]; H=0; R=[]} "GoRight"
        MachineTest "1001" (GoRight >> GoRight) {L=[1;0;0;1;0]; H=0; R=[]} "GoRight x2"

    let RunTest args =
        RunStackTests args
        RunMachineTests args
