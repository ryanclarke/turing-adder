module Test
    open Stack
    open Tape
    open Machine
    open App

    let TestRunner setup display input f test desc =
        let out = setup input |> f
        match out = test with
        | true -> printfn "<T> - %s" (sprintf "%A -> %s -> %A" input desc (display test))
        | false -> printfn "<F>>> %s" (sprintf "%A -> %s -> %A <<< %A >>>" input desc (display out) (display test))

    let StackTest input f test desc =
        TestRunner Explode id input f test desc

    let TapeTest input f test desc =
        TestRunner AppRun Display input f test desc

    let RunStackTests args =
        printfn "\n%s" "Stack Tests"
        StackTest "" id [] "id"
        StackTest "0" id [0] "id"
        StackTest "1" id [1] "id"
        StackTest "1001001" id [I;O;O;I;O;O;I] "id"
        StackTest "0" Pop (O,[]) "Pop"
        StackTest "0101" Pop (O,[I;O;I]) "Pop"
        StackTest "101" (Push I) [I;I;0;I] "Push 1"

    let RunTapeTests args =
        printfn "\n%s" "Tape Tests"
        TapeTest "" id {L=[]; H=0; R=[]} "id"
        TapeTest "" GoLeft {L=[]; H=0; R=[0]} "GoLeft"
        TapeTest "" GoRight {L=[0]; H=0; R=[]} "GoRight"
        TapeTest "1001" id {L=[0;0;1]; H=1; R=[]} "id"
        TapeTest "1001" (GoLeft) {L=[0;1]; H=0; R=[1]} "GoLeft"
        TapeTest "1001" (GoLeft >> GoLeft) {L=[1]; H=0; R=[0;1]} "GoLeft x2"
        TapeTest "1001" (GoRight) {L=[1;0;0;1]; H=0; R=[]} "GoRight"
        TapeTest "1001" (GoRight >> GoRight) {L=[0;1;0;0;1]; H=0; R=[]} "GoRight x2"

    let MachineTestRunner rules data state accept =
        let start = AppRun data
        let (success, finish) = Machine.Run accept rules state start
        printfn "<%A> - %A -> %A" success (Display start) (Display finish)

    let RunMachineTests args =
        printfn "\n%s" "Machine Tests"
        MachineTestRunner [CreateRule (Write, 1, "a", "b")] "1010" "a" ["b"]
        MachineTestRunner (CreateRules [(Read, 0, "a", "a");(Read, 1, "a", "b")]) "01100" "a" ["b"]

    let RunTest args =
        RunStackTests args
        RunTapeTests args
        RunMachineTests args
