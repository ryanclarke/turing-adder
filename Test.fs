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

    let MachineTestRunner rules data state accept answer=
        let start = AppRun data
        let (success, finish) = Machine.Run accept rules state start
        let pass = Display finish = answer
        match pass with
        | true -> printfn "<T> - %A -> %A" (Display start) (Display finish)
        | false -> printfn "<F>>> %A -> %A <<< %A >>>" (Display start) answer (Display finish)

    let RunMachineTests args =
        printfn "\n%s" "Machine Tests"
        MachineTestRunner [CreateRule ("a", "b", 0, 1, id)] "1010" "a" ["b"] "101(1)"
        MachineTestRunner (CreateRules [("a", "a", 0, 0, GoLeft);("a", "b", 1, 1, id)]) "1100" "a" ["b"] "1(1)00"

    let AdderTestRunner data =
        MachineTestRunner (CreateNBitAdderRuleset data) data

    let RunAdder1BitTests args =
        printfn "\n%s" "Adder (1Bit) Tests"
        AdderTestRunner "000" "a00" ["done"] "(0)000000"
        AdderTestRunner "001" "a00" ["done"] "(0)000101"
        AdderTestRunner "010" "a00" ["done"] "(0)000110"
        AdderTestRunner "011" "a00" ["done"] "(0)100011"

    let RunAdder2BitTests args =
        printfn "\n%s" "Adder (nBit) Tests"
        AdderTestRunner "000000" "a00" ["done"] "(0)000000000"
        AdderTestRunner "000001" "a00" ["done"] "(0)000000101"
        AdderTestRunner "000010" "a00" ["done"] "(0)000000110"
        AdderTestRunner "000011" "a00" ["done"] "(0)000100011"
        AdderTestRunner "001000" "a00" ["done"] "(0)000101000"
        AdderTestRunner "001001" "a00" ["done"] "(0)000101101"
        AdderTestRunner "001010" "a00" ["done"] "(0)000101110"
        AdderTestRunner "001011" "a00" ["done"] "(0)100001011"
        AdderTestRunner "010000" "a00" ["done"] "(0)000110000"
        AdderTestRunner "010001" "a00" ["done"] "(0)000110101"
        AdderTestRunner "010010" "a00" ["done"] "(0)000110110"
        AdderTestRunner "010011" "a00" ["done"] "(0)100010011"
        AdderTestRunner "011000" "a00" ["done"] "(0)100011000"
        AdderTestRunner "011001" "a00" ["done"] "(0)100011101"
        AdderTestRunner "011010" "a00" ["done"] "(0)100011110"
        AdderTestRunner "011011" "a00" ["done"] "(0)100111011"

    let RunTest args =
        RunStackTests args
        RunTapeTests args
        RunMachineTests args
        RunAdder1BitTests args
        RunAdder2BitTests args
