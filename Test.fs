module Test
    open Stack
    open Tape
    open Machine
    open App

    let TestResultsDisplayer display input out test desc =
        match out = test with
        | true ->
            printfn "<T> - %s" (sprintf "%A -> %s -> %A" input desc (display test))
        | false ->
            printfn "<F>>> %s" (sprintf "%A -> %s -> %A <<<out: %A >>>" input desc (display test) (display out))

    let TestRunner setup display input f test desc =
        let out = setup input |> f
        TestResultsDisplayer display input out test desc

    let StackTest input f test desc =
        TestRunner Explode id input f test desc

    let TapeTest input f test desc =
        TestRunner ToTape Display input f test desc

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
        let (success, finish) = Machine.Run accept rules state data
        let pass = Display finish = answer
        match pass with
        | true -> printfn "<T> - %A -> %A" (Display data) (Display finish)
        | false -> printfn "<F>>> %A -> %A <<<out: %A >>>" (Display data) answer (Display finish)

    let RunMachineTests args =
        printfn "\n%s" "Machine Tests"
        MachineTestRunner [CreateRule ("a", "b", 0, 1, id)] (ToTape "1010") "a" ["b"] "101(1)"
        MachineTestRunner (CreateRules [("a", "a", 0, 0, GoLeft);("a", "b", 1, 1, id)]) (ToTape "1100") "a" ["b"] "1(1)00"

    let AdderTapeTestRunner data =
        MachineTestRunner (CreateNBitAdderRuleset data) data "a00" ["done"]

    let AdderTestRunner data =
        AdderTapeTestRunner (ToTape data)

    let RunAdder1BitTests args =
        printfn "\n%s" "Adder (1-bit) Tests"
        AdderTestRunner "000" "(0)000000"
        AdderTestRunner "001" "(0)000101"
        AdderTestRunner "010" "(0)000110"
        AdderTestRunner "011" "(0)100011"

    let RunAdder2BitTests args =
        printfn "\n%s" "Adder (2-bit) Tests"
        AdderTestRunner "000000" "(0)000000000"
        AdderTestRunner "000001" "(0)000000101"
        AdderTestRunner "000010" "(0)000000110"
        AdderTestRunner "000011" "(0)000100011"
        AdderTestRunner "001000" "(0)000101000"
        AdderTestRunner "001001" "(0)000101101"
        AdderTestRunner "001010" "(0)000101110"
        AdderTestRunner "001011" "(0)100001011"
        AdderTestRunner "010000" "(0)000110000"
        AdderTestRunner "010001" "(0)000110101"
        AdderTestRunner "010010" "(0)000110110"
        AdderTestRunner "010011" "(0)100010011"
        AdderTestRunner "011000" "(0)100011000"
        AdderTestRunner "011001" "(0)100011101"
        AdderTestRunner "011010" "(0)100011110"
        AdderTestRunner "011011" "(0)100111011"

    let CreateTapeTestRunner a b expected =
        TestResultsDisplayer Display (sprintf "%s + %s" a b) (CreateTape a b) (ToTape expected) ""

    let RunCreateTapeTests args =
        printfn "\n%s" "Create Tape Tests"
        CreateTapeTestRunner "0" "0" "000"
        CreateTapeTestRunner "0" "1" "001"
        CreateTapeTestRunner "1" "0" "010"
        CreateTapeTestRunner "1" "1" "011"
        CreateTapeTestRunner "00" "00" "000000"
        CreateTapeTestRunner "00" "01" "000001"
        CreateTapeTestRunner "11" "00" "010010"
        CreateTapeTestRunner "01" "11" "001011"
        CreateTapeTestRunner "10100" "01110" "010001011001000"

    let AppTestRunner a b =
        AdderTapeTestRunner (CreateTape a b)

    let RunAdderNBitTests args =
        printfn "\n%s" "Adder (n-bit) Tests"
        AppTestRunner "0" "0" "(0)000000"
        AppTestRunner "0" "1" "(0)000101"
        AppTestRunner "1" "0" "(0)000110"
        AppTestRunner "1" "1" "(0)100011"
        AppTestRunner "10100" "01110" "(0)100010001011101000"
        

    let RunTests args =
        RunStackTests args
        RunTapeTests args
        RunMachineTests args
        RunAdder1BitTests args
        RunAdder2BitTests args
        RunCreateTapeTests args
        RunAdderNBitTests args
