// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

module EndlessStack
    type MachineValue = 
        | NADA = 0
        | WITH = 1
    type T = StackContents of MachineValue list

    let WITH = MachineValue.WITH
    let NADA = MachineValue.NADA
    
    let defaultValue = NADA
    let (StackContents emptyStack) = StackContents []

    let New = emptyStack

    let Push x (StackContents stack) = List.append x stack
    let Pop (StackContents stack) =
        match stack with
        | top::rest -> (top, rest)
        | [] -> (defaultValue, emptyStack)

    let ToList (StackContents stack) : MachineValue list = stack

    let AppRun args =
        Seq.cast<MachineValue> args |> Seq.toList |> StackContents

    let Test input test =
        AppRun input |> test |> printfn "%b"

    let TestRun args =
         Test [] (ToList >> List.isEmpty)
         Test [0] (fun x -> x |> ToList |> List.length = 1)
         Test [1] (fun x -> x |> ToList |> List.length = 1)
         Test [0] (fun x -> Pop x = (NADA, emptyStack))
         Test [0;1;0;1] (fun x -> Pop x = (NADA, [WITH;NADA;WITH]))

    [<EntryPoint>]
    let main argv =
        argv |> Array.toList |> TestRun
        0 // return an integer exit code
