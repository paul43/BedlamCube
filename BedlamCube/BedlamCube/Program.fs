// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
namespace BedlamCube


module NativeInterop = 
    open System.Runtime.InteropServices

    [<DllImport("kernel32.dll")>]
    extern uint32 SetThreadExecutionState(uint32 esFlags)
    [<Literal>]
    let ES_CONTINUOUS = 0x80000000u;
    [<Literal>]
    let ES_SYSTEM_REQUIRED = 0x00000001u;

module Main = 
    let rec presentOptions option keys = 
        printfn "\n%s or (Q)uit?" option
        match System.Console.ReadKey() with
        | s when List.contains s.KeyChar keys -> 
            
            // try and prevent the system going into sleep mode while we're working
            NativeInterop.SetThreadExecutionState(NativeInterop.ES_CONTINUOUS ||| NativeInterop.ES_SYSTEM_REQUIRED) |> ignore
            
            printfn ""
            printfn "\nRunning BedlamCube solver:"
            let pieces = Pieces.allPieces 
//            ignore <| Strategies.playGame3 pieces
            ignore <| Version2.playGame pieces

            NativeInterop.SetThreadExecutionState(NativeInterop.ES_CONTINUOUS) |> ignore

            presentOptions "(R)e-run" ['R'; 'r']
        | s when s.KeyChar = 'q' || s.KeyChar = 'Q' -> 0
        | _ -> 
            printfn "\nUnrecognised option" 
            presentOptions option keys



    [<EntryPoint>]
    let main argv = 
        printfn "++++++++++++++++++++++"
        printfn "+ Bedlam Cube Solver +" 
        printfn "++++++++++++++++++++++"
        printfn ""

        presentOptions "(S)tart" ['S'; 's']
