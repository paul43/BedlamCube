module BedlamCube.Strategies
open Pieces
open Game




// Strategies for searching the solution space

// Just place pieces till you can't, then back up. 
// Doesn't find solutions in my lifetime
let playGame1 pieces = 
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        
    let mutable attempts = 0L
    let mutable placementTests = 0L
    let mutable maxDepth = 0
    let mutable previous = 0

    let gameBox = Game.makeGameBox()

    let printAttempt () = if attempts % 10000L = 0L then printfn "%A -- %ik attempts. Max depth %2i (previous %2i). Total tests: %i" stopwatch.Elapsed (attempts / 1000L) maxDepth previous placementTests 

    let rec loopOverPieces record (remainingPieces : Piece list) =
        match remainingPieces with
        | [] -> record |> List.rev // win
        | nextPiece :: remainingPieces -> 
            let nextPieceRotations = List.randomise <| nextPiece.Rotations
            loopOverRotations record remainingPieces nextPieceRotations (List.head nextPieceRotations)

    and loopOverRotations record remainingPieces remainingRotations currentPiece =
        match remainingRotations with
            | [] -> loopOverPreviousPieces record remainingPieces 
            | nextRotation :: remainingRotations -> 
                let nextPossibleLocations = List.randomise <| possibleLocations gameBox nextRotation
                loopOverLocations record remainingPieces remainingRotations nextPossibleLocations nextRotation

    and loopOverLocations record remainingPieces remainingRotations remainingLocations currentPiece = 
        match remainingLocations with
        | [] -> loopOverRotations record remainingPieces remainingRotations currentPiece
        | loc :: remainingLocations ->
            placementTests <- placementTests + 1L
            if tryAddPiece gameBox currentPiece loc 
            then 
                let record = { Piece = currentPiece; Loc = loc; Layer = loc.K; RemainingRotations = remainingRotations; RemainingLocations = remainingLocations } :: record
                previous <- List.length record
                maxDepth <- max maxDepth previous
                loopOverPieces record remainingPieces
            else loopOverLocations record remainingPieces remainingRotations remainingLocations currentPiece
        
    and loopOverPreviousPieces (record : PlacementLog list) remainingPieces = 
        match record with 
        | [] -> failwith "Well this isn't going to plan"
        | placementLog :: previousLogs ->
            removePiece gameBox placementLog.Piece placementLog.Loc
            match placementLog.RemainingRotations, placementLog.RemainingLocations with 
            | [], [] -> 
                loopOverPreviousPieces record (placementLog.Piece :: remainingPieces)
            | remainingRotations, remainingLocations -> 
                loopOverLocations record remainingPieces remainingRotations remainingLocations placementLog.Piece


    stopwatch.Start()
    let record = loopOverPieces [] pieces
    stopwatch.Stop()
    let resultString = gameStateString gameBox record 
    printfn "FinalState: \n%s \nAttempts: %i" resultString attempts
    let finalState = Array3D.map id gameBox
    fun () ->  
        record, 
        finalState




















let playGame2 pieces = 
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        
    let mutable attempts = 0L
    let mutable placementTests = 0L
    let mutable maxDepth = 0
    let mutable previous = 0

    let gameBox = Game.makeGameBox()
    let neighboursArray = gameBox |> Array3D.map (fun _ -> -1)

    let printAttempt () = if attempts % 10000L = 0L then printfn "%A -- %ik attempts. Max depth %2i (previous %2i). Total tests: %i" stopwatch.Elapsed (attempts / 1000L) maxDepth previous placementTests 

    let rec loopOverPieces record (remainingPieces : Piece list) =
        match remainingPieces with
        | [] -> record |> List.rev // win
        | nextPiece :: remainingPieces -> 
            let nextPieceRotations = List.randomise <| nextPiece.Rotations
            loopOverRotations record remainingPieces nextPieceRotations (List.head nextPieceRotations)

    and loopOverRotations record remainingPieces remainingRotations currentPiece =
        match remainingRotations with
            | [] -> loopOverPreviousPieces record remainingPieces 
            | nextRotation :: remainingRotations -> 
                let nextPossibleLocations = List.randomise <| possibleLocations gameBox nextRotation
                loopOverLocations record remainingPieces remainingRotations nextPossibleLocations nextRotation

    and loopOverLocations record remainingPieces remainingRotations remainingLocations currentPiece = 
        match remainingLocations with
        | [] -> loopOverRotations record remainingPieces remainingRotations currentPiece
        | loc :: remainingLocations ->
            placementTests <- placementTests + 1L
            if tryAddPieceWithIlligalGapsTest gameBox neighboursArray currentPiece loc 
            then 
                let record = { Piece = currentPiece; Loc = loc; Layer = loc.K; RemainingRotations = remainingRotations; RemainingLocations = remainingLocations } :: record
                previous <- List.length record
                maxDepth <- max maxDepth previous
                loopOverPieces record remainingPieces
            else loopOverLocations record remainingPieces remainingRotations remainingLocations currentPiece
        
    and loopOverPreviousPieces (record : PlacementLog list) remainingPieces = 
        match record with 
        | [] -> failwith "Well this isn't going to plan"
        | placementLog :: previousLogs ->
            removePiece gameBox placementLog.Piece placementLog.Loc
            match placementLog.RemainingRotations, placementLog.RemainingLocations with 
            | [], [] -> 
                loopOverPreviousPieces record (placementLog.Piece :: remainingPieces)
            | remainingRotations, remainingLocations -> 
                loopOverLocations record remainingPieces remainingRotations remainingLocations placementLog.Piece


    stopwatch.Start()
    let record = loopOverPieces [] pieces
    stopwatch.Stop()
    let resultString = gameStateString gameBox record 
    printfn "FinalState: \n%s \nAttempts: %i" resultString attempts
    let finalState = Array3D.map id gameBox
    fun () ->  
        record, 
        finalState



















let playGame3 pieces = 
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        
    let mutable attempts = 0L
    let mutable placementTests = 0L
    let mutable maxDepth = 0
    let mutable previous = 0

    let gameBox = Game.makeGameBox()
    let neighboursArray = gameBox |> Array3D.map (fun _ -> -1)

    let printAttempt () = 
        if attempts % 10000L = 0L then 
            printfn "%A -- %ik attempts. Most recent: %2i, best: %2i. Total tests: %i" 
                stopwatch.Elapsed (attempts / 1000L) previous maxDepth placementTests 

    let rec loopOverPieces record layer discardedPieces (remainingPieces : Piece list) =
        match remainingPieces with
        | [] -> 
            match discardedPieces with 
            | [] -> record |> List.concat |> List.rev // win
            | discardedPieces -> 
                attempts <- attempts + 1L
                printAttempt ()
                loopOverPreviousPieces record discardedPieces 
        | nextPiece :: remainingPieces -> 
            let nextPieceRotations = List.randomise <| nextPiece.Rotations
            loopOverRotations record layer discardedPieces remainingPieces nextPieceRotations (List.head nextPieceRotations)

    and loopOverRotations record layer discardedPieces remainingPieces remainingRotations currentPiece =
        match remainingRotations with
            | [] -> loopOverPieces record layer (currentPiece :: discardedPieces) remainingPieces
            | nextRotation :: remainingRotations -> 
                let nextPossibleLocations = List.randomise <| possibleLocationsInLayer layer gameBox nextRotation
                loopOverLocations record layer discardedPieces remainingPieces remainingRotations nextPossibleLocations nextRotation

    and loopOverLocations record layer discardedPieces remainingPieces remainingRotations remainingLocations currentPiece = 
        match remainingLocations with
        | [] -> loopOverRotations record layer discardedPieces remainingPieces remainingRotations currentPiece
        | loc :: remainingLocations ->
            placementTests <- placementTests + 1L
            if tryAddPieceWithIlligalGapsTest gameBox neighboursArray currentPiece loc 
            then 
                let record      = addLog record { Piece = currentPiece; Loc = loc; Layer = layer; RemainingRotations = remainingRotations; RemainingLocations = remainingLocations } 
                let layerFull   = testLayerFull layer gameBox 
                let nextLayer   = if layerFull then layer + 1 else layer
                let record      = if layerFull then [] :: record else record
                previous <- List.sumBy List.length record
                maxDepth <- max maxDepth previous
                loopOverPieces record nextLayer discardedPieces remainingPieces
            else loopOverLocations record layer discardedPieces remainingPieces remainingRotations remainingLocations currentPiece
        
    and loopOverUnusedOptions (placementLog : PlacementLog) record layer remainingPieces =
        removePiece gameBox placementLog.Piece placementLog.Loc
        match placementLog.RemainingRotations, placementLog.RemainingLocations with 
        | [], [] -> 
            loopOverPreviousPieces record (placementLog.Piece :: remainingPieces)
        | remainingRotations, remainingLocations -> 
            loopOverLocations record layer [] remainingPieces remainingRotations remainingLocations placementLog.Piece

    and loopOverPreviousPieces record remainingPieces = 
        match record with 
        | [] -> failwith "Well this isn't going to plan"
        | thisLevel :: previousLevels ->
            match thisLevel with 
            | [] -> 
                loopOverPreviousPieces previousLevels remainingPieces
            | lastPlacement :: previousPlacements -> 
                loopOverUnusedOptions lastPlacement (previousPlacements :: previousLevels) lastPlacement.Layer remainingPieces


    stopwatch.Start()
    let record = 
        loopOverPieces [] 0 [] (List.randomise pieces)
        |> List.mapi (fun i log -> { log with Game.Piece = { log.Piece with Label = char (int 'A' + i)} })
    stopwatch.Stop()
    let resultString = gameStateString gameBox record 
    printfn "FinalState: \n%s \nAttempts: %i" resultString attempts
    let finalState = Array3D.map id gameBox
    fun () ->  
        record, 
        finalState

