namespace BedlamCube

// Helper functions for placeing pieces in the game box using the array representation

module Game = 
    open Pieces



    type Box = bool [,,]
        
    type PlacementLog = 
        {
            Piece : Piece
            Loc : Location
            Layer : int
            RemainingRotations : Piece list
            RemainingLocations : Location list
        }

    #if INTERACTIVE
    fsi.AddPrinter(fun (b : Box) -> b.ToString())
    #endif

    let makeGameBox () = Array3D.create 4 4 4 false

    let possibleLocations box (piece : Piece) = 
        let box1 = Array3D.length1 box
        let box2 = Array3D.length2 box
        let box3 = Array3D.length3 box
        let p1   = Array3D.length1 piece.Coordinates
        let p2   = Array3D.length2 piece.Coordinates
        let p3   = Array3D.length3 piece.Coordinates
        if p1 > box1 || p2 > box2 || p3 > box3 then []
        else 
            [ for i = 0 to box1 - p1 do
                for j = 0 to box2 - p2 do
                    for k = 0 to box3 - p3 do
                        yield Location (i,j,k) ]

    let possibleLocationsInLayer layer box (piece : Piece) = 
        let box1 = Array3D.length1 box
        let box2 = Array3D.length2 box
        let box3 = Array3D.length3 box
        let p1   = Array3D.length1 piece.Coordinates
        let p2   = Array3D.length2 piece.Coordinates
        let p3   = Array3D.length3 piece.Coordinates
        if p1 > box1 || p2 > box2 || (p3 + layer) > box3 then []
        else 
            [ for i = 0 to box1 - p1 do
                for j = 0 to box2 - p2 do
                    yield Location (i,j,layer) ]

    let testLayerFull layer box = 
        let d1 = Array3D.length1 box
        let d2 = Array3D.length2 box
        let mutable full = true
        for i = 0 to d1 - 1 do
            for j = 0 to d2 - 1 do
                full <- full && box.[i,j,layer]
        full

    let numPositions box (piece : Piece) = 
        piece.Rotations 
        |> List.collect (possibleLocations box)
        |> List.length
    
    let testPlacement (box : Box) (piece : Piece) (loc : Location) = 
        let mutable fit = true
        try 
            piece.Coordinates 
            |> Array3D.iteri (fun i j k block -> 
                let thisCoordOk = not (box.[i + loc.I, j + loc.J, k + loc.K] && block)
                fit <- fit && thisCoordOk)
        with | e -> printfn "Error: tried to place piece \n%s\n at loc %s" (piece.ToString()) (loc.ToString()); raise e
        fit

    let countEmptyNeighbours (box : Box) i j k = 
        let mutable emptyNeighbours = 0
        if i > 0                        && not box.[i - 1, j, k] then emptyNeighbours <- emptyNeighbours + 1
        if i < Array3D.length1 box - 1  && not box.[i + 1, j, k] then emptyNeighbours <- emptyNeighbours + 1
        if j > 0                        && not box.[i, j - 1, k] then emptyNeighbours <- emptyNeighbours + 1
        if j < Array3D.length2 box - 1  && not box.[i, j + 1, k] then emptyNeighbours <- emptyNeighbours + 1
        if k > 0                        && not box.[i, j, k - 1] then emptyNeighbours <- emptyNeighbours + 1
        if k < Array3D.length3 box - 1  && not box.[i, j, k + 1] then emptyNeighbours <- emptyNeighbours + 1
        emptyNeighbours

    let calculateEmptyNeighbours gameBox (neighboursArray : int [,,]) = 
        gameBox |> Array3D.iteri (fun i j k filled ->
            if filled 
            then neighboursArray.[i,j,k] <- -1
            else neighboursArray.[i,j,k] <- countEmptyNeighbours gameBox i j k)

    let sumNeighbours (a : int [,,]) i j k = 
        let mutable sum = 0
        if i > 0                        && a.[i - 1, j, k] > 0 then sum <- sum + a.[i - 1, j, k]
        if i < Array3D.length1 a - 1    && a.[i + 1, j, k] > 0 then sum <- sum + a.[i + 1, j, k]
        if j > 0                        && a.[i, j - 1, k] > 0 then sum <- sum + a.[i, j - 1, k]
        if j < Array3D.length2 a - 1    && a.[i, j + 1, k] > 0 then sum <- sum + a.[i, j + 1, k]
        if k > 0                        && a.[i, j, k - 1] > 0 then sum <- sum + a.[i, j, k - 1]
        if k < Array3D.length3 a - 1    && a.[i, j, k + 1] > 0 then sum <- sum + a.[i, j, k + 1]
        sum

    let illigalGapsTest neighboursArray = 
        let mutable ok = true
        neighboursArray |> Array3D.iteri (fun i j k neighbours ->
            if neighbours < 0 || neighbours > 2 then ()
            else if neighbours = 0 then ok <- false
            else if neighbours = 1 then 
                if sumNeighbours neighboursArray i j k <= 1 then ok <- false
            else if neighbours = 2 then
                if sumNeighbours neighboursArray i j k <= 2 then ok <- false)
        ok

    let addPiece (box : Box) (piece : Piece) (loc : Location) = 
        piece.Coordinates 
        |> Array3D.iteri (fun i j k block -> if block then box.[i + loc.I, j + loc.J, k + loc.K] <- true)

    let removePiece (box : Box) (piece : Piece) (loc : Location) = 
        piece.Coordinates 
        |> Array3D.iteri (fun i j k block -> if block then box.[i + loc.I, j + loc.J, k + loc.K] <- false)

    let tryAddPiece box piece loc = 
        if testPlacement box piece loc
        then addPiece box piece loc; true
        else false

    let tryAddPieceWithIlligalGapsTest box neighboursArray piece loc = 
        if testPlacement box piece loc
        then 
            addPiece box piece loc
            calculateEmptyNeighbours box neighboursArray 
            if illigalGapsTest neighboursArray
            then true
            else 
                removePiece box piece loc
                false
        else false

    let gameStateString (box : Box) (record : PlacementLog list) = 
        let blankChar = '_'
        let charBox = box |> Array3D.map (fun _ -> blankChar)
        record 
        |> List.iter (fun log -> 
            let loc = log.Loc
            log.Piece.Coordinates |> Array3D.iteri (fun i j k block ->
                if block then 
                    if charBox.[i + loc.I, j + loc.J, k + loc.K] = blankChar
                    then charBox.[i + loc.I, j + loc.J, k + loc.K] <- log.Piece.Label
                    else failwith "Invalid state"))
        Array3D.toString charBox

    let resetGame box = 
        box |> Array3D.iteri (fun i j k _ -> box.[i,j,k] <- false)
    
    let setGameStateToRecord gameBox (record : PlacementLog list) = 
        resetGame gameBox
        record |> List.iter (fun log -> 
            let loc = log.Loc
            log.Piece.Coordinates |> Array3D.iteri (fun i j k block ->
                if block then 
                    if not gameBox.[i + loc.I, j + loc.J, k + loc.K] 
                    then gameBox.[i + loc.I, j + loc.J, k + loc.K] <- true
                    else failwith "Invalid state"))
    
    let addLog (record : PlacementLog list list) item =
        match record with
        | [] -> [[item]]
        | head :: tail -> (item :: head) :: tail    



