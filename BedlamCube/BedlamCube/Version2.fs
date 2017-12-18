module BedlamCube.Version2
open Pieces
open System
open System.Collections.Generic


   


[<Literal>] 
let oneMillion = 1000000L

let formatInt (l : int) = System.String.Format("{0:#,###}", l)
let formatLong (l : int64) = System.String.Format("{0:#,###}", l)

let encodeCoordinate i j k = 
    let n = 16 * k + 4 * j + i
    1UL <<< n

let encodeLocation (loc : Location) = 
    encodeCoordinate loc.I loc.J loc.K 

let binaryIndexToLocation n = 
    let k = n / 16
    let j = (n % 16) / 4
    let i = n % 4
    Location.Make i j k

let encodePieceAsInt64 (piece : Piece) (loc : Location) =
    let mutable encoding = 0UL
    piece.Coordinates
    |> Array3D.iteri (fun i j k b ->
        if b then encoding <- encoding ||| encodeCoordinate (i + loc.I) (j + loc.J) (k + loc.K))
    encoding

let encodeBoxAsInt64 (gameBox : bool[,,]) = 
    let mutable encoding = 0UL
    gameBox |> Array3D.iteri (fun i j k b -> 
        if b then encoding <- encoding ||| encodeCoordinate i j k)
    encoding

let decodeInt64ToBox (encoding : uint64) = 
    let box = Array3D.create 4 4 4 false
    for n = 0 to 63 do
        let loc = binaryIndexToLocation n
        if encoding >>> n &&& 1UL = 1UL 
        then box.[loc.I, loc.J, loc.K] <- true
    box


let inline testBitSet (l : uint64) (n : int) = 
    l >>> n &&& 1UL = 1UL

let inline testPlacementSimple (boxEncoding : uint64) (pieceEncoding : uint64) = 
    boxEncoding &&& pieceEncoding = 0UL

let inline addPiece (boxEncoding : uint64) (pieceEncoding : uint64) = 
    boxEncoding ||| pieceEncoding 

let inline removePiece (boxEncoding : uint64) (pieceEncoding : uint64) = 
    boxEncoding ^^^ pieceEncoding 

let inline testLayerFull (boxEncoding : uint64) (layer : int) = 
    boxEncoding >>> (layer * 16) &&& 65535UL = 65535UL // 65535L = 2^16 - 1, i.e. 16 1s
    
let neighbourEncodings =   
    Array.init 64 (fun n -> 
        let loc = binaryIndexToLocation n
        let mutable neighboursEncoding = 0UL
        if loc.I > 0 then neighboursEncoding <- neighboursEncoding ||| encodeCoordinate (loc.I - 1) loc.J loc.K
        if loc.I < 3 then neighboursEncoding <- neighboursEncoding ||| encodeCoordinate (loc.I + 1) loc.J loc.K
        if loc.J > 0 then neighboursEncoding <- neighboursEncoding ||| encodeCoordinate loc.I (loc.J - 1) loc.K
        if loc.J < 3 then neighboursEncoding <- neighboursEncoding ||| encodeCoordinate loc.I (loc.J + 1) loc.K
        if loc.K > 0 then neighboursEncoding <- neighboursEncoding ||| encodeCoordinate loc.I loc.J (loc.K - 1)
        if loc.K < 3 then neighboursEncoding <- neighboursEncoding ||| encodeCoordinate loc.I loc.J (loc.K + 1)
        neighboursEncoding)
   
let inline dropLowestBit (l : uint64) = l &&& (l - 1UL) 

/// We only need to know if the encoding has 0, 1, 2 or more set bits
let countBits (l : uint64) = 
    if l = 0UL then 0 else
        let l1 = dropLowestBit l
        if l1 = 0UL then 1 else 
            let l2 = dropLowestBit l1
            if l2 = 0UL then 2 else 3
        
let inline moreThanOneBitSet (l : uint64) = l <> 0UL && dropLowestBit l <> 0UL

let getEmptyNeighbours (boxEncoding : uint64) n = 
    let neighboursEncoding = neighbourEncodings.[n]
    ~~~boxEncoding &&& neighboursEncoding

let ffsSlow l = 
    if l = 0UL then 0 else 
        let mutable t = 1UL
        let mutable r = 0
        while (l &&& t) = 0UL do t <- t <<< 1; r <- r + 1
        r
    
/// Gets the index of the most significant bit. Using a lookup table is slightly faster than the naive method above.
let findFirstSet = 
    let table = Array.init (1 <<< 8) (fun i -> ffsSlow (uint64 i))
    fun (l : uint64) ->
        let mutable l = l
        let mutable r = 0
        while l &&& 255UL = 0UL do l <- l >>> 8; r <- r + 8
        r + table.[ int (l &&& 255UL) ]



let testPlacementWithNeighbourCheck (boxEncoding : uint64) layer (pieceEncoding : uint64) = 
    if not <| testPlacementSimple boxEncoding pieceEncoding
    then false
    else
        let mutable n = layer * 16
        let mutable ok = true
        while ok && n < 64 do
            if not <| testBitSet boxEncoding n 
            then 
                let emptyNeighbours = getEmptyNeighbours boxEncoding n
                match countBits emptyNeighbours with
                | 0 -> ok <- false
                | 1 -> 
                    let neighbourIndex = findFirstSet emptyNeighbours
                    ok <- moreThanOneBitSet (getEmptyNeighbours boxEncoding neighbourIndex)
                | 2 -> 
                    let neighbourIndex1 = findFirstSet emptyNeighbours
                    if moreThanOneBitSet (getEmptyNeighbours boxEncoding neighbourIndex1) then ()
                    else 
                        let neighbourIndex2 = findFirstSet (dropLowestBit emptyNeighbours)
                        ok <- moreThanOneBitSet (getEmptyNeighbours boxEncoding neighbourIndex2)
                | _ -> ()
            n <- n + 1
        ok





type PlacementLog(pieceIndex : int, char : char, layer : int, positionIndex : int, encoding : uint64, discardedPieces : int list) = 
    struct
        member this.PieceIndex = pieceIndex
        member this.Char = char
        member this.Layer = layer
        member this.PositionIndex = positionIndex
        member this.Encoding = encoding
        member this.DiscardedPieces = discardedPieces
        override this.ToString() = sprintf "Piece %i in position %i of layer %i (%iUL)" this.PieceIndex this.PositionIndex this.Layer this.Encoding
    end






[<CustomEquality; CustomComparison>]
type Solution = 
    {
        Box : Char[,,]
    }
    static member fromRecord (record : PlacementLog list) = 
        let blankChar = '_'
        let charBox = Array3D.create 4 4 4 blankChar
        record 
        |> List.iteri (fun n log -> 
            log.Encoding
            |> decodeInt64ToBox
            |> Array3D.iteri (fun i j k b -> if b then charBox.[i,j,k] <- log.Char))
        { Box = charBox }

    member this.Rotations = 
        Rotation.List |> List.collect (fun rot1 -> 
        Rotation.List |> List.collect (fun rot2 -> 
        Rotation.List |> List.map (fun rot3 -> 
            this.Box |> Rotation.ApplyToArray3D rot1 |> Rotation.ApplyToArray3D rot2 |> Rotation.ApplyToArray3D rot3)))
        |> List.distinct
        |> List.sort

    override this.ToString() = Array3D.toString this.Box

    override this.GetHashCode() = hash this.Rotations
    override this.Equals(that : obj) = 
        match that with
        | :? Solution as that -> this.Rotations = that.Rotations
        | _ -> false

    interface IComparable with 
        member this.CompareTo that = 
            match that with 
            | :? Solution as that -> compare this.Rotations that.Rotations
            | _ -> failwith "Can't compare to solution"





let playGame (pieces : Piece list) = 
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let pieces          = List.randomise pieces |> List.toArray
    let pieceChars      = pieces |> Array.map (fun p -> p.Label)
    let numPieces       = Array.length pieces
    let placedPieces    = Array.create numPieces false

    let solutions       = ResizeArray<_>()

    let gameBox = Game.makeGameBox()
    let initialboxEncoding = encodeBoxAsInt64 gameBox

    let mutable totalTests = 0L
    let mutable totalAttempts = 0L
    let mutable tests = 0L
    let mutable attempts = 0L
    let mutable lastSuccessTime = TimeSpan.Zero

    let allPossiblePiecePositions = 
        let piecesAndRotations = 
            let mutable restrictedOnePiece = false
            pieces
            |> Array.map (fun piece -> 
                // We exclude rotations of one piece to make sure we're not finding solution that are rotations of each other. 
                // We pick a piece with no rotational symetry to be on the safe side
                if List.length piece.Rotations = 24 && not restrictedOnePiece
                then restrictedOnePiece <- true; [piece]
                else piece.Rotations)

        Array2D.init numPieces 4 (fun pieceIndex layer ->
            piecesAndRotations.[pieceIndex]
            |> List.collect (fun rotation ->                 
                Game.possibleLocationsInLayer layer gameBox rotation
                |> List.map (encodePieceAsInt64 rotation))
            |> List.distinct
            |> List.randomise 
            |> List.toArray)
        
    let recordSolution =
        let filePath = @"BedlamCubeSolutions_" + (DateTime.Now.ToString("yyyyMMdd_HHmm")) + ".txt"
        fun string -> System.IO.File.AppendAllText(filePath, string)
        

    let printAttempt () = if attempts % oneMillion = 0L then printf "."; if attempts % (50L * oneMillion) = 0L then printfn ""

    let printSolution (record : PlacementLog list) = 
        let solution = Solution.fromRecord record
        
        let solutionString = solution.ToString()
        let totalTime = stopwatch.Elapsed
        let elapsed = TimeSpan.op_Subtraction(totalTime, lastSuccessTime).TotalSeconds
        let testsPerSecond = int <| double tests / elapsed
        let attempsPerSecond = int <| double attempts / elapsed
        printfn "\n\n%A -- Solution %i found in %s attempts. Attempts/s %s, tests/s %s\n%s\n" totalTime solutions.Count (formatLong attempts) (formatInt attempsPerSecond) (formatInt testsPerSecond) solutionString
        recordSolution (sprintf "\r\n%i\r\n%s\r\n" solutions.Count solutionString)
                
        solutions.Add solution 

        totalTests <- totalTests + tests
        totalAttempts <- totalAttempts + attempts
        lastSuccessTime <- totalTime
        tests <- 0L
        attempts <- 0L

    let rec loopOverPieces boxEncoding (record : PlacementLog list) layer remainingPieces discardedPieces = 
        match remainingPieces with
        | [] -> 
            if not (placedPieces |> Array.contains false) then printSolution record
            attempts <- attempts + 1L
            printAttempt ()
            loopOverPreviousPieces boxEncoding record layer discardedPieces
        | nextPiece :: remainingPieces -> loopOverLocations boxEncoding record layer remainingPieces discardedPieces nextPiece 0
        
    and loopOverLocations boxEncoding record layer remainingPieces discardedPieces currentPiece positionIndex = 
        let positionsToTest = allPossiblePiecePositions.[currentPiece, layer]
        
        if positionIndex < positionsToTest.Length
        then
            let positionToTest = positionsToTest.[positionIndex]
            tests <- tests + 1L
            if testPlacementWithNeighbourCheck boxEncoding layer positionToTest
            then 
                let boxEncoding = addPiece boxEncoding positionToTest
                placedPieces.[currentPiece] <- true
                let record = PlacementLog(currentPiece, pieceChars.[currentPiece], layer, positionIndex, positionToTest, discardedPieces) :: record
                if testLayerFull boxEncoding layer 
                then 
                    // when we go up a layer we want to bring all previously discarded pieces back into the game
                    let remainingPieces = [ 0 .. numPieces - 1 ] |> List.filter (fun i -> not placedPieces.[i]) 
                    loopOverPieces boxEncoding record (layer + 1) remainingPieces []

                else loopOverPieces boxEncoding record layer remainingPieces [] 
            else loopOverLocations boxEncoding record layer remainingPieces discardedPieces currentPiece (positionIndex + 1)
        else loopOverPieces boxEncoding record layer remainingPieces (currentPiece :: discardedPieces)

    and loopOverPreviousPieces boxEncoding (record : PlacementLog list) layer discardedPieces = 
        match record with 
        | [] -> () // Done!
        | lastPlacement :: earlierPlacements ->
            let boxEncoding = removePiece boxEncoding lastPlacement.Encoding
            placedPieces.[lastPlacement.PieceIndex] <- false
            let remainingPieces =
                if lastPlacement.Layer = layer 
                then discardedPieces
                else // we're dropping down a layer, so need to remove from the game those pieces that we'd previously ruled out on the layer below
                    let piecesToExclude = Array.copy placedPieces
                    record |> List.filter (fun l -> l.Layer = lastPlacement.Layer) |> List.iter (fun l -> l.DiscardedPieces |> List.iter (fun i -> piecesToExclude.[i] <- true))
                    discardedPieces |> List.filter (fun pieceIndex -> not piecesToExclude.[pieceIndex])
            
            loopOverLocations boxEncoding earlierPlacements lastPlacement.Layer remainingPieces lastPlacement.DiscardedPieces lastPlacement.PieceIndex (lastPlacement.PositionIndex + 1)

    stopwatch.Start()
    loopOverPieces initialboxEncoding [] 0 [0 .. numPieces - 1] [] 
    stopwatch.Stop()
    
    printfn "\n\n%A -- Finished search. Total attempts %s, total tests %s" stopwatch.Elapsed (formatLong <| attempts + totalAttempts) (formatLong <| tests + totalTests)
    printfn "%i solutions found. Average solution time %g seconds (%g solutions / hour)" solutions.Count (stopwatch.Elapsed.TotalSeconds / double solutions.Count) (double solutions.Count / stopwatch.Elapsed.TotalHours)

    let solutionsSet = solutions.ToArray() |> Set.ofArray

    printfn "Unique solutions: %i" solutionsSet.Count




    ()






let test () = 
    let pieces = Pieces.allPieces |> List.take 13
    playGame pieces 








