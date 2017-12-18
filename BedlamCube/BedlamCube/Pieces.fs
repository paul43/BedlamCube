namespace BedlamCube

// Setting up a little DSL for building and manipulating the puzzle pieces

module Pieces = 

    // A location in three dimensions
    type Location(i : int, j : int, k : int) = 
        struct
            member this.I = i
            member this.J = j
            member this.K = k
        end
        override this.ToString () = sprintf "(%i, %i, %i)" this.I this.J this.K
        static member Make i j k = Location(i, j, k) 

    // We build pieces by walking in 3-dimensions and leaving a trail of blocks
    type Move =
        | North
        | South
        | East
        | West
        | Up
        | Down
        member this.Directions =
            match this with 
            | North -> 1, -1
            | South -> 1,  1
            | East  -> 2,  1
            | West  -> 2, -1
            | Up    -> 3,  1
            | Down  -> 3, -1
        static member Apply move (loc : Location) = 
            match move with
            | North -> Location(loc.I - 1, loc.J, loc.K) 
            | South -> Location(loc.I + 1, loc.J, loc.K) 
            | East  -> Location(loc.I, loc.J + 1, loc.K) 
            | West  -> Location(loc.I, loc.J - 1, loc.K) 
            | Up    -> Location(loc.I, loc.J, loc.K + 1) 
            | Down  -> Location(loc.I, loc.J, loc.K - 1) 

    #if INTERACTIVE
    fsi.AddPrinter(fun (l : Location) -> l.ToString())
    #endif


    // A piece is described by the smallest 3D array it fits inside, with true representing a block and false empty space
    [<CustomEquality; NoComparison>]
    type Piece = 
        { 
            Coordinates : bool [,,]
            Label : char 
        }
        override this.ToString () = "\n" +  Array3D.toString (Array3D.map (function true -> this.Label | false -> '_') this.Coordinates)
        
        member this.Rotations = 
            Rotation.List |> List.collect (fun rot1 -> 
            Rotation.List |> List.collect (fun rot2 -> 
            Rotation.List |> List.map (fun rot3 -> 
                this |> Rotation.Apply rot1 |> Rotation.Apply rot2 |> Rotation.Apply rot3)))
            |> List.distinct

        static member Make dim1 dim2 dim3 label = 
            { 
                Coordinates     = Array3D.create dim1 dim2 dim3 false
                Label           = label
            }
        static member Add (loc : Location) (p : Piece) = p.Coordinates.[loc.I, loc.J, loc.K] <- true; p

        // If we walk off the edge of the containing box, we need to make it bigger
        static member Extend dimension direction (piece : Piece) = 
            let d1, d2, d3 = Array3D.length1 piece.Coordinates, Array3D.length2 piece.Coordinates, Array3D.length3 piece.Coordinates
            let offset = if direction >= 0 then 0 else 1
            let newCoords = 
                match dimension with 
                | 1 -> 
                    let newCoords = Array3D.create (d1 + 1) d2 d3 false
                    piece.Coordinates |> Array3D.iteri (fun i j k b -> newCoords.[i + offset,j,k] <- b)
                    newCoords
                | 2 -> 
                    let newCoords = Array3D.create d1 (d2 + 1) d3 false
                    piece.Coordinates |> Array3D.iteri (fun i j k b -> newCoords.[i,j + offset,k] <- b)
                    newCoords
                | 3 -> 
                    let newCoords = Array3D.create d1 d2 (d3 + 1) false
                    piece.Coordinates |> Array3D.iteri (fun i j k b -> newCoords.[i,j,k + offset] <- b)
                    newCoords
                | _ -> failwith "Invalid dimension"
            { piece with Coordinates = newCoords }
        override this.Equals that = match that with | :? Piece as that -> this.Coordinates = that.Coordinates | _ -> false
        override this.GetHashCode () = hash this.Coordinates 

        static member EncodeAsLong (piece : Piece) (loc : Location) = ()
        



    and Rotation = 
        | Dim1 of int
        | Dim2 of int
        | Dim3 of int
        static member List = [ Dim1 0; Dim1 1; Dim1 2; Dim1 -1; Dim2 1; Dim2 -1; ]
        static member Dim1Rotations = [ Dim1 0; Dim1 1; Dim1 2; Dim1 -1 ]

        static member ApplyToArray3D rotation (a : 'a [,,]) = 
            let newArray = 
                match rotation with
                | Dim1 turns -> Array3D.rotateDim1 turns a
                | Dim2 turns -> Array3D.rotateDim2 turns a
                | Dim3 turns -> Array3D.rotateDim3 turns a
            newArray

        static member Apply rotation piece = 
            { piece with Coordinates = Rotation.ApplyToArray3D rotation piece.Coordinates }

    #if INTERACTIVE
    fsi.AddPrinter(fun (p : Piece) -> p.ToString())
    #endif
   

    


    // This guy takes instructions for how to build game pieces
    type PieceBuilder = 
        { Piece : Piece; Loc : Location }      
        static member Start label = 
            { 
                Piece   = Piece.Make 1 1 1 label |> Piece.Add (Location.Make 0 0 0)
                Loc     = Location.Make 0 0 0 
            }
        static member ToPiece (pb : PieceBuilder) = pb.Piece

        static member Move (move : Move) (pb : PieceBuilder) = 
            let dim, dir = move.Directions
            let newLoc = Move.Apply move pb.Loc
            let extendPiece = 
                newLoc.I < 0 || newLoc.I > Array3D.length1 pb.Piece.Coordinates - 1
                || newLoc.I < 0 || newLoc.J > Array3D.length2 pb.Piece.Coordinates - 1
                || newLoc.I < 0 || newLoc.K > Array3D.length3 pb.Piece.Coordinates - 1
            let piece = if extendPiece then Piece.Extend dim dir pb.Piece else pb.Piece
            let newLoc = if extendPiece && dir < 0 then pb.Loc else newLoc
            { 
                Piece = Piece.Add newLoc piece
                Loc = newLoc 
            }

    // The 13 puzzle pieces

    let pieceA = 
        PieceBuilder.Start 'A' 
        |> PieceBuilder.Move South
        |> PieceBuilder.Move East
        |> PieceBuilder.Move Up 
        |> PieceBuilder.ToPiece
        
    let pieceB = 
        PieceBuilder.Start 'B' 
        |> PieceBuilder.Move South
        |> PieceBuilder.Move East
        |> PieceBuilder.Move Up
        |> PieceBuilder.Move South
        |> PieceBuilder.ToPiece

    let pieceC = 
        PieceBuilder.Start 'C' 
        |> PieceBuilder.Move East
        |> PieceBuilder.Move North
        |> PieceBuilder.Move East
        |> PieceBuilder.Move Up
        |> PieceBuilder.ToPiece

    let pieceD = 
        PieceBuilder.Start 'D'
        |> PieceBuilder.Move East
        |> PieceBuilder.Move East
        |> PieceBuilder.Move South
        |> PieceBuilder.Move Up
        |> PieceBuilder.ToPiece

    let pieceE = 
        PieceBuilder.Start 'E'
        |> PieceBuilder.Move East
        |> PieceBuilder.Move South
        |> PieceBuilder.Move East
        |> PieceBuilder.Move South
        |> PieceBuilder.ToPiece

    let pieceF = 
        PieceBuilder.Start 'F'
        |> PieceBuilder.Move South
        |> PieceBuilder.Move Up
        |> PieceBuilder.Move Down
        |> PieceBuilder.Move East
        |> PieceBuilder.Move West
        |> PieceBuilder.Move South
        |> PieceBuilder.ToPiece

    let pieceG = 
        PieceBuilder.Start 'G'
        |> PieceBuilder.Move North
        |> PieceBuilder.Move East
        |> PieceBuilder.Move East
        |> PieceBuilder.Move Up
        |> PieceBuilder.ToPiece

    let pieceH = 
        PieceBuilder.Start 'H'
        |> PieceBuilder.Move East
        |> PieceBuilder.Move Up
        |> PieceBuilder.Move Down
        |> PieceBuilder.Move North
        |> PieceBuilder.Move East
        |> PieceBuilder.ToPiece

    let pieceI = 
        PieceBuilder.Start 'I'
        |> PieceBuilder.Move South
        |> PieceBuilder.Move East
        |> PieceBuilder.Move Up
        |> PieceBuilder.Move Down
        |> PieceBuilder.Move West
        |> PieceBuilder.Move South
        |> PieceBuilder.ToPiece

    let pieceJ = 
        PieceBuilder.Start 'J'
        |> PieceBuilder.Move East
        |> PieceBuilder.Move South
        |> PieceBuilder.Move North
        |> PieceBuilder.Move East
        |> PieceBuilder.Move West
        |> PieceBuilder.Move North
        |> PieceBuilder.ToPiece

    let pieceK = 
        PieceBuilder.Start 'K'
        |> PieceBuilder.Move South
        |> PieceBuilder.Move Up
        |> PieceBuilder.Move Down
        |> PieceBuilder.Move East
        |> PieceBuilder.Move East
        |> PieceBuilder.ToPiece

    let pieceL = 
        PieceBuilder.Start 'L'
        |> PieceBuilder.Move East
        |> PieceBuilder.Move South
        |> PieceBuilder.Move South
        |> PieceBuilder.Move North
        |> PieceBuilder.Move East
        |> PieceBuilder.ToPiece

    let pieceM = 
        PieceBuilder.Start 'M'
        |> PieceBuilder.Move South
        |> PieceBuilder.Move Up
        |> PieceBuilder.Move Down
        |> PieceBuilder.Move South
        |> PieceBuilder.Move East
        |> PieceBuilder.ToPiece

    let allPieces = [ pieceA; pieceB; pieceC; pieceD; pieceE; pieceF; pieceG; pieceH; pieceI; pieceJ; pieceK; pieceL; pieceM ]


