namespace BedlamCube





[<AutoOpen>]
module Helpers =
    let cprintf c fmt =
        Printf.kprintf (fun s -> 
            let old = System.Console.ForegroundColor
            try 
                System.Console.ForegroundColor <- c
                System.Console.Write s
            finally
                System.Console.ForegroundColor<- old)
            fmt

module List = 
    let private r = System.Random()
    let randomise list = list |> List.sortBy (fun _ -> r.NextDouble())


// Helper functions for manipulating 2 and 3-dim arrays. 
// Allow roation, transoposition etc in 2 and 3 dimensions

module Array2D =    

    let length i (a : 'a [,]) = 
        match i with 
        | 1 -> Array2D.length1 a
        | 2 -> Array2D.length2 a
        | _ -> failwith "Invalid dimension"

    let transpose (a : 'a [,]) = 
        let d1 = Array2D.length1 a
        let d2 = Array2D.length2 a
        Array2D.init d2 d1 (fun i j -> a.[j,i])

    let reflectDim1 (a : 'a [,]) = 
        let d1 = Array2D.length1 a
        a |> Array2D.mapi (fun i j _ -> a.[d1 - 1 - i,j])

    let reflectDim2 (a : 'a [,]) = 
        let d2 = Array2D.length2 a
        a |> Array2D.mapi (fun i j _ -> a.[i,d2 - 1 - j])

    let rec rotate turns (a : 'a [,]) = 
        if   turns > 0 then a |> transpose |> reflectDim1 |> rotate (turns - 1)
        elif turns < 0 then a |> transpose |> reflectDim2 |> rotate (turns + 1)
        else a

    let toString (a : 'a [,]) = 
        let d1 = Array2D.length1 a
        let d2 = Array2D.length2 a
        [ 0 .. d1 - 1] 
        |> List.map (fun i -> 
            [ 0 .. d2 - 1 ] 
            |> List.map (Array2D.get a i >> fun x -> x.ToString()) 
            |> String.concat " ")
        |> String.concat "    "

module Array3D = 

    let length i (a : 'a [,,]) = 
        match i with 
        | 1 -> Array3D.length1 a
        | 2 -> Array3D.length2 a
        | 3 -> Array3D.length3 a
        | _ -> failwith "Invalid dimension"

    let dim1Slice index (a : 'a [,,]) = 
        let dim1 = Array3D.length1 a
        let dim2 = Array3D.length2 a
        let dim3 = Array3D.length3 a
        if index < 0 || index > dim1 then raise <| System.IndexOutOfRangeException()
        Array2D.init dim2 dim3 (fun j k -> a.[index,j,k])

    let dim2Slice index (a : 'a [,,]) = 
        let dim1 = Array3D.length1 a
        let dim2 = Array3D.length2 a
        let dim3 = Array3D.length3 a
        if index < 0 || index > dim2 then raise <| System.IndexOutOfRangeException()
        Array2D.init dim1 dim3 (fun i k -> a.[i,index,k])

    let dim3Slice index (a : 'a [,,]) = 
        let dim1 = Array3D.length1 a
        let dim2 = Array3D.length2 a
        let dim3 = Array3D.length3 a
        if index < 0 || index > dim3 then raise <| System.IndexOutOfRangeException()
        Array2D.init dim1 dim2 (fun i j -> a.[i,j,index])

    let fromDim1Slices (a : 'a [,] []) = 
        let d1 = a.Length
        let d2 = Array2D.length1 a.[0]
        let d3 = Array2D.length2 a.[0]
        Array3D.init d1 d2 d3 (fun i j k -> a.[i].[j,k])
        
    let fromDim2Slices (a : 'a [,] []) = 
        let d2 = a.Length
        let d1 = Array2D.length1 a.[0]
        let d3 = Array2D.length2 a.[0]
        Array3D.init d1 d2 d3 (fun i j k -> a.[j].[i,k])

    let fromDim3Slices (a : 'a [,] []) = 
        let d3 = a.Length
        let d1 = Array2D.length1 a.[0]
        let d2 = Array2D.length2 a.[0]
        Array3D.init d1 d2 d3 (fun i j k -> a.[k].[i,j])

    let rotateDim1 turns (a : 'a [,,]) = 
        let d1 = Array3D.length1 a
        Array.init d1 (fun i -> dim1Slice i a |> Array2D.rotate turns)
        |> fromDim1Slices

    let rotateDim2 turns (a : 'a [,,]) = 
        let d2 = Array3D.length2 a
        Array.init d2 (fun i -> dim2Slice i a |> Array2D.rotate turns)
        |> fromDim2Slices

    let rotateDim3 turns (a : 'a [,,]) = 
        let d3 = Array3D.length3 a
        Array.init d3 (fun i -> dim3Slice i a |> Array2D.rotate turns)
        |> fromDim3Slices

    let toString (a : 'a [,,]) = 
        let d1 = Array3D.length1 a
        let d2 = Array3D.length2 a
        let d3 = Array3D.length3 a
        [ 0 .. d1 - 1 ] 
        |> List.map (fun i -> Array2D.init d3 d2 (fun k j -> a.[i, j, k]))
        |> List.map Array2D.toString
        |> String.concat "\r\n"

