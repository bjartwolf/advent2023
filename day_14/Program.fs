open MathNet.Numerics.LinearAlgebra
open System

module Program =
    open System.IO
    open Xunit 

    let square = 1.0
    let round = 2.0
    let space = 3.0

    type Matrix = float [] [] 

    let readMatrix (filePath: string): Matrix = 
        let lines = File.ReadAllLines filePath 
        let parseChar c = if c = '.' then space 
                          else if c = '#' then square
                          else if c = 'O' then round
                          else failwith "whoopsy"

        lines |> Array.map (fun l -> l.ToCharArray()) 
              |> Array.map (Array.map parseChar)
              |> DenseMatrix.ofColumnArrays
              |> Matrix.toRowArrays

    let splitRowAtRocks (input: float []) : Matrix =
        let mutable tmpGroup: float list = []
        let mutable readingGroup = false

        [| for i = 0 to Array.length input - 1 do
              let current = input[i]
              if (not readingGroup) then
                tmpGroup <- [current] 
                readingGroup <- true
              else if (readingGroup && current = square) then
                yield tmpGroup |> List.toArray
                tmpGroup <- [square] 
              else if (readingGroup && current <> square) then
                tmpGroup <- tmpGroup @ [current]
              if i = Array.length input - 1 then
                yield tmpGroup |> List.toArray
        |] 

    let sortRowAndJoin (splitRow: Matrix ) =
        splitRow |> Array.map Array.sort
                 |> Array.collect id
    
    let splitAndSort input =  
        splitRowAtRocks input
            |> sortRowAndJoin
 
    let splitAndSortMatrixN (input: Matrix) : Matrix = 
       input |> Array.map splitAndSort

    let transpose (input: Matrix): Matrix = 
        input |> Seq.transpose |> Seq.toArray |> Array.map Seq.toArray

    // rotate 90 degrees clockwise
    let rotate90C(matrix: Matrix) =
        matrix |> Array.map Array.rev |> transpose

    let prettyPrintMatrix (matrix: Matrix) =
        let matrix = DenseMatrix.ofRowArrays (matrix |> Array.map (Array.map float))
                         |> Matrix.transpose 
        for row in matrix.EnumerateRows() do
            for char in row do
                printf (match char with 
                         | 1.0 -> "#"
                         | 2.0 -> "O"
                         | 3.0 -> ".")
            printfn "" 

    let rotateAndSortCycle (input: Matrix): Matrix =
        let sortAndRotate = splitAndSortMatrixN >> rotate90C
        input |> sortAndRotate 
              |> sortAndRotate 
              |> sortAndRotate 
              |> sortAndRotate 

    let rotateAndSortN (max: int) (matrix: Matrix)  =
        let rec rotateInner (i: int) (mtrx: Matrix) (seenBefore: Map<Matrix,int>) =
            if i = max then 
                mtrx 
            else 
                let nextMatrix = rotateAndSortCycle mtrx 
                if Map.containsKey nextMatrix seenBefore then
                    let lastIterationWithMap = Map.find nextMatrix seenBefore
                    // we know that we saw this on iteration X. That means we can just skip ahead, this part I stole from Einarwh
                    let cycle = lastIterationWithMap - i 
                    let distanceToGoal = max - i
                    Map.findKey (fun _ i -> i = (distanceToGoal % cycle) + lastIterationWithMap - 1) seenBefore
                else 
                    rotateInner (i+1) nextMatrix (Map.add nextMatrix i seenBefore)
        rotateInner 0 matrix Map.empty

    let calcLoad (input: float []): float =
        input |> Array.rev
              |> Array.mapi (fun i elem -> if elem = round then float i + 1.0 else 0.0)
              |> Array.sum

    [<Fact>]
    let cycletest () = 
        let input = readMatrix "testinput.txt" 
        let sum = input |> splitAndSortMatrixN 
                        |> rotateAndSortN 1000000000
                        |> Array.map calcLoad 
                        |> Array.sum
        Assert.Equal(64, int sum) 

    [<Fact>]
    let cycle () = 
        let input = readMatrix "input.txt" 
        let sum = input |> splitAndSortMatrixN 
                        |> rotateAndSortN 1000000000
                        |> Array.map calcLoad 
                        |> Array.sum
        Assert.Equal(102657, int sum) 

    [<Fact>]
    let spintest () = 
        let input = readMatrix "testinput.txt" 
        prettyPrintMatrix (input |> transpose)
        let sum = input |> splitAndSortMatrixN 
                        |> Array.map calcLoad 
                        |> Array.sum
        Assert.Equal(136, int sum) 

    [<Fact>]
    let spin () = 
        let input = readMatrix "input.txt" 
        let sum = input |> splitAndSortMatrixN
                        |> Array.map calcLoad
                        |> Array.sum
        Assert.Equal(109638, int sum) 

    let [<EntryPoint>] main _ = Console.ReadKey() |> ignore
                                0
