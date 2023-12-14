open MathNet.Numerics.LinearAlgebra
open System

module Program =
    open System.IO
    open Xunit 

    let square = 1
    let round = 2
    let space = 3

    type Matrix = int [] [] 

    let readMatrix (filePath: string): Matrix = 
        let lines = File.ReadAllLines filePath 
        let lengthOfLine = lines[0].Length
        [|
            for i in 0 .. lengthOfLine - 1 do
                let col = lines |> Array.map (fun x -> x.[i])
                                |> Array.map (fun c -> if c = '.' then space 
                                                       else if c = '#' then square
                                                       else if c = 'O' then round
                                                       else failwith "whoopsy")
                yield col 
        |]

    let splitRowAtRocks (input: int []) : Matrix =
        let mutable tmpGroup: int list = []
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
        DenseMatrix.ofRowArrays (input |> Array.map (Array.map float))
            |> Matrix.transpose
            |> Matrix.toRowArrays
            |> Array.map (Array.map int) 

    // rotate 90 degrees clockwise
    let rotate90C(matrix: Matrix) =
        matrix |> Array.map Array.rev |> transpose

    let prettyPrintMatrix (matrix: Matrix) =
        DenseMatrix.ofRowArrays (matrix |> Array.map (Array.map float))
         |> Matrix.transpose 
         |> printfn "%A" 
        //for line in matrix do
        //    let prettyLine = line |> Array.map (fun x -> match x with 
        //                                                       | 1 -> '#'
        //                                                       | 2 -> 'O'
        //                                                       | 3 -> '.')
        //    printfn "%A" (new string(prettyLine |> List.toArray ))

    let rotateAndSortCycle (input: Matrix): Matrix =
        let north = splitAndSortMatrixN input
        let west = splitAndSortMatrixN (north |> rotate90C) 
        let south = splitAndSortMatrixN (west |> rotate90C) 
        let east = splitAndSortMatrixN (south |> rotate90C) 
        east |> rotate90C

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

    [<Fact>]
    let testAutoCycle () = 
        let input = readMatrix "testinput.txt"

        let cycle1 = readMatrix "cycle1.txt" 
        let cycle2 = readMatrix "cycle2.txt" 
        let cycle3 = readMatrix "cycle3.txt" 

        Assert.Equal<Matrix>(cycle1, input |> rotateAndSortN 1) 
        Assert.Equal<Matrix>(cycle2, input |> rotateAndSortN 2 ) 
        Assert.Equal<Matrix>(cycle3, input |> rotateAndSortN 3) 
        Assert.Equal<Matrix>(cycle3, input |> rotateAndSortN 10) 
        Assert.Equal<Matrix>(cycle3, cycle3 |> rotateAndSortN 7) 
        Assert.NotEqual<Matrix>(cycle3, cycle3 |> rotateAndSortN 8) 
 
    let calcLoad (input: int []): int =
        input |> Array.rev
              |> Array.mapi (fun i elem -> if elem = round then i + 1 else 0)
              |> Array.sum

    [<Fact>]
    let testcycletestdata () = 
        let input = readMatrix "testinput.txt" 
        let sum = input |> splitAndSortMatrixN 
                        |> rotateAndSortN 1000000000
                        |> Array.map calcLoad 
                        |> Array.sum
        Assert.Equal(64, sum) 

    [<Fact>]
    let testcycledata () = 
        let input = readMatrix "input.txt" 
        let sum = input |> splitAndSortMatrixN 
                        |> rotateAndSortN 1000000000
                        |> Array.map calcLoad 
                        |> Array.sum
        Assert.Equal(102657, sum) 

    [<Fact>]
    let test2 () = 
        let input = readMatrix "testinput.txt" 
        prettyPrintMatrix (input |> transpose)
        let sum = input |> splitAndSortMatrixN 
                        |> Array.map calcLoad 
                        |> Array.sum
        Assert.Equal(136, sum) 

    [<Fact>]
    let testprod () = 
        let input = readMatrix "input.txt" 
        let sum = input |> splitAndSortMatrixN
                        |> Array.map calcLoad
                        |> Array.sum
        Assert.Equal(109638, sum) 

    let [<EntryPoint>] main _ = Console.ReadKey() |> ignore
                                0
