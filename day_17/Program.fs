open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double


module Input =
    open System
    open System.IO
    open Xunit 

    type Map = int [] [] 
    let readInit (filePath: string): Map =  
        File.ReadAllLines filePath
            |> Array.map (fun l -> l.ToCharArray() |> Array.map (fun c -> int (c.ToString())))

    type Dir = N | S | E | W 

    // y, x, Direction and turns forward
    // skip turns forward first and see if it easier and then add that logic
    // should find a cheaper way, that should terminate faster as everything should become expensive? Or not.

    type CrucState = int*int*Dir*int
    
    // low startofcost initially to avoid deep searches
    let initialMinCost (map: Map): int =
        let matrix = map |> Array.map (fun r -> r |> Array.map float)
                         |> DenseMatrix.OfRowArrays 
        let diag = matrix.Diagonal().MapIndexed(fun i n -> if i = 0 then 0.0 else n)
        let diag2 = matrix.RemoveColumn(0).Diagonal()
        let diag3 = matrix.RemoveRow(0).Diagonal()
        let sum1 = diag.Sum() + diag2.Sum()
        let sum2 = diag.Sum() + diag3.Sum()
        let minsum = min sum1 sum2
        int minsum

    type VisitedMap = Map<(int*int),Dir>
        
    let hasVisited (cruc: CrucState) (visited: VisitedMap):bool = 
        false

    // naive visited to begin with, can add that logic too
    let calcMinimalPaths (initialCutOff: int) (map: Map) : int =
        let maxMapCol = map.Length - 1
        let maxMapRow = map[0].Length - 1
        
        let rec findMinPathInner (currentCost: int) (cutoff: int) (visited:VisitedMap) (cruc: CrucState) : int seq=
            seq {
                match cruc with 
                    | (col, row, _, m) when col = maxMapCol && row = maxMapRow && m <= 3 -> yield currentCost 
                    | _ -> 
                        // check if this location is in map, then yield nothing, has been seen before. 
                        // or cutoff
                        if (currentCost < cutoff) && (not (hasVisited cruc visited)) then    
                           let (col, row, dir,m) = cruc
                           let nextCol = col + 1
                           let nextRow = row + 1
                           let next = (nextCol, nextRow, dir,m)
                           let costOfNext = map[nextCol][nextRow]
                           yield! findMinPathInner (currentCost + costOfNext) cutoff visited next 
                        // if not in map, then check its legal directions, only straight if
                        // not moved three can be added later...
            }

        findMinPathInner 0 initialCutOff Map.empty (0,0,E,0) |> Seq.min

    [<Fact>]
    let pathTest () = 
        let map = readInit "testinput.txt" 
        let initialCutoff = initialMinCost map 
        Assert.Equal(68, calcMinimalPaths initialCutoff map)


    [<Fact>]
    let sumsTest () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(133, initialMinCost input)


    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(13, input.Length) 
        Assert.Equal(13, input[0].Length) 
        Assert.Equal(2, input[0][0]) 
        Assert.Equal(3, input[12][12]) 
        Assert.Equal(4, input[0][1]) 

module Program = let [<EntryPoint>] main _ = 0