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
        

    // shouldCutOff (can make a smart cutoff, that counts how many steps ther are as well, to make
    // make sure we cut searches early

    let hasVisited (cruc: CrucState) (visited: VisitedMap):bool = 
        let (col, row, dir, m) = cruc
        if Map.containsKey (col,row) visited then true
        else false

        // this must calculate the directions left etc later
    let nextDirs (cruc: CrucState) (map: Map) : CrucState list  =
        let (col, row, dir,m) = cruc
        let allDirs = match dir with
                                    | N -> [(col, row-1,W,3);(col,row+1,E,3);(col-1,row,N,m-1)]
                                    | E -> [(col-1, row,N,3); (col+1,row,S,3); (col, row+1,E,m-1)] 
                                    | S -> [(col,row-1,E,3);(col,row+1,W,3);(col+1, row,S,m-1)] 
                                    | W -> [(col-1,row,N,3);(col+1,row,S,3);(col,row-1,W,m-1)]
        let maxMapCol, maxMapRow  = map.Length, map[0].Length
        allDirs |> List.filter (fun (col, row,_,m) -> col > 0 && col < maxMapCol && row > 0 && row < maxMapRow && m > 0 )


    // naive visited to begin with, can add that logic too
    let calcMinimalPaths (initialCutOff: int) (map: Map) : int =
        let maxMapCol, maxMapRow  = map.Length - 1, map[0].Length - 1
        
        let rec findMinPathInner (currentCost: int) (cutoff: int) (visited:VisitedMap) (cruc: CrucState) : int seq=
            seq {
                match cruc with 
                    | (col, row, _, m) when col = maxMapCol && row = maxMapRow && m <= 3 -> yield currentCost 
                    | _ -> 
                        // check if this location is in map, then yield nothing, has been seen before. 
                        // or cutoff
                        if not (hasVisited cruc visited) then 
                            let (col, row, dir,m) = cruc
                            let visited' = Map.add (col,row) dir visited // any point in updated visited for those we do not visit?
                            if (currentCost < cutoff) then 
                               let nextDirs = nextDirs cruc map 
                               for next in nextDirs do
                                    let (nextCol, nextRow,d,m) = next 
                                    let costOfNext = map[nextCol][nextRow]
                                    yield! findMinPathInner (currentCost + costOfNext) cutoff visited' next 
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