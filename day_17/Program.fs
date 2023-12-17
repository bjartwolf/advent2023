open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double


module Program =
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

    // need to add cost and m to this
    // the direction, the number of straight moves left and the totalCost (should return the totalcost and just
    // not visit it if the cost is higher_
    // store the location, direction and number of moves left
    // only cheaper if there are more moves left or some combination with cost...
        

    // shouldCutOff (can make a smart cutoff, that counts how many steps ther are as well, to make
    // make sure we cut searches early

    // billigere OG med færre steg er billigere, så bør kanskje sjekke alle
    // oppslag og kost med antall flytt 
    type Visit = { NrLeft: int; Cost: int } 
    type VisitedMap = Map<int*int*Dir,Visit list>

    let hasVisitedCheaper (currentCost: int) (cruc: CrucState) (visited: VisitedMap):bool = 
        let (col,row,Dir,m) = cruc 
        let visits = Map.tryFind (col,row,Dir) visited
        match visits with 
            | Some costs -> costs |> List.exists (fun c -> c.Cost < currentCost && c.NrLeft >= m)
            | None -> false

        // this must calculate the directions left etc later
    let nextDirs (cruc: CrucState) (map: Map) : CrucState list  =
        let (col, row, dir,m) = cruc
        let allDirs = match dir with
                                    | N -> [(col, row-1,W,3);(col,row+1,E,3);(col-1,row,N,m-1)]
                                    | E -> [(col-1, row,N,3); (col+1,row,S,3); (col, row+1,E,m-1)] 
                                    | S -> [(col,row-1,E,3);(col,row+1,W,3);(col+1, row,S,m-1)] 
                                    | W -> [(col-1,row,N,3);(col+1,row,S,3);(col,row-1,W,m-1)]
        let maxMapCol, maxMapRow  = map.Length, map[0].Length
        allDirs |> List.filter (fun (col, row,_,m) -> col >= 0 && col < maxMapCol && row >= 0 && row < maxMapRow && m > 0 )

    let updateVisitMap (c: CrucState) (thisVisit: Visit) (visits: VisitedMap) : VisitedMap =
        let (col,row,dir,m) = c
        let visitedEntry = Map.find (col,row,dir) visits
        let filtered = (visitedEntry@ [thisVisit]) |> List.filter (fun l -> l.Cost <= thisVisit.Cost && l.NrLeft >= thisVisit.NrLeft)
        Map.add (col,row,dir) filtered visits

    [<Fact>]
    let nextDirText () = 
        let map = readInit "testinput.txt" 
        Assert.Equal<CrucState list>([(1,0,S,3);(0,1,E,2)], nextDirs (0,0,E,3) map)

    // naive visited to begin with, can add that logic too
    let calcMinimalPaths (initialCutOff: int) (map: Map) : int seq =
        let maxMapCol, maxMapRow  = map.Length - 1, map[0].Length - 1
        let mutable cutAt = initialCutOff 
        let rec findMinPathInner (currentCost: int) (visited:VisitedMap) (cruc: CrucState) : int seq=
            seq {
                match cruc with 
                    | (col, row, _, m) when col = maxMapCol && row = maxMapRow && m <= 3 -> 
                        if currentCost < cutAt then
                            cutAt <- currentCost
                            yield currentCost 
                    | _ -> 
                        if not (hasVisitedCheaper currentCost cruc visited) then  // må sammenligne med kosten å gå dit? eller spilller det ingen rolle for den er samme for alle
                            let (col, row, dir,m) = cruc
                            // must update the entire list too...
                            let visit = { NrLeft = m; Cost = currentCost}
                            let visited' = updateVisitMap cruc visit visited
                            if (currentCost < cutAt ) then 
                               let nextDirs = nextDirs cruc map 
                               for next in nextDirs do
                                    let (nextCol, nextRow,d,m) = next 
                                    let costOfNext = map[nextCol][nextRow]
                                    yield! findMinPathInner (currentCost + costOfNext) visited' next 
            }
        findMinPathInner 0 Map.empty (0,0,E,3) 

    [<Fact>]
    let pathTest () = 
        let map = readInit "testinput.txt" 
        let initialCutoff = initialMinCost map 
        Assert.Equal(68, calcMinimalPaths initialCutoff map |> Seq.min)


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

    let [<EntryPoint>] main _ =
        let map = readInit "testinput.txt" 
        let initialCutoff = initialMinCost map 
        for cost in calcMinimalPaths initialCutoff map do
            printfn "%A cost" cost
        0

