open MathNet.Numerics.LinearAlgebra.Double
open System.Collections.Generic


module Program =
    open System
    open System.IO
    open Xunit 

    type Map = int [] [] 
    let readInit (filePath: string): Map =  
        File.ReadAllLines filePath
            |> Array.map (fun l -> l.ToCharArray() |> Array.map (fun c -> int (c.ToString())))

    type Dir = N | S | E | W 

    type CrucState = int*int*Dir*int
    
    type Visit = { NrLeft: int; Cost: int } 
    type VisitedMap = Dictionary<int*int*int,Visit list>

    let hasVisitedCheaper (currentCost: int) (cruc: CrucState) (visited: VisitedMap):bool = 
        let (col,row,_,m) = cruc 
        let (found, value) = visited.TryGetValue((col,row,m))
        if (found) then
            value |> List.exists (fun c -> c.Cost < currentCost && c.NrLeft >= m) // if something exists that is cheaper with as many steps left
        else
            false

    let nextDirs (cruc: CrucState) (map: Map) : CrucState list  =
        let (col, row,dir, m) = cruc
        let allDirs = match dir with
                                    // left right straigth
                                    | S -> [(col,  row+1,E,3);(col,  row-1,W,3); (col+1,row,S,m-1)] 
                                    | E -> [(col-1,row,N,3)  ;(col+1,row,S,3)  ; (col,  row+1,E,m-1)] 
                                    | N -> [(col,  row-1,W,3);(col,  row+1,E,3); (col-1,row,N,m-1)]
                                    | W -> [(col+1,row,S,3)  ;(col-1,row,N,3)  ; (col,  row-1,W,m-1)]
        let maxMapCol, maxMapRow  = map.Length, map[0].Length
        allDirs |> List.filter (fun (col, row,_,m) -> col >= 0 && col < maxMapCol && row >= 0 && row < maxMapRow && m >=1 ) // should make it go left and right

    let updateVisitMap (c: CrucState) (thisVisit: Visit) (visits: VisitedMap) =
        let (col,row,dir,m) = c
        let (found, value) = visits.TryGetValue((col,row,m))
        if found then
            let cheaperButFewerStepsLeft = value |> List.filter (fun l -> l.Cost < thisVisit.Cost && l.NrLeft > thisVisit.NrLeft)
            visits[(col,row,m)] <- (cheaperButFewerStepsLeft @ [thisVisit]) 
        else
            visits.Add((col,row,m), [thisVisit]) 

    let calcMinimalPaths (map: Map) : int =
        let maxMapCol, maxMapRow  = map.Length - 1, map[0].Length - 1
        let visited = Dictionary()
        let visitStack = new PriorityQueue<CrucState, int>()

        let findMinPathInner () : int =
            seq {
                while (visitStack.Count > 0) do
                    let _, current, priority = visitStack.TryDequeue()
                    let col, row, d , m = current 
                    if col = maxMapCol && row = maxMapRow then 
                       yield priority 
                    else
                        let neighbors = nextDirs current map 
                        let notSeenCheaper = neighbors |> List.filter (fun (col,row,d,m) -> not (hasVisitedCheaper (priority+map[col][row]) (col,row,d,m) visited) )
                        for neighbor in notSeenCheaper do
                            let (col,row,d,m)  = neighbor
                            let nextCost = map[col][row] + priority 
                            let visit = { NrLeft = m; Cost = nextCost}
                            visitStack.Enqueue((col,row,d,m), nextCost)
                            updateVisitMap current visit visited 
            } |> Seq.head
        visitStack.Enqueue( (0,0,E,3),0 )
        findMinPathInner () 

    [<Fact>]
    let pathTest () = 
        let map = readInit "testinput.txt" 
        Assert.Equal(102, calcMinimalPaths map)

    [<Fact>]
    let pathTestReal () = 
        let map = readInit "input2.txt" 
        Assert.Equal(924, calcMinimalPaths map)

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(13, input.Length) 
        Assert.Equal(13, input[0].Length) 
        Assert.Equal(2, input[0][0]) 
        Assert.Equal(3, input[12][12]) 
        Assert.Equal(4, input[0][1]) 

    let [<EntryPoint>] main _ =
 //       let map = readInit "input2.txt" 
        let map = readInit "input2.txt" 
        
        printfn "%A cost" (calcMinimalPaths map)
        Console.ReadKey()
        0

