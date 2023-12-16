open FSharp.Collections.ParallelSeq


module Program =
    open System
    open System.IO
    open Xunit 

    let Empty = '.'
    let VerticalSplit = '|'
    let HorizontalSplit = '-'
    let Mirror1 = '/'
    let Mirror2 = '\\'

    type Map = char list list
    let readInit (filePath: string): Map =  
        File.ReadAllLines filePath
            |> Array.toList
            |> List.map (fun x -> x.ToCharArray() |> Array.toList)

    
    // alle posisjoner i y, der y er negativ akse og x
    type Position = int*int // kan lage record 
    type Positions = (int*int) list 
    type Beam = int*int*int
    type Beams = Beam list

    let move ((x,y,d): Beam): Beam =
        match d with 
            | 0 -> (x-1,y,d) 
            | 1 -> (x,y+1,d) 
            | 2 -> (x+1,y, d) 
            | 3 -> (x,y-1, d) 

    // N = 0, E = 1, S = 2, W=3 
    let lookup (m:Map) ((y,x): Position) : Char option =
        if (y < 0 || y > m.Length - 1 || x < 0 || x > m[0].Length - 1) then
           None
        else
           Some (m[y][x])

    let eval (map:Map) (beam: Beam): Beam list =
        let x,y,d = beam
        let p = (x,y)
        let tile = lookup map p 
        match tile with
            | None -> []
            | Some tile -> match tile with 
                                | '.' -> [beam]
                                | '-' when d = 1 || d = 3 -> [beam]
                                | '-' when d = 2 || d = 0 -> [(x,y,1);(x,y,3)]
                                | '|' when d = 2 || d = 0 -> [beam]
                                | '|' when d = 1 || d = 3 -> [(x,y,2);(x,y,0)]
                                | '/' when d = 1 -> [(x,y,0)]
                                | '/' when d = 0 -> [(x,y,1)]
                                | '/' when d = 3 -> [(x,y,2)]
                                | '/' when d = 2 -> [(x,y,3)]
                                | '\\' when d = 2 -> [(x,y,1)]
                                | '\\' when d = 0 -> [(x,y,3)]
                                | '\\' when d = 3 -> [(x,y,0)]
                                | '\\' when d = 1 -> [(x,y,2)]
                                | _ -> failwith "No such position"

    let runSim (map: Map) (initialBeam: Beam)  : int=
        let mapEval = eval map 
        
        let rec innerSim (beams: Set<Beam>) (history: Set<Beam>): Set<Beam> =
            if Set.isEmpty beams then 
                history 
            else 
               let movedBeams = beams |> Set.map move 
               let evaledBeams = movedBeams |> Set.map mapEval |> Seq.collect id |> Set.ofSeq
               printfn "Evaled %A history %A" evaledBeams.Count history.Count
               let newBeams = Set.difference evaledBeams history 
               innerSim newBeams (Set.union history newBeams) 
        let firstEval = mapEval initialBeam |> Set.ofList
        let beams = innerSim firstEval firstEval 
        beams |> Set.map (fun (x,y,d) -> (x,y)) |> Set.count


    let findCombos (map: Map): Beam list =
        let max = map.Length - 1
        let size = [0 .. max]
        [
            for x in size do
                yield (0,x,2)
                yield (max,x,0)
                yield (x,0,1)
                yield (x,max,3)
        ]

    let findMaxCombo (map: Map) : int = 
        let combos = findCombos map 
        [
            for combo in combos do
                let count = runSim map combo
                printfn "%A %A" count combo
                yield count
        ] |> List.max

    [<Fact>]
    let testMapMax () = 
        let map = readInit "testinput.txt" 
        let max = findMaxCombo map
        Assert.Equal(51, max)

    [<Fact>]
    let testRunMap () = 
        let map = readInit "testinput.txt" 
        let positions = runSim map (0,0,1) 
        printfn "%A" (positions )
        Assert.Equal(46, positions )
        Assert.Equal(10, map.Length) 

    [<Fact>]
    let testRunMapReal () = 
        let map = readInit "input.txt" 
        let positions = runSim map (0,0,1) 
        printfn "%A" (positions ) 
        Assert.Equal(7623, positions) 


    [<Fact>]
    let testMove() = 
        Assert.Equal<Beam>((2,1,2), move (1,1,2) )
        Assert.Equal<Beam>((0,1,0), move (1,1,0) )
 
    [<Fact>]
    let test2 () = 
        let map = readInit "testinput.txt" 
        Assert.Equal(10, map.Length) 

    let [<EntryPoint>] main _ = 
        let map = readInit "input.txt" 
        let max = findMaxCombo map 
        printfn "%A" max 
        Console.ReadKey()
        //let map = readInit "input.txt" 
        //let positions = runSim map ((0,0),E)  
        //printfn "%A" (positions |> Set.count) 
        0

