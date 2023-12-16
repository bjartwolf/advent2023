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

    let prettyPrint (m: Map) =
        [for line in m do
            line |> String.Concat |> printfn "%A"
        ]
    
    let (+) (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
    // alle posisjoner i y, der y er negativ akse og x
    let North = (-1,0)
    let South = (1,0)
    let East = (0,1)
    let West = (0,-1)
    type Direction = N|S|E|W
    type Position = int*int // kan lage record 
    type Positions = Position list
    type Beam = Position * Direction 
    type Beams = Beam list

    let prettyPrintPositions (m: Map) (positions: Positions) =
        printfn "***"
        [for y in 0 .. m.Length - 1 do
            for x in 0 .. m[0].Length - 1 do
                if List.contains (y,x) positions then
                    printf "X"
                else
                    printf "."
            printfn ""
        ]

    let move ((p,d): Beam): Beam =
        match d with 
            | N -> (p+North, d) 
            | E -> (p+East, d) 
            | W -> (p+West, d) 
            | S -> (p+South, d) 

    let lookup (m:Map) ((y,x): Position) : Char option =
        if (y < 0 || y > m.Length - 1 || x < 0 || x > m[0].Length - 1) then
           None
        else
           Some (m[y][x])

    let eval (map:Map) (beam: Beam): Beam list =
        let p,d = beam
        let tile = lookup map p 
        match tile with
            | None -> []
            | Some tile -> match tile with 
                                | '.' -> [beam]
                                | '-' when d = E || d = W -> [beam]
                                | '-' when d = S || d = N -> [(p,E);(p,W)]
                                | '|' when d = S || d = N -> [beam]
                                | '|' when d = E || d = W -> [(p,S);(p,N)]
                                | '/' when d = E -> [(p,N)]
                                | '/' when d = N -> [(p,E)]
                                | '/' when d = W -> [(p,S)]
                                | '/' when d = S -> [(p,W)]
                                | '\\' when d = S -> [(p,E)]
                                | '\\' when d = N -> [(p,W)]
                                | '\\' when d = W -> [(p,N)]
                                | '\\' when d = E -> [(p,S)]
                                | _ -> failwith "No such position"

    let runSim (map: Map) (initialBeam: Beam)  : Positions =
        // missing cycle detection 
        // hvis vi har sett alle beamene før er det vel en sykel...
        let mapEval = eval map 
        
        let rec innerSim (beams: Beams) (cycleMap: Beams): Beams =
            if List.isEmpty beams then 
                cycleMap 
            else 
               let movedBeams = beams |> List.map move      
               let evaledBeams = movedBeams |> List.collect mapEval |> List.distinct 
               let newBeams = evaledBeams |> List.filter (fun beam -> not (List.contains beam cycleMap)) 

               innerSim newBeams (List.distinct (cycleMap @ evaledBeams)) 
        let firstEval = mapEval initialBeam 
        let beams = innerSim firstEval firstEval 
        beams |> List.map (fun (b,d) -> b) |> List.distinct 


    let findCombos (map: Map): Beam list =
        let max = map.Length - 1
        let size = [0 .. max]
        [
            for x in size do
                yield (0,x),S
                yield (max,x),N
                yield (x,0),E 
                yield (x,max),W  
        ]

    let findMaxCombo (map: Map) : int = 
        let combos = findCombos map |> List.toArray 
        let sim (x:Beam) = runSim map x |> List.length
        combos |> PSeq.map sim |> Seq.toList |>Seq.max

    [<Fact>]
    let testMapMax () = 
        let map = readInit "testinput.txt" 
        let max = findMaxCombo map
        Assert.Equal(51, max)

    [<Fact>]
    let testRunMap () = 
        let map = readInit "testinput.txt" 
        let positions = runSim map ((0,0),E) 
        printfn "%A" (positions |> List.length)
        Assert.Equal(46, positions |> List.length)
        Assert.Equal(10, map.Length) 

    [<Fact>]
    let testRunMapReal () = 
        let map = readInit "input.txt" 
        let positions = runSim map ((0,0),E) 
        printfn "%A" (positions |> List.length) 
        Assert.Equal(7623, positions.Length) 


    [<Fact>]
    let testMove() = 
        Assert.Equal<Beam>(((2,1),S), move ((1,1),S) )
        Assert.Equal<Beam>(((0,1),N), move ((1,1),N) )
 
    [<Fact>]
    let test2 () = 
        let map = readInit "testinput.txt" 
        prettyPrint map |> ignore 
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

