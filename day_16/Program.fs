module Input =
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
                if List.exists (fun p -> p = (y,x)) positions then
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

    let runSim (map: Map) : Positions =
        // missing cycle detection 
        // hvis vi har sett alle beamene før er det vel en sykel...
        let mapEval = eval map
        let initialBeam = ((0,0),E)
        let rec innerSim (beams: Beams) (positions: Positions) (cycleMap: Set<Position> list): Beams*Positions =
            match beams with
                | [] -> beams, positions 
                | beams -> 
                           let movedBeams = beams |> List.map move      
                           let evaledBeams = movedBeams |> List.collect mapEval |> List.distinct
                           printfn "%A" evaledBeams
                           //if evaledBeams.IsEmpty then (beams, positions)
                           let prevPositions = beams |> List.map (fun (b,_) -> b)  // eval eats the empty ones. 

                           let currentBeamSet = prevPositions |> Set.ofList
                           // check if in cyclemap
                           if (List.contains currentBeamSet cycleMap) then  
                                beams, positions @ prevPositions
                           else 
                               //prettyPrintPositions map positions |> ignore 
                               //printfn "%A" evaledBeams
                               innerSim evaledBeams (positions @ prevPositions) (cycleMap @ [currentBeamSet]) 
        let firstEval = mapEval initialBeam
        innerSim firstEval [] [Set.empty] |> snd 

    [<Fact>]
    let testRunMap () = 
        let map = readInit "testinput.txt" 
        let positions = runSim map  
        printfn "%A" (positions |> List.distinct |> List.length)
        Assert.Equal(46, positions |> List.distinct |> List.length)
        Assert.Equal(10, map.Length) 

    [<Fact>]
    let testRunMapReal () = 
        let map = readInit "input.txt" 
        let positions = runSim map  
        printfn "%A" (positions |> List.distinct |> List.length)
        Assert.Equal(110, map.Length) 


    [<Fact>]
    let testMove() = 
        Assert.Equal<Beam>(((2,1),S), move ((1,1),S) )
        Assert.Equal<Beam>(((0,1),N), move ((1,1),N) )
 
    [<Fact>]
    let test2 () = 
        let map = readInit "testinput.txt" 
        prettyPrint map |> ignore 
        Assert.Equal(10, map.Length) 

module Program = let [<EntryPoint>] main _ = 0