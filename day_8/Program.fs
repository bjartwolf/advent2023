module Program =
    open System
    open Xunit 

    type Instruction = Left | Right
    type Instructions = Instruction list

    let parseLineToStr (str: string) =
        let a = str.Substring(0,3) 
        let b = str.Substring(7,3) 
        let c = str.Substring(12,3) 
        (a,b,c)
    
    [<Fact>]
    let mapLine() =
        let ex = "QKX = (SQD, XTJ)"
        Assert.Equal<(string*string*string)> (("QKX","SQD","XTJ"),parseLineToStr ex)

    type Node = { Left: string; Right: string }  

    let parseMapToInput (input: string*string*string) : (string*Node) =
        let key, l,r = input
        (key, {Left= l; Right=r})

    [<Fact>]
    let toMapInput() =
        let ex = ("QKX","SQD","XTJ")
        Assert.Equal<(string*Node)> (("QKX",{Left = "SQD";Right ="XTJ" }), parseMapToInput ex)

    
    let readInit (filePath: string)  = 
        let lines = System.IO.File.ReadAllLines filePath 
        let instructions = 
            lines[0].ToCharArray()
                |> Array.map (fun x -> if x = 'L' then Left else if x = 'R' then Right else failwithf "No direction %A" x)
                |> Seq.toList
        

        let maps = lines[2 ..]
                    |> Array.map parseLineToStr
                    |> Array.map parseMapToInput 
                    |> Map.ofArray
            
        instructions,maps

    let start = "AAA"
    let finish = "ZZZ"

    let isStartNode (node: string) : bool = 
        node[2] = 'A' 
        
    [<Fact>]
    let isStartNodeTests() = 
        Assert.Equal(true, isStartNode "11A")
        Assert.Equal(true, isStartNode "22A")
        Assert.Equal(false, isStartNode "A22")
        Assert.Equal(false, isStartNode "BBB")
 
    let isEndNode (node: string) : bool = 
        node[2] = 'Z' 
        
    [<Fact>]
    let isEndNodeTests() = 
        Assert.Equal(false, isEndNode "ZZA")
        Assert.Equal(true, isEndNode "ZZZ")

    let areAllEndNodes (nodes: string list): bool =
        nodes |> List.forall isEndNode

    [<Fact>]
    let AreAllEndNodeTests() = 
        Assert.Equal(false, areAllEndNodes ["ZZA";"ZZZ"])
        Assert.Equal(true, areAllEndNodes ["ZZZ";"ZZZ"])
        Assert.Equal(true, areAllEndNodes ["11Z";"22Z"])
        Assert.Equal(false, areAllEndNodes ["11Z";"22A"])


    let getAllStartNodes (nodes: string list): string list = 
        nodes |> List.where isStartNode 

    [<Fact>]
    let getAllStartNodesTest() = 
        Assert.Equal<string list>(["11A"], getAllStartNodes ["11A"])
        Assert.Equal<string list>(["22A"], getAllStartNodes ["22A"])
        Assert.Equal<string list>(["11A";"22A"], getAllStartNodes ["11A";"BBB";"22A"])
        Assert.Equal<string list>([], getAllStartNodes ["A22"])
 
 
    let lookupInstruction (instrs: Instructions) (i: int64): Instruction =
        let instructionLength = int64 instrs.Length
        let lookup = int(i % instructionLength)
        instrs[lookup] 

    [<Fact>]
    let testInstructions() = 
        let instr, _= readInit "testinput2.txt" 
        let lookup = lookupInstruction instr
        Assert.Equal(Left, lookup 0L)
        Assert.Equal(Left, lookup 1L)
        Assert.Equal(Right, lookup 2L)
        Assert.Equal(Left, lookup 3L)
        Assert.Equal(Left, lookup 4L)
        Assert.Equal(Right, lookup 5L)
        Assert.Equal(Left, lookup 6L)
        Assert.Equal(Left, lookup 7L)
        Assert.Equal(Right, lookup 8L)

    let walkMapUntilEndOne startNode (desertMap:Map<string,Node>) (instructions: Instructions) : int64= 
        let lookup = lookupInstruction instructions
        let rec walkMapInner (locations:string list) (i:int64) = 
            if (areAllEndNodes locations) then i
            else 
                let currentInstruction = lookup i 
                let currentSelections = locations |> List.map (fun l -> desertMap[l])
                let nextLocations  =  currentSelections |> List.map (fun n -> 
                    match currentInstruction with
                        | Left -> n.Left 
                        | Right -> n.Right )
 //               printfn "You are at %A" locations 
//                printfn "You choose all of the %A paths leading you to %A" currentInstruction nextLocations 
                walkMapInner nextLocations (i+1L)
        walkMapInner startNode 0L

    let walkMapFirst (desertMap: Map<string,Node>) (instructions: Instructions)  =
        let startNodes = (getAllStartNodes (desertMap.Keys |> Seq.toList))
        let firstNode = [startNodes[0]]
        walkMapUntilEndOne firstNode desertMap instructions

    let walkEntireMap (desertMap: Map<string,Node>) (instructions: Instructions) : int64 list =
        let startNodes = (getAllStartNodes (desertMap.Keys |> Seq.toList))
        let allDistances = startNodes |> List.map (fun x ->  walkMapUntilEndOne [x] desertMap instructions)
        allDistances 

    [<Fact>]
    let testfirststep () = 
        let instr, map = readInit "input.txt" 
        let foo = walkMapFirst map instr 
        Assert.Equal(14893L, foo) 

    [<Fact>]
    let testalll() = 
        let instr, map = readInit "input.txt" 
        let startNodes = (getAllStartNodes (map.Keys |> Seq.toList))
        let foo = walkEntireMap map instr 
        Assert.Equal<int64 list>([14893L;16579L;12083L;13207L;22199L;20513L], foo) 




    //[<Fact>]
    //let testSecondMap() = 
    //    let instr, map = readInit "testinput3.txt" 
    //    let step = walkMapUntilEnd map instr 
    //    Assert.Equal(6L, step) 
 (*
    [<Fact>]
    let tesINput() = 
        let instr, map = readInit "input.txt" 
        let step = walkMapUntilEnd map instr 
        Assert.Equal(14893, step) 
*)
    //[<Fact>]
    //let test2 () = 
    //    let instr, map = readInit "input.txt" 
    //    Assert.Equal(714, map.Count) 

    let [<EntryPoint>] main _ = 
                let lcm x y =
                    let rec gcd (x : int64) (y : int64) = 
                        if y = 0 then abs x else gcd y (x % y)
                    x * y / (gcd x y)
                let foo = [14893L;16579L;12083L;13207L;22199L;20513L]
                foo |> Seq.reduce lcm |> printfn "%A"
                Console.ReadKey() |> ignore
                0