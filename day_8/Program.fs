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

    let getAllStartNodes (nodes: string list): string list = 
        nodes |> List.where isStartNode 

    [<Fact>]
    let getAllStartNodesTest() = 
        Assert.Equal<string list>(["11A"], getAllStartNodes ["11A"])
        Assert.Equal<string list>(["22A"], getAllStartNodes ["22A"])
        Assert.Equal<string list>(["11A";"22A"], getAllStartNodes ["11A";"BBB";"22A"])
        Assert.Equal<string list>([], getAllStartNodes ["A22"])
 
    [<Fact>]
    let AreAllEndNodeTests() = 
        Assert.Equal(false, areAllEndNodes ["ZZA";"ZZZ"])
        Assert.Equal(true, areAllEndNodes ["ZZZ";"ZZZ"])

 
    let lookupInstruction (instrs: Instructions) (i: int): Instruction =
        let instructionLength = instrs.Length
        instrs[i % instructionLength] 

    [<Fact>]
    let testInstructions() = 
        let instr, _= readInit "testinput2.txt" 
        let lookup = lookupInstruction instr
        Assert.Equal(Left, lookup 0)
        Assert.Equal(Left, lookup 1)
        Assert.Equal(Right, lookup 2)
        Assert.Equal(Left, lookup 3)
        Assert.Equal(Left, lookup 4)
        Assert.Equal(Right, lookup 5)
        Assert.Equal(Left, lookup 6)
        Assert.Equal(Left, lookup 7)
        Assert.Equal(Right, lookup 8)


    let walkMapUntilEnd (desertMap:Map<string,Node>) (instructions: Instructions) : int= 
        let lookup = lookupInstruction instructions
        let rec walkMapInner (locations:string list) (i:int) = 
            if areAllEndNodes locations then i
            else 
                let currentInstruction = lookup i 
                let currentSelections = locations |> List.map (fun l -> desertMap[l])
                let nextLocations  =  currentSelections |> List.map (fun n -> 
                    match currentInstruction with
                        | Left -> n.Left 
                        | Right -> n.Right )
//                printfn "You choose all of the %A paths leading you to %A" currentInstruction nextLocations 
                walkMapInner nextLocations (i+1)
        walkMapInner (getAllStartNodes (desertMap.Keys |> Seq.toList)) 0

    [<Fact>]
    let testSecondMap() = 
        let instr, map = readInit "testinput3.txt" 
        let step = walkMapUntilEnd map instr 
        Assert.Equal(6, step) 
                     
 (*
    [<Fact>]
    let tesINput() = 
        let instr, map = readInit "input.txt" 
        let step = walkMapUntilEnd map instr 
        Assert.Equal(14893, step) 
*)
    [<Fact>]
    let test2 () = 
        let instr, map = readInit "input.txt" 
        Assert.Equal(714, map.Count) 

    let [<EntryPoint>] main _ = 
            let instr, map = readInit "input.txt" 
//            let instr, map = readInit "testinput3.txt" 
            let steps = walkMapUntilEnd map instr 
            printfn "Steps: %A" steps
            Console.ReadKey() |> ignore
            0