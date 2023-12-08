module Input =
    open System
    open System.IO
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

    let lookupInstruction (instrs: Instructions) (i: int): Instruction =
        let instructionLength = instrs.Length
        instrs[i % instructionLength] 

    [<Fact>]
    let testInstructions() = 
        let instr, _= readInit "testinput1.txt" 
        let lookup = lookupInstruction instr
        Assert.Equal(Right, lookup 0)
        Assert.Equal(Left, lookup 1)
        Assert.Equal(Right, lookup 2)
        Assert.Equal(Left, lookup 3)
 
    let walkMapUntilEnd (desertMap:Map<string,Node>) (instructions: Instructions) : int= 
        let instructionLength = instructions.Length
        let lookup = lookupInstruction instructions
        let rec walkMapInner (location:string) (i:int) = // watch for overflows.... 
            if location = finish then i
            else 
                let currentInstruction = lookup i 
                let currentSelection = desertMap[location]
                let nextLocation  = 
                    match currentInstruction with
                        | Left -> currentSelection.Left 
                        | Right -> currentSelection.Right 
                walkMapInner nextLocation (i+1)
        walkMapInner start 0
    
    [<Fact>]
    let testFirstMap () = 
        let instr, map = readInit "testinput1.txt" 
        let step = walkMapUntilEnd map instr 
        Assert.Equal(2, step) 

    [<Fact>]
    let testSecondMap() = 
        let instr, map = readInit "testinput2.txt" 
        let step = walkMapUntilEnd map instr 
        Assert.Equal(6, step) 
                     
    [<Fact>]
    let tesINput() = 
        let instr, map = readInit "input.txt" 
        let step = walkMapUntilEnd map instr 
        Assert.Equal(14893, step) 

    [<Fact>]
    let test2 () = 
        let instr, map = readInit "input.txt" 
        Assert.Equal(714, map.Count) 

module Program = let [<EntryPoint>] main _ = 0