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
                    

    [<Fact>]
    let test2 () = 
        let instr, map = readInit "input.txt" 
        Assert.Equal(714, map.Count) 

module Program = let [<EntryPoint>] main _ = 0