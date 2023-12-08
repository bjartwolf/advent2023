module Input =
    open System
    open System.IO
    open Xunit 

    type Instruction = Left | Right
    type Instructions = Instruction list

    type Node = { Left: string; Right: string }  
    let readInit (filePath: string)  = 
        let lines = System.IO.File.ReadAllLines filePath 
        let instructions = 
            lines[0].ToCharArray()
                |> Array.map (fun x -> if x = 'L' then Left else if x = 'R' then Right else failwithf "No direction %A" x)
                |> Seq.toList
        

        let maps = lines[2 ..]

        instructions,maps
                    

    [<Fact>]
    let test2 () = 
        let instr, map = readInit "input.txt" 
        Assert.Equal(714, map.Length) 

module Program = let [<EntryPoint>] main _ = 0