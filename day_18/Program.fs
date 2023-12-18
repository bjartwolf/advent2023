module Input =
    open System
    open System.IO
    open Xunit 

    type Dir = U | D | L | R
    type RGB = string
    type Command = Dir * int * RGB 
    type Position = int*int
    type Wall = Position*RGB
    type OutLine = Map<Position,RGB>

    let parseCommand (cmdString: string): Command = 
        let parts = cmdString.Split(" ")
        let cmd = match parts[0] with 
                        |"L" -> L
                        |"R"-> R
                        |"U"-> U
                        |"D"-> D
                        | x -> failwithf "ouch %A %A" x parts[0]
        let steps = int parts[1]
        cmd,steps,parts[2]

    let readInit (filePath: string): Command list = 
        File.ReadAllLines filePath
            |> Array.toList
            |> List.map parseCommand

    let digOutline (commands: Command list): OutLine =
        let rec digger (commands: Command list) (current: Position) : Wall seq =
            seq {
                match commands with
                    | [] -> () 
                    | (a,b,c)::t -> yield current,c
                                    yield! digger t current 
            }
        let wall = digger commands (0,0)
        Map.ofSeq wall 
 
    [<Fact>]
    let testOutline () =
        Assert.Equal<OutLine>(Map.empty, digOutline [])
        Assert.Equal<OutLine>(Map.ofList[(0,0),"a";(1,0),"a";(2,0),"a"], digOutline [R 3 "a"])

    [<Fact>]
    let parseCommandTest () =
        Assert.Equal<Command>((R, 6,"(#70c710)"), parseCommand "R 6 (#70c710)")

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(14, input.Length) 

module Program = let [<EntryPoint>] main _ = 0