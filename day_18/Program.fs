module Input =
    open System
    open System.IO
    open Xunit 

    type Dir = U | D | L | R
    type RGB = string
    type Command = Dir * int * RGB 
    type Position = int*int
    type Wall = Position*RGB
    type Outline = Map<Position,RGB>
    type Tile = Outside | Wall | Inner 
    type Sitemap = Tile list list 

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

    let digOutline (commands: Command list): Outline =
        let rec digger (commands: Command list) (current: Position) : Wall seq =
            seq {
                match commands with
                    | [] -> () 
                    | (cmd,dist,color)::t -> let x,y = current
                                             for d in 0 .. dist do 
                                                    match cmd with
                                                    | U -> yield (x,y+d), color 
                                                    | D -> yield (x,y-d), color 
                                                    | L -> yield (x+d,y), color 
                                                    | R -> yield (x-d,y), color 
                                             yield! digger t current 
            }
        let wall = digger commands (0,0)
        Map.ofSeq wall 

    let outlineMap (outline:Outline): Sitemap =
        [ [] ]
 
    [<Fact>]
    let testOutline () =
        Assert.Equal<Outline>(Map.empty, digOutline [])
        Assert.Equal<Outline>(Map.ofList[(0,0),"a";(1,0),"a";(2,0),"a"], digOutline [L, 2, "a"])

    [<Fact>]
    let parseCommandTest () =
        Assert.Equal<Command>((R, 6,"(#70c710)"), parseCommand "R 6 (#70c710)")

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(14, input.Length) 

module Program = let [<EntryPoint>] main _ = 0