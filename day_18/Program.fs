module Input =
    open System
    open System.IO
    open Xunit 

    type Dir = U | D | L | R
    type RGB = string
    type Command = Dir * int * RGB 
    type Position = int*int
    type Wall = Position*RGB
    type Outline = (Position*RGB) list
    type Tile = Outside | Wall of RGB | Inner 
    type Sitemap = (Tile) list list 

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
                                             for d in 1 .. dist do 
                                                    match cmd with
                                                    | U -> yield (x,y+d), color 
                                                    | D -> yield (x,y-d), color 
                                                    | L -> yield (x-d,y), color 
                                                    | R -> yield (x+d,y), color 
                                             let nextPos = 
                                                    match cmd with
                                                    | U -> (x,y+dist) 
                                                    | D -> (x,y-dist) 
                                                    | L -> (x-dist,y) 
                                                    | R -> (x+dist,y) 
                                             yield! digger t nextPos 
            }
        let wall = digger commands (0,0)
        List.ofSeq wall

    let getTile (walls: Wall list) x y =  
        let a = walls |> List.tryFind (fun (pos,_) -> pos = (x,y))
        match a with
            | Some (_,c)-> Wall c
            | None _ -> Outside 

    // bryr jeg meg om posisjonene er negative og sånt? kan normalisere alle til
    // 0 for enkelhetsskyld
    let outlineMap (outline:Outline): Sitemap =
        let minX = outline |> List.map (fun ((x,_),_) -> x) |> List.min
        let minY = outline |> List.map (fun ((_,y),_) -> y) |> List.min
        let normalized = outline |> List.map (fun ((x,y),c) -> (x + abs minX, y + abs minY), c)
        let maxX = normalized |> List.map (fun ((x,_),_) -> x) |> List.max
        let maxY = normalized |> List.map (fun ((_,y),_) -> y) |> List.max
        [
            for y in 0 .. maxY  do
                let xs = [0 .. maxX  ]
                let xWall = xs |> List.map (fun x -> getTile normalized x y)
                yield xWall
        ]
    let prettyPrint (map: Sitemap)  =
        for line in (map |> List.rev) do
            for tile in line do
                match tile with
                   | Outside -> printf "o" 
                   | Wall _ -> printf "X"
                   | Inner -> printf "."
            printfn ""

    let countInner (tiles: Tile list) : int =
        [ for x in 0 .. tiles.Length - 1 do
            match tiles[x] with 
                | Outside | Inner -> let before= tiles.[0 .. x] 
                                     let wallsPassed = before |> List.filter (fun x -> match x with 
                                                                                    | Wall _ -> true
                                                                                    | _ -> false ) 
                                     let wallCount = wallsPassed |> List.distinct
                                                                 |> List.length// hope we cross distinct colored walls
                                     printfn "%A WallCount: %A before %A" x wallCount wallsPassed
                                     if (wallCount % 2) = 0 then yield 0 
                                     else yield 1
                | Wall _ -> yield 1 
        ] |> List.sum
        

    let rec findArea (map: Sitemap) : int =
        [
            for line in map do
               countInner line 
        ] |> List.sum

    [<Fact>]
    let findAreaTest () =
        let cmds = readInit "testinput.txt" 
        let outline = digOutline cmds
        let map = outlineMap outline
        let cnt = findArea map
        Assert.Equal(62, cnt) 
                    
                    
 
    [<Fact>]
    let getWallTest () =
        let cmds = readInit "testinput.txt" 
        let outline = digOutline cmds
        let map = outlineMap outline
        prettyPrint map
        Assert.Equal(10, map.Length)
        Assert.Equal(7, map[0].Length)
        //printfn "%A" map
  
    [<Fact>]
    let testOutline () =
        Assert.Equal<Outline>([], digOutline [])
        Assert.Equal<Outline>([(0,0),"a";(1,0),"a";(2,0),"a"], digOutline [L, 2, "a"])

    [<Fact>]
    let parseCommandTest () =
        Assert.Equal<Command>((R, 6,"(#70c710)"), parseCommand "R 6 (#70c710)")

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(14, input.Length) 

module Program = let [<EntryPoint>] main _ = 0