open System.Drawing


module Proga=
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

    let prettyPrintFile (map: Sitemap) (f:string)=
        use sw = new StringWriter()
        for line in (map |> List.rev) do
            for tile in line do
                match tile with
                   | Outside -> sw.Write "o" 
                   | Wall _ -> sw.Write "X"
                   | Inner -> sw.Write "."
            sw.Write(Environment.NewLine) 
        File.WriteAllText(f, sw.ToString())

    //let parseColor (colorString: string) : 
    let prettyPrintBmp (map: Sitemap) (f:string)=
        let width = map.Head.Length
        let height = map.Length

        use bitmap = new Bitmap(width, height)


        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                let color = match map[y][x] with
                                     | Wall (c:string) ->  ColorTranslator.FromHtml(c.Replace("(","").Replace(")","" ) )
                                     | _ ->  ColorTranslator.FromHtml("#FFFFFF")
                bitmap.SetPixel(x, y, color)
        bitmap.Save(f)
      
    let rec simplifyWalls (str: string) : string =
        if (str.Contains("xx")) then
            simplifyWalls (str.Replace("xx","x"))
        else str

    [<Fact>]
    let testSimplifyWalls() =
        Assert.Equal("x00x0x", simplifyWalls "xxx00xx0x" ) 
        Assert.Equal("x", simplifyWalls "xx" ) 
        Assert.Equal("x", simplifyWalls "x" ) 

    let countWalls (str: string) : int =
        simplifyWalls str
            |> Seq.filter (fun c -> c = 'x')
            |> Seq.length

    [<Fact>]
    let countWallsTest() =
        Assert.Equal(3, countWalls "xxx00xx0x" ) 
        Assert.Equal(1, countWalls "xx" ) 
        Assert.Equal(1, countWalls "x" ) 
        //Assert.Equal(3, countWalls "             xxx  xxxxx  0" ) vertical walls
  
    let countInner (tiles: Tile list) : int =
        [ for x in 0 .. tiles.Length - 1  do
            match tiles[x] with 
                | Outside | Inner -> let before= tiles.[0 .. x - 1] 
                                     let wallsPassed = before |> List.map (fun x -> match x with 
                                                                                    | Wall _ -> 'x' 
                                                                                    | _ -> '.')  
                                                              |> List.map string 
                                                              |> String.concat "" 
                                                              |> countWalls
                                     if (wallsPassed% 2) = 0 then 
                                        yield 0 
                                     else yield 1
                                          
                | Wall _ -> yield 1 
        ] |> List.sum

    let fillInner (tiles: Tile list) : Tile list=
        [ for x in 0 .. tiles.Length - 1  do
            match tiles[x] with 
                | Outside | Inner -> let before= tiles.[0 .. x - 1] 
                                     let wallsPassed = before |> List.map (fun x -> match x with 
                                                                                    | Wall _ -> 'x' 
                                                                                    | _ -> '.')  
                                                              |> List.map string 
                                                              |> String.concat "" 
                                                              |> countWalls
                                     if (wallsPassed% 2) = 0 then 
                                        yield Outside 
                                     else yield Inner 
                                          
                | Wall c -> yield Wall c 
        ] 

    let fillMap (map: Sitemap):Sitemap =
        map |> List.map fillInner

    [<Fact>]
    let countInnerText() =
        let cmds = readInit "testinput.txt" 
        let outline = digOutline cmds
        let map = outlineMap outline
        Assert.Equal(7, countInner map[2]) 
        Assert.Equal(6, countInner map[0]) 
        Assert.Equal(6, countInner map[1]) 
        Assert.Equal(5, countInner map[3]) 
         

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
    let findAreaLargeTest () =
        let cmds = readInit "input.txt" 
        let outline = digOutline cmds
        let map = outlineMap outline
//        prettyPrint map "map.out"
        let cnt = findArea map
        Assert.NotEqual(42970, cnt) 

    [<Fact>]
    let getWallTestLarge () =
        let cmds = readInit "input.txt" 
        let outline = digOutline cmds
        let map = outlineMap outline
        prettyPrint map
 
    [<Fact>]
    let getWallTest () =
        let cmds = readInit "testinput.txt" 
        let outline = digOutline cmds
        let map = outlineMap outline
        prettyPrint map
        Assert.Equal(10, map.Length)
        Assert.Equal(7, map[0].Length)
        //printfn "%A" map
  
    //[<Fact>]
    //let testOutline () =
    //    Assert.Equal<Outline>([], digOutline [])
    //    Assert.Equal<Outline>([(0,0),"a";(1,0),"a";(2,0),"a"], digOutline [L, 2, "a"])

    [<Fact>]
    let parseCommandTest () =
        Assert.Equal<Command>((R, 6,"(#70c710)"), parseCommand "R 6 (#70c710)")

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(14, input.Length) 

    let [<EntryPoint>] main _ =
        let cmds = readInit "input.txt" 
        let outline = digOutline cmds
        let map = outlineMap outline
        //prettyPrintFile map "map.out"
        prettyPrintBmp map "map.bmp"
        //let filledmap = fillMap map 
        //prettyPrintFile filledmap "map_filled.out"
        0 