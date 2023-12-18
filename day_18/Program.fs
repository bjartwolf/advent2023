open MathNet.Numerics.LinearAlgebra


module Progam =
    open System.IO
    open MathNet.Numerics
    open Xunit 

    type Dir = U | D | L | R
    type RGB = string
    type Command = Dir * int64 * RGB 
    type Position = double*double 
    type Wall = Position*RGB
    type Outline = Position list

    let parseCommand (cmdString: string): Command = 
        let parts = cmdString.Split(" ")
        let cmd = match parts[0] with 
                        |"L" -> L
                        |"R"-> R
                        |"U"-> U
                        |"D"-> D
                        | x -> failwithf "ouch %A %A" x parts[0]
        let steps = int64 parts[1]
        cmd,steps,parts[2]

    let parseStupidCommand (cmdString: string): Command = 
        let parts = cmdString.Split(" ")
        let cmd = match parts[2][7] with 
                        | '0' -> R
                        | '1' -> D
                        | '2' -> L
                        | '3' -> U
                        | x -> failwithf "ouch %A %A" x parts[0]
        let steps = System.Convert.ToInt32(parts[2][2..6], 16)
        cmd,steps,""

    [<Fact>]
    let parseStupidCommandTest () =
        Assert.Equal<Command>((R, 461937L,""), parseStupidCommand "R 6 (#70c710)")

    let readInit (filePath: string): Command list = 
        File.ReadAllLines filePath
            |> Array.toList
            |> List.map parseCommand

    let readStupid (filePath: string): Command list = 
        File.ReadAllLines filePath
            |> Array.toList
            |> List.map parseStupidCommand

    let digOutline (commands: Command list): Outline =
        let rec digger (commands: Command list) (current: Position) : Position seq =
            seq {
                match commands with
                    | [] -> () 
                    | (dir,dist,_)::t->
                                          let x,y = current
                                          let nextPos = 
                                                  match dir with
                                                  | U -> (x,y + (double dist)) 
                                                  | D -> (x,y-(double dist)) 
                                                  | L -> (x- (double dist),y) 
                                                  | R -> (x+ (double dist),y) 
                                          let x',y'= nextPos
                                          if not t.IsEmpty then
                                              let nextDir,_,_ = t.Head
                                              match (dir,nextDir) with
                                                    | (L,U) ->
                                                        yield (x'-0.5,y'-0.5)
                                                    |(D,R)-> 
                                                        yield (x'+0.5,y'+0.5)
                                                    | (R,D)  ->
                                                        yield (x'+0.5,y'+0.5)
                                                    | (U,L) -> 
                                                        yield (x'-0.5,y'-0.5)
                                                    | (L,D) -> 
                                                        yield (x'+0.5,y'-0.5)
                                                    | (U,R) -> 
                                                        yield (x'-0.5,y'+0.5)
                                                    | (D,L)  ->
                                                        yield (x'+0.5,y'-0.5)
                                                    | (R,U) -> 
                                                        yield (x'-0.5,y'+0.5)
                                                    | dir1,dir2 -> failwithf "%A %A is not ok" dir1 dir2
                                              yield! digger t nextPos 
                    | _ -> yield (0.0,0.0)
            }
        let wall = digger (commands @ [commands.Head]) (0.5,-0.5)
        (List.ofSeq wall)

    let positionToArray ((x,y): Position) = [|x; y|]
     
    let vectorsToMatrix (outline: Outline) = 
        let vectors = outline |> List.toArray |> Array.map positionToArray
        let mtrx = DenseMatrix.ofColumnArrays vectors
        mtrx

    let rec shoelace (m: Matrix<double>): double =
        if m.ColumnCount = 0 then 0.0
        else
            let subM = m.SubMatrix(0,2,0,2)
            let det = subM.Determinant()
            if m.ColumnCount = 2 then det
            else 
                let rest = m.SubMatrix(0,2,2,(m.ColumnCount - 2))
                det + (shoelace rest)

    let mirror matrix = matrix |> Matrix.mapRows (fun _ row -> row |> Vector.toArray |> Array.rev |> vector) 


    [<Fact>]
    let fooTest () =
        let cmds = readInit "testinput.txt" 
        let outline = digOutline cmds
        let mtrx = vectorsToMatrix outline
        printfn "%A" mtrx 
        let area = shoelace (mirror mtrx)
        printfn "area %A" area 
        Assert.Equal(62.0, area)

    [<Fact>]
    let stupidTest () =
        let cmds = readStupid "testinput.txt" 
        let outline = digOutline cmds
        let mtrx = vectorsToMatrix outline
        printfn "%A" mtrx 
        let area = shoelace (mirror mtrx)
        printfn "area %A" area 
        Assert.Equal(952408144115.0, area)

    [<Fact>]
    let stupidTest2 () =
        let cmds = readStupid "input.txt" 
        let outline = digOutline cmds
        let mtrx = vectorsToMatrix outline
        printfn "%A" mtrx 
        let area = shoelace (mirror mtrx)
        printfn "area %A" area 
        Assert.Equal(104454050898330.98, area)


    [<Fact>]
    let fooTestw () =
        let cmds = readInit "input.txt" 
        let outline = digOutline cmds
        let mtrx = vectorsToMatrix outline
        printfn "%A" mtrx 
        let area = shoelace (mirror mtrx)
        Assert.Equal( 40131.0, area)
        ()

  
    [<Fact>]
    let parseCommandTest () =
        Assert.Equal<Command>((R, 6L,"(#70c710)"), parseCommand "R 6 (#70c710)")

    let [<EntryPoint>] main _ =
        0 