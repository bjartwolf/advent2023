open MathNet.Numerics.LinearAlgebra


module Progam =
    open System.IO
    open MathNet.Numerics
    open Xunit 

    type Dir = U | D | L | R
    type RGB = string
    type Command = Dir * int * RGB 
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
        let steps = int parts[1]
        cmd,steps,parts[2]

    let readInit (filePath: string): Command list = 
        File.ReadAllLines filePath
            |> Array.toList
            |> List.map parseCommand

    let digOutline (commands: Command list): Outline =
        let rec digger (prevDir: Dir) (commands: Command list) (current: Position) : Position seq =
            seq {
                match commands with
                    | [] -> () 
                    | (cmd,dist,_)::t ->   let x,y = current
                                           match (prevDir, cmd) with
                                                | (L,U) |(D,R)-> yield (x-0.5,y-0.5)
                                                | (R,D) | (U,L) -> yield (x+0.5,y+0.5)
                                                | (L,D) | (U,R) -> yield (x-0.5,y+0.5)
                                                | (D,L) | (R,U) -> yield (x+0.5,y-0.5)
                                                | dir1,dir2 -> failwithf "%A %A is not ok" dir1 dir2
                                           let nextPos = 
                                                  match cmd with
                                                  | U -> (x,y + (double dist)) 
                                                  | D -> (x,y-(double dist)) 
                                                  | L -> (x- (double dist),y) 
                                                  | R -> (x+ (double dist),y) 
                                           yield! digger cmd t nextPos 
            }
        let wall = digger U commands (0.5,-0.5)
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
        Assert.Equal<Command>((R, 6,"(#70c710)"), parseCommand "R 6 (#70c710)")

    let [<EntryPoint>] main _ =
        0 