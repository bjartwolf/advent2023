open MathNet.Numerics.LinearAlgebra


module Progam =
    open System.IO
    open MathNet.Numerics
    open Xunit 

    type Dir = U | D | L | R
    type RGB = string
    type Command = Dir * int * RGB 
    type Position = int*int
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
        let rec digger (commands: Command list) (current: Position) : Position seq =
            seq {
                match commands with
                    | [] -> () 
                    | (cmd,dist,_)::t -> let x,y = current
                                         for d in 1 .. dist do 
                                                match cmd with
                                                | U -> yield (x,y+d)
                                                | D -> yield (x,y-d)
                                                | L -> yield (x-d,y)
                                                | R -> yield (x+d,y)
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

    let positionToArray ((x,y): int*int) =
        [|double x; double y|]
     
    let vectorsToMatrix (outline: Outline) = 
        let vectors = outline |> List.toArray |> Array.map positionToArray
        let mtrx = DenseMatrix.ofColumnArrays vectors
        let firstCol = mtrx.SubMatrix(0,2,0,1)
        mtrx.Append(firstCol) 

    [<Fact>]
    let fooTest () =
        let cmds = readInit "testinput.txt" 
        let outline = digOutline cmds
        let mtrx = vectorsToMatrix outline
        printfn "%A" mtrx
        ()


 
    [<Fact>]
    let findAreaTest () =
        let cmds = readInit "testinput.txt" 
        let outline = digOutline cmds
        ()

  
    [<Fact>]
    let parseCommandTest () =
        Assert.Equal<Command>((R, 6,"(#70c710)"), parseCommand "R 6 (#70c710)")

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(14, input.Length) 

    let [<EntryPoint>] main _ =
        0 