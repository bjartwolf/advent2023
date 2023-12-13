open MathNet.Numerics.LinearAlgebra


module Input =
    open System
    open Xunit 

    let readInit (filePath: string): string [] []= 
        let allLines = IO.File.ReadAllText(filePath)
        let boards = allLines.Split(Environment.NewLine+Environment.NewLine) 
        boards 
            |> Array.map (fun b -> b.Split(Environment.NewLine)) 

    let getMatrixes (filePath:string) : Matrix<float> array =
        readInit filePath
         |> Array.map ( fun x -> x |> Array.map (fun a -> a.ToCharArray() |> Array.map ( fun c -> if c = '#' then 1.0 else 0.0))
                                   |> matrix )

    let mirror matrix = matrix |> Matrix.mapRows (fun _ row -> row |> Vector.toArray |> Array.rev |> vector) 

    let flip matrix = matrix |> Matrix.transpose |> mirror |> Matrix.transpose

    [<Fact>]
    let doubleMirrorTest () = 
        let input = getMatrixes "testinput.txt" 
        let isEqual =  input = (input |> Array.map mirror |> Array.map mirror )
        Assert.True(isEqual) 
 
    [<Fact>]
    let test2 () = 
        let input = getMatrixes "testinput.txt" 
        let mirrored = input |> Array.map mirror 
        let flipped = input |> Array.map flip 
        printfn "%A input" input.[0]
        printfn "%A flipped" flipped.[0]
        printfn "%A mirroed" mirrored.[0]
//        printfn "%A" input.[0]
 //       input.[0].[1..,1..] |> printfn "%A"
//        printfn "%A" flipped 
        Assert.Equal(2, input.Length) 

module Program = let [<EntryPoint>] main _ = 0