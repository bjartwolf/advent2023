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

    let mirror matrix = matrix |> Matrix.mapRows (fun i row -> row |> Vector.toArray |> Array.rev |> vector) 

    [<Fact>]
    let doubleMirrorTest () = 
        let input = getMatrixes "testinput.txt" 
        let isEqual =  input = (input |> Array.map mirror |> Array.map mirror )
        Assert.True(isEqual) 
 
    [<Fact>]
    let test2 () = 
        let input = getMatrixes "testinput.txt" 
        let flipped = input |> Array.map mirror 
        printfn "%A" input
        printfn "%A" flipped 
        Assert.Equal(2, input.Length) 

module Program = let [<EntryPoint>] main _ = 0