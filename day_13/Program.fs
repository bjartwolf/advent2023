open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double


module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): string [] []= 
        let allLines = IO.File.ReadAllText(filePath)
        let boards = allLines.Split(Environment.NewLine+Environment.NewLine) 
        boards 
            |> Array.map (fun b -> b.Split(Environment.NewLine)) 

    let getMatrixes (filePath:string) : Matrix<float> array =
        let rawData = readInit filePath

        rawData |> Array.map (
            fun x -> x |> Array.map (fun a -> a.ToCharArray() |> Array.map ( fun c -> if c = '#' then 1.0 else 0.0))
                       |> matrix )

    [<Fact>]
    let test2 () = 
        let input = getMatrixes "testinput.txt" 
        printfn "%A" input
        Assert.Equal(2, input.Length) 

module Program = let [<EntryPoint>] main _ = 0