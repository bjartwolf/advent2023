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

    let matrixRowsSymmetricalAroundN (matrix: Matrix<float>) (n:int) =
        let rowCount = matrix.RowCount
        let maxCount = min n (rowCount - n)
        let firstRow = n - maxCount
        let lastRow = n + maxCount - 1
        let subMatrix1 = matrix.[firstRow ..n-1,0..]
        let subMatrix2 = matrix.[n ..lastRow,0..]
        subMatrix1 = flip subMatrix2

    let matrixColumnSymmetricalAroundN (matrix: Matrix<float>) (n:int) =
        matrixRowsSymmetricalAroundN (matrix |> Matrix.transpose) n

    [<Fact>]
    let colSymmetricsTest () = 
        let input = getMatrixes "testinput.txt" 
        Assert.False(matrixColumnSymmetricalAroundN input.[0] 4) 
        Assert.False(matrixColumnSymmetricalAroundN input.[0] 3) 
        Assert.True(matrixColumnSymmetricalAroundN input.[0] 5) 

    [<Fact>]
    let rowSymmetricsTest () = 
        let input = getMatrixes "testinput.txt" 
        Assert.True(matrixRowsSymmetricalAroundN input.[1] 4) 
        Assert.False(matrixRowsSymmetricalAroundN input.[1] 3) 
        Assert.False(matrixRowsSymmetricalAroundN input.[1] 5) 

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
        input.[0].[3..6,0..] |> printfn "%A"
        Assert.Equal(2, input.Length) 

module Program = let [<EntryPoint>] main _ = 0