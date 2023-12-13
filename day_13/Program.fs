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

    let findMatrixSymmetryNr (matrix: Matrix<float>): int =
        [
            for i in 1 .. matrix.RowCount - 1 do
                if matrixRowsSymmetricalAroundN matrix i then
                    yield i*100
            for i in 1 .. matrix.ColumnCount - 1 do 
                if matrixColumnSymmetricalAroundN matrix i then
                    yield i
        ] |> List.head

    [<Fact>]
    let testSumTest() = 
        let input = getMatrixes "testinput.txt" 
        let sum = input |> Array.map findMatrixSymmetryNr |> Array.sum
        Assert.Equal(405, sum)

    [<Fact>]
    let prodSumTest() = 
        let input = getMatrixes "input.txt" 
        let sum = input |> Array.map findMatrixSymmetryNr |> Array.sum
        Assert.Equal(33735, sum)

module Program = let [<EntryPoint>] main _ = 0