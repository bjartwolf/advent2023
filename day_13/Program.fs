open MathNet.Numerics.LinearAlgebra

module Input =
    open System
    open Xunit 

    let getMatrixes (filePath: string): Matrix<float> array = 
        let allLines = IO.File.ReadAllText(filePath)
        let boards = allLines.Split(Environment.NewLine+Environment.NewLine) 
        boards 
            |> Array.map (fun b -> b.Split(Environment.NewLine)) 
            |> Array.map (fun x -> x |> Array.map (fun a -> a.ToCharArray() |> Array.map ( fun c -> if c = '#' then 1.0 else 0.0))
                                     |> matrix )

    let mirror matrix = matrix |> Matrix.mapRows (fun _ row -> row |> Vector.toArray |> Array.rev |> vector) 

    let flip matrix = matrix |> Matrix.transpose |> mirror |> Matrix.transpose

    let getSubmatrixes (matrix: Matrix<float>) n =
        let rowCount = matrix.RowCount
        let maxCount = min n (rowCount - n)
        let firstRow = n - maxCount
        let lastRow = n + maxCount - 1
        (matrix.[firstRow ..n-1,0..], matrix.[n ..lastRow,0..])

    let matrixRowsSymmetricalAroundN (matrix: Matrix<float>) (n:int) =
        let subMatrix1, subMatrix2 = getSubmatrixes matrix n 
        subMatrix1 = flip subMatrix2

    let smudgeMatrix (m: Matrix<float>) ((smudgeRow, smudgeCol) :(int*int)) =
        let value = m.[smudgeRow,smudgeCol] 
        if value = 1.0 then
            m.[smudgeRow,smudgeCol] <- 0.0
        else
            m.[smudgeRow,smudgeCol] <- 1.0
        m
         
    let matrixRowsSymmetricalAroundNSmudgedInner (matrix: Matrix<float>) (n:int) ((smudgeRow, smudgeCol): (int*int)) =
        let subMatrix1, subMatrix2 = getSubmatrixes matrix n 
        if smudgeCol > subMatrix2.ColumnCount - 1 || smudgeRow > subMatrix2.RowCount - 1 then 
            false
        else
            let smugdedMatrix = smudgeMatrix subMatrix2 (smudgeRow, smudgeCol)
            subMatrix1 = flip smugdedMatrix 

    let matrixRowsSymmetricalAroundNSmudged (matrix: Matrix<float>) (n:int) =
        let cols = [0 .. matrix.ColumnCount - 1 ]
        let rows = [0 .. matrix.RowCount - 1]
        let coords = List.allPairs rows cols
        coords |> List.exists (fun smudge -> matrixRowsSymmetricalAroundNSmudgedInner matrix n smudge) 

    let matrixColumnSymmetricalAroundN (matrix: Matrix<float>) (n:int) =
        matrixRowsSymmetricalAroundN (matrix |> Matrix.transpose) n

    let matrixColumnSymmetricalAroundNSmugded (matrix: Matrix<float>) (n:int) =
        matrixRowsSymmetricalAroundNSmudged (matrix |> Matrix.transpose) n

    let findMatrixSymmetryNr (matrix: Matrix<float>): int =
        [
            for i in 1 .. matrix.RowCount - 1 do
                if matrixRowsSymmetricalAroundN matrix i then
                    yield i*100
            for i in 1 .. matrix.ColumnCount - 1 do 
                if matrixColumnSymmetricalAroundN matrix i then
                    yield i
        ] |> List.head

    let findMatrixSymmetryNrSmuged (matrix: Matrix<float>): int =
        [
            for i in 1 .. matrix.RowCount - 1 do
                if matrixRowsSymmetricalAroundNSmudged matrix i then
                    yield i*100
            for i in 1 .. matrix.ColumnCount - 1 do 
                if matrixColumnSymmetricalAroundNSmugded matrix i then
                    yield i
        ] |> List.head

    [<Fact>]
    let testSumSmugedTest() = 
        let input = getMatrixes "testinput.txt" 
        let sum = input |> Array.map findMatrixSymmetryNrSmuged |> Array.sum
        Assert.Equal(400, sum)

    [<Fact>]
    let testSumSmugedProd() = 
        let input = getMatrixes "input.txt" 
        let sum = input |> Array.map findMatrixSymmetryNrSmuged |> Array.sum
        Assert.Equal(38063, sum)

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