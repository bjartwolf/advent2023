module Input =
    open System
    open System.IO
    open Xunit 

    let findZeroRows (filePath: string): int list = 
        let lines = IO.File.ReadAllLines(filePath)
        [
            let height = lines.Length
            for i in 0 .. height - 1 do
                if lines[i] |> String.forall (fun c -> c = '.') then
                    yield i
        ]

    let findZeroColumns(filePath: string): int list = 
        let lines = IO.File.ReadAllLines(filePath)
        [
            let height = lines.Length
            for i in 0 .. height - 1 do
                if (lines |> Array.forall (fun l -> l[i] = '.')) then
                    yield i
        ]



    let readInit (filePath: string): (int*int) list = 
        let lines = IO.File.ReadAllLines(filePath)
        [
            let height = lines.Length
            let width = lines[0].Length
            for i in 0 .. height - 1 do
                for j in 0 .. width - 1 do
                    if lines[i][j] = '#' then
                        yield (i,j)
        ]

     //  the sum of the shortest path
     //  path is absolute value of taxicab distance all the pairs with added value, can count itself because it is zero anyway 

    [<Fact>]
    let testFindRows() = 
        let input = findZeroRows "testinput.txt" 
        Assert.Equal<int list>([3;7], input)

    [<Fact>]
    let testFindColumns () = 
        let input = findZeroColumns "testinput.txt" 
        Assert.Equal<int list>([2;5;8], input)


    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(9, input.Length) 
        Assert.True(input |> List.contains (9,0))
        Assert.True(input |> List.contains (9,4))

module Program = let [<EntryPoint>] main _ = 0