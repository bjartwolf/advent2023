module Input =
    open System
    open System.IO
    open Xunit 

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

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(9, input.Length) 

module Program = let [<EntryPoint>] main _ = 0