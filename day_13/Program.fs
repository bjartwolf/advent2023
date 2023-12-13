module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): string list list = 
        let allLines = IO.File.ReadAllText(filePath)
        let boards = allLines.Split(Environment.NewLine+Environment.NewLine) 
        boards 
            |> Array.map (fun b -> b.Split(Environment.NewLine)) 
            |> Array.map Array.toList
            |> Array.toList

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(2, input.Length) 

module Program = let [<EntryPoint>] main _ = 0