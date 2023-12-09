module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): int64 list list= 
        let parseLine (l:string): int64 list =  l.Split(" ") |> Array.map int64 |> Array.toList
        let file = File.ReadAllLines(filePath)
        file |> Array.map parseLine |> Array.toList

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(3, input.Length) 

module Program = let [<EntryPoint>] main _ = 0