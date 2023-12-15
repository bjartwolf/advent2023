module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): int list = 
        use sr = new StreamReader (filePath) 
        let line = sr.ReadLine()
        let numbers = line.Split(",")
        numbers |> Array.map(fun f -> Int32.Parse(f)) |> Array.toList
        
    let testString = "HASH"
  // csv, ignore newline.
  // blir masse tall i en linje
  // sum av er et tall
  // sum av det igjen
    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(1, input.Length) 

module Program = let [<EntryPoint>] main _ = 0