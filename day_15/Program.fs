module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string) = 
        let line = File.ReadAllLines filePath
                        |> Array.head
        line.Split(",")
        
        
    let testInput = readInit "testinput.txt"

    let wordToNumbers (word:string): int list =
        word.ToCharArray() |> Array.map int |> Array.toList

    [<Fact>]
    let makeWordTest () = 
        Assert.Equal<int list>([72;65;83;72],wordToNumbers "HASH")

  // csv, ignore newline.
  // blir masse tall i en linje
  // sum av er et tall
  // sum av det igjen

    [<Fact>]
    let test2 () = 
        let input = readInit "input.txt" 
        Assert.Equal(4000, input.Length) 

module Program = let [<EntryPoint>] main _ = 0