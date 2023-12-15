module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string) = 
        let line = File.ReadAllLines filePath
                        |> Array.head
        line.Split(",")
            |> Array.toList
        
    let testInput = readInit "testinput.txt"

    let wordToNumbers (word:string): int list =
        word.ToCharArray() |> Array.map int |> Array.toList

    [<Fact>]
    let makeWordTest () = 
        Assert.Equal<int list>([72;65;83;72],wordToNumbers "HASH")

    let calcWordNr (word: string) : int =
        let rec calcWordNrInner (wordDigits: int list) (current: int): int =
            match wordDigits with
                | [] -> current
                | h::t -> calcWordNrInner t (((current + h) * 17) % 256)
        calcWordNrInner (wordToNumbers word) 0 

    [<Fact>]
    let calcWordTest () = 
        Assert.Equal(52, calcWordNr "HASH")
            
    let calcWordList (words: string list) : int list =
        words |> List.map calcWordNr

    [<Fact>]
    let calcWordListTest () = 
        let input = readInit "testinput.txt" 
        Assert.Equal<int list>([30;253;97;47;14;180;9;197;48;214;231], calcWordList input)

    [<Fact>]
    let calcSumForTest () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(1320, input |> calcWordList |> List.sum) 


    [<Fact>]
    let test2 () = 
        let input = readInit "input.txt" 
        Assert.Equal(4000, input.Length) 

module Program = let [<EntryPoint>] main _ = 0