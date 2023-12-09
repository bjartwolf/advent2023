module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): int64 list list= 
        let parseLine (l:string): int64 list =  l.Split(" ") |> Array.map int64 |> Array.toList
        let file = File.ReadAllLines(filePath)
        file |> Array.map parseLine |> Array.toList


        // maybe not use lists if slow
    let testinput = readInit "testinput.txt" 

    let rec findDiffSeq (lst: int64 list): int64 seq =
        seq {
            match lst with 
                | a :: b :: tail -> yield b - a
                                    yield! findDiffSeq (b :: tail)
                | _ -> () 
        } 
    let findDiff lst = findDiffSeq lst |> Seq.toList

    [<Fact>]
    let testdiffer () = 
        Assert.Equal<int64 list>([3L], findDiff [3L; 6L]) 
        Assert.Equal<int64 list>([3L; 3L], findDiff [3L; 6L; 9L]) 
        Assert.Equal<int64 list>([2L;3L;4L;5L;6L;7L], findDiff [1L; 3L; 6L; 10L; 15L; 21L; 28L])

    [<Fact>]
    let test2 () = 
        Assert.Equal(3, testinput.Length) 

module Program = let [<EntryPoint>] main _ = 0