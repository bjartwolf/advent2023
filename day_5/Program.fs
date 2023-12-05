module Input =
    open System
    open System.IO
    open Xunit 

    let test_seeds = [79;14;55;13]
    let readInit (filePath: string): int list = 
        let input = System.IO.File.ReadAllLines filePath
        let seeds = input[0].Replace("seeds: ","").Split(" ") |> Array.map (fun x -> int x) |> Array.toList
        seeds 

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal<int list>(test_seeds, input) 

module Program = let [<EntryPoint>] main _ = 0