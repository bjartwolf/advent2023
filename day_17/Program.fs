module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): int list list = 
        File.ReadAllLines filePath
            |> Array.map (fun l -> l.ToCharArray() |> Array.map int |> Array.toList) 
            |> Array.toList

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(10, input.Length) 

module Program = let [<EntryPoint>] main _ = 0