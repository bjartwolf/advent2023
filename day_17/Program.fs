module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): int list list = 
        File.ReadAllLines filePath
            |> Array.map (fun l -> l.ToCharArray() |> Array.map (fun c -> int (c.ToString())) |> Array.toList) 
            |> Array.toList

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(13, input.Length) 
        Assert.Equal(13, input[0].Length) 
        Assert.Equal(2, input[0][0]) 
        Assert.Equal(3, input[12][12]) 
        Assert.Equal(4, input[0][1]) 

module Program = let [<EntryPoint>] main _ = 0