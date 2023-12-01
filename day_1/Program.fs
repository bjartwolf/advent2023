module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): string array = 
        System.IO.File.ReadAllLines filePath


    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(1000, input.Length) 

module Program = let [<EntryPoint>] main _ = 0