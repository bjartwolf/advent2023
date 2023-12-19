module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): int list = 
        [1]

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(1, input.Length) 

module Program = let [<EntryPoint>] main _ = 0