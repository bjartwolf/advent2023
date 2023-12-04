module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string) = 
        System.IO.File.ReadAllLines filePath

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(6, input.Length) 

    [<Fact>]
    let test3 () = 
        let input = readInit "input.txt" 
        Assert.Equal(120, input.Length) 


module Program = let [<EntryPoint>] main _ = 0