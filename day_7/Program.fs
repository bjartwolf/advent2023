module Input =
    open System
    open System.IO
    open Xunit 

    let readInit filePath: (string*int) [] = 
        IO.File.ReadAllLines filePath
            |> Array.map (fun x -> let a = x.Split(" ")
                                   a[0],int a[1])

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(5, input.Length) 
        Assert.Equal(("32T3K",765), input[0])

module Program = let [<EntryPoint>] main _ = 0