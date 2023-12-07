module Program =
    open System
    open Xunit 

    // lowerranked ASCII, remember when sorting by value
    // T=:, J=;, Q=< K = = , A= >
    // T=58, J=59, Q=60, K=61 A=62
    let readInit filePath: (string*int) [] = 
        let replaceWithAsciiValues (input:string) =  
            input.ToCharArray() 
                |> Seq.map (fun (x:Char) -> match x with
                                                | 'T' -> char 58
                                                | 'J' -> char 59
                                                | 'Q' -> char 60
                                                | 'K' -> char 61
                                                | 'A' -> char 62
                                                | x -> x) 
                |> Seq.toArray
                |> System.String

        IO.File.ReadAllLines filePath
            |> Array.map (fun x -> let a = x.Split(" ")
                                   (replaceWithAsciiValues a[0]),int a[1])

    //let countEqual (hand: string) -> 
    let testinput = readInit "testinput.txt" 
        

    [<Fact>]
    let test2 () = 
        Assert.Equal(5, testinput.Length) 
        Assert.Equal(("32:3=",765), testinput[0])

    let [<EntryPoint>] main _ = 0