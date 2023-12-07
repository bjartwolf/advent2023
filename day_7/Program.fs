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
       
    // -1 means hand1 is weaker than hand2
    // 1 means hand1 is stronger than hand2
    // a string here startes with the highest ascii car codes for strongest card
    let compareRule2 ((hand1,_):string*int) ((hand2,_):string*int): int =
        hand1.CompareTo(hand2) 

    [<Fact>]
    let testRule2 () = 
        Assert.Equal(0, compareRule2 testinput[0] testinput[0]) 
        Assert.Equal(1, compareRule2 testinput[2] testinput[3]) // Card 2 is stronger than card 3 because KK > KT 
        Assert.Equal(-1, compareRule2 testinput[1] testinput[4]) // Card 4 is stronger than 1 
 
    [<Fact>]
    let test2 () = 
        Assert.Equal(5, testinput.Length) 
        Assert.Equal(("32:3=",765), testinput[0])

    let [<EntryPoint>] main _ = 0