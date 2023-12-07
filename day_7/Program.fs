open System.Collections


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
    let input = readInit "input.txt" 
       
    // -1 means hand1 is weaker than hand2
    // 1 means hand1 is stronger than hand2
    // a string here startes with the highest ascii car codes for strongest card
    let compareRule2 (hand1:string) (hand2:string): int =
        hand1.CompareTo(hand2) 

    let compareRule1 ((hand1,_):string*int) ((hand2,_):string*int): int =
        let groupHand (hand:string) = 
            hand.ToCharArray() 
                |> Array.groupBy (fun x -> x) 
                |> Array.map (fun (x,cards) -> cards.Length )
                |> Array.sort 
                |> Array.rev
                |> Array.toList
        let grouped1 = groupHand hand1 
        let grouped2 = groupHand hand2
        match grouped1,grouped2 with
            | [5],[5] -> compareRule2 hand1 hand2
            | [5],_ -> 1
            | _,[5] -> -1
            | 4::_, 4::_ -> compareRule2 hand1 hand2 
            | 4::_,_ -> 1 
            | _,4::_ -> -1 
            | 3::2::_, 3::2::_ -> compareRule2 hand1 hand2 
            | 3::2::_, _ -> 1
            | _,3::2::_ -> -1
            | 3::_, 3::_ -> compareRule2 hand1 hand2 
            | 3::_, _ -> 1
            | _, 3::_ -> -1
            | 2::2::_, 2::2::_-> compareRule2 hand1 hand2 
            | 2::2::_,_ -> 1
            | _, 2::2::_ -> -1
            | 2::_,2::_ -> compareRule2 hand1 hand2 
            | 2::_,_ -> 1
            | _, 2::_ -> -1
            | _ -> compareRule2 hand1 hand2 

    [<Fact>]
    let testRule1 () = 
        Assert.Equal(0, compareRule1 testinput[0] testinput[0]) 
        // all hands stronger than hand1
        Assert.Equal(-1, compareRule1 testinput[0] testinput[1]) 
        Assert.Equal(-1, compareRule1 testinput[0] testinput[2]) 
        Assert.Equal(-1, compareRule1 testinput[0] testinput[3]) 
        Assert.Equal(-1, compareRule1 testinput[0] testinput[4]) 

        Assert.Equal(1, compareRule1 testinput[2] testinput[3]) 
        Assert.Equal(-1, compareRule1 testinput[1] testinput[4]) 

    let rankCards (cards: (string*int) []) : (int*int) list=
        cards |> Array.toList 
              |> List.sortWith compareRule1 
              |> List.mapi (fun i (_,bet) -> (i+1, bet))
    
    let productSum (valuedCards: (int*int) list): int =
        valuedCards 
            |> List.map (fun (x,y) -> x * y) 
            |> List.sum

    [<Fact>]
    let sumRanked() = 
        let ranked = rankCards testinput 
        let productSum = productSum ranked 
        Assert.Equal(6440, productSum) 

    [<Fact>]
    let sumRankedProd() = 
        let ranked = rankCards input 
        let productSum = productSum ranked 
        Assert.Equal(249229592, productSum) 
  
    [<Fact>]
    let test2 () = 
        Assert.Equal(5, testinput.Length) 
        Assert.Equal(("32:3=",765), testinput[0])


    let [<EntryPoint>] main _ = 0