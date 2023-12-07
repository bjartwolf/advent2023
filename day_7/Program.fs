open System.Collections


module Program =
    open System
    open Xunit 

    // lowerranked ASCII, remember when sorting by value
    let replaceWithAsciiValues (input:string) =  
        input.ToCharArray() 
            |> Seq.map (fun (x:Char) -> match x with
                                            | 'A' -> 'a' 
                                            | 'K' -> 'b' 
                                            | 'Q' -> 'c' 
                                            | 'J' -> 'd' 
                                            | 'T' -> 'e' 
                                            | '9' -> 'f' 
                                            | '8' -> 'g' 
                                            | '7' -> 'h' 
                                            | '6' -> 'i' 
                                            | '5' -> 'j' 
                                            | '4' -> 'k' 
                                            | '3' -> 'l' 
                                            | '2' -> 'm' 
                                           // | 'J' -> 'n' 
                                            | s -> failwith (sprintf "booom %A" s)) 
            |> Seq.toArray
            |> System.String

    let readInit filePath: (string*int) [] = 
        IO.File.ReadAllLines filePath
            |> Array.map (fun x -> let a = x.Split(" ")
                                   (replaceWithAsciiValues a[0]),int a[1])

    //let countEqual (hand: string) -> 
    let testinput = readInit "testinput.txt" 
    let input = readInit "input.txt" 

    let compareRule2 (hand1:string) (hand2:string): int =
        let a = String.Compare(hand1, hand2)
        let b = hand1.CompareTo(hand2)
        let foo = [hand1;hand2] |> List.sort
        if foo[0] = hand1 then
            1
        else
            -1
 
    let findWinner (group1: int list) (group2: int list) (hand1: string) (hand2: string) =
        match group1, group2 with
        // fem like
            | [5],[5] -> compareRule2 hand1 hand2
            | [5],_ -> 1
            | _,[5] -> -1
         // fire like
            | 4::_, 4::_ -> compareRule2 hand1 hand2 
            | 4::_,_ -> 1 
            | _,4::_ -> -1 
            // hus
            | 3::2::_, 3::2::_ -> compareRule2 hand1 hand2 
            | 3::2::_, _ -> 1
            | _      , 3::2::_ -> -1
            // 3 like
            | 3::_, 3::_ -> compareRule2 hand1 hand2 
            | 3::_, _ -> 1
            | _   , 3::_ -> -1
            // 2o par
            | 2::2::_, 2::2::_ -> compareRule2 hand1 hand2 
            | 2::2::_,_        -> 1
            | _      , 2::2::_ -> -1
            // 1 par
            | 2::_,2::_ -> compareRule2 hand1 hand2 
            | 2::_,_ -> 1
            | _, 2::_ -> -1
            | _ -> compareRule2 hand1 hand2 
 
       
       //a
    let countChar (c: char) (str: string) =
        str
        |> Seq.filter (fun x -> x = c)
        |> Seq.length


    let compareRule1 ((hand1,_):string*int) ((hand2,_):string*int): int =
        let groupHand (hand:string) = 
            hand.ToCharArray() 
                |> Array.groupBy (fun x -> x) 
                |> Array.map (fun (x,cards) -> cards.Length )
                |> Array.sort 
                |> Array.rev
                |> Array.toList
        let jokersIn1 = countChar 'n' hand1 
        let jokersIn2 = countChar 'n' hand2 
        let grouped1 = groupHand hand1 
        let grouped2 = groupHand hand2
        findWinner grouped1 grouped2 hand1 hand2

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


    let [<EntryPoint>] main _ = 0