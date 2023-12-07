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
        //a

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
 
    [<Fact>]
    let testRules_FourAces_STrongerTHanFourKings () = 
        let card1 = replaceWithAsciiValues "KKKKK"
        let card2 = replaceWithAsciiValues "AAAAA"
        Assert.Equal(-1, compareRule1 (card1,99) (card2,99))
        Assert.Equal(1, compareRule1 (card2,99) (card1,99))

    [<Fact>]
    let testRules_FourKings_STrongerTHanFour99999() = 
        let card1 = replaceWithAsciiValues "KKKKK"
        let card2 = replaceWithAsciiValues "99999"
        Assert.Equal(1, compareRule1 (card1,99) (card2,99))
        Assert.Equal(-1, compareRule1 (card2,99) (card1,99))


    let rankCards (cards: (string*int) []) : (int*int) list=
        cards |> Array.toList 
              |> List.sortWith compareRule1 
              |> List.mapi (fun i (_,bet) -> (i+1, bet))
    
    let productSum (valuedCards: (int*int) list): int =
        valuedCards 
            |> List.map (fun (x,y) -> x * y) 
            |> List.sum

    [<Fact>]
    let testcompare () = 
        Assert.Equal(-1, "abc".CompareTo("bca"))
        Assert.Equal(-1, "aab".CompareTo("aac"))
        Assert.Equal(1, "77888".CompareTo("77788")) // 8 is stronger than 8
        Assert.Equal(-1, "lllmm".CompareTo("mmmnn")) 
        Assert.Equal(-1, String.Compare("llll", "mmmn"))
 


    [<Fact>]
    let sumRanked() = 
        let ranked = rankCards testinput 
        let productSum = productSum ranked 
        Assert.Equal(6440, productSum) 

    [<Fact>]
    let testRules () = 
        let card1 = replaceWithAsciiValues "33332"
        let card2 = replaceWithAsciiValues "2AAAA"
        Assert.Equal(1, compareRule1 (card1,99) (card2,99))
        Assert.Equal(-1, compareRule1 (card2,99) (card1,99))

(*
0 32T3K 765
1 T55J5 684
2 KK677 28
3 KTJJT 220
4 QQQJA 483
*)

    [<Fact>]
    let testRule1 () = 
   
        Assert.Equal(1, compareRule1 testinput[2] testinput[3])  //  KK677 >  KTJJT 

        Assert.Equal(0, compareRule1 testinput[0] testinput[0]) 
        // all hands stronger than hand1
        Assert.Equal(-1, compareRule1 testinput[0] testinput[1]) 
        Assert.Equal(-1, compareRule1 testinput[0] testinput[2]) 
        Assert.Equal(-1, compareRule1 testinput[0] testinput[3]) 
        Assert.Equal(-1, compareRule1 testinput[0] testinput[4]) 

        Assert.Equal(-1, compareRule1 testinput[1] testinput[4]) 


    [<Fact>]
    let sumRankedProd() = 
        let ranked = rankCards input 
        let productSum = productSum ranked 
        Assert.Equal(249390788, productSum) 
//249505486 is not right
// 249245762 is too low
    [<Fact>]
    let test2 () = 
        Assert.Equal(5, testinput.Length) 
//        Assert.Equal(("32:3=",765), testinput[0])

    [<Fact>]
    let testRules2 () = 
        let card1 = replaceWithAsciiValues "77888"
        let card2 = replaceWithAsciiValues "77788"
        Assert.Equal(1, compareRule1 (card1,99) (card2,99))
        Assert.Equal(-1, compareRule1 (card2,99) (card1,99))

    [<Fact>]
    let testRules3 () = 
        let card1 = replaceWithAsciiValues "23456"
        let card2 = replaceWithAsciiValues "65432"
        Assert.Equal(-1, compareRule1 (card1,99) (card2,99))
        Assert.Equal(1, compareRule1 (card2,99) (card1,99))


    let [<EntryPoint>] main _ = 0