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
//                                            | 'J' -> 'd'  // move J after the logic works
                                            | 'T' -> 'e' 
                                            | '9' -> 'f' 
                                            | '8' -> 'g' 
                                            | '7' -> 'h' 
                                            | '6' -> 'i' 
                                            | '5' -> 'j' 
                                            | '4' -> 'k' 
                                            | '3' -> 'l' 
                                            | '2' -> 'm' 
                                            | 'J' -> 'n' 
                                            | s -> failwith (sprintf "booom %A" s)) 
            |> Seq.toArray
            |> System.String

    let readInit filePath: (string*int) [] = 
        IO.File.ReadAllLines filePath
            |> Array.map (fun x -> let a = x.Split(" ")
                                   (replaceWithAsciiValues a[0]),int a[1])

    let testinput = readInit "testinput.txt" 
    let input = readInit "input.txt" 

    let compareRule2 (hand1:string) (hand2:string): int =
        let foo = [hand1;hand2] |> List.sort
        if hand1 = hand2 then 
            0 
        else if foo[0] = hand1 then
            1
        else
            -1

    let isHouse a b j : bool =
        if b > a then failwith "should not be this way"
        if (j = 0) then
            a = 3 && b = 2
        else if (j = 1) then
            (a = 2 && b = 2) || (a = 3 && b = 1)
        else if (j = 2) then
            (a = 2 && b = 1)  || a = 3 
        else 
            failwith "booomjoker"

    let isFive a j : bool = a + j >= 5
    let isFour a j : bool = a + j >= 4
    let isThree a j : bool = a + j >= 3
    let isTwoPairs a b j : bool = 
        if j >= 2 then  failwith "Should have been three equal or something"
        a = 2 && b = 2 || a =2 && b = 1 && j = 1
        
    let isOnePair a j : bool = 
        a = 2 || a = 1 && j = 1 

 
    let findWinner (group1: int list) (group2: int list) (hand1: string) (hand2: string) j1 j2 =
        if hand1 = hand2 then 0
        else match group1, group2 with
                | [],[] -> 0  // all jokers
                | [],b::_ when isFive 0 j1 && isFive b j2-> compareRule2 hand1 hand2
                | a::_,[] when isFive a j1 -> compareRule2 hand1 hand2
                | [],_ -> 1 // all jokers
                | _ ,[] -> -1 //all jokers
                | a::_,b::_ when isFive a j1 && isFive b j2-> compareRule2 hand1 hand2
                | a::_,_ when isFive a j1 -> 1
                | _,b::_ when isFive b j2 -> -1
                | a::_, b::_ when isFour a j1 && isFour b j2 -> compareRule2 hand1 hand2 
                | a::_,_ when isFour a j1 -> 1 
                | _,b::_ when isFour b j2 -> -1 
                | a::c::_,b::d::_ when isHouse a c j1 && isHouse b d j2 -> compareRule2 hand1 hand2 
                | a::c::_,_       when isHouse a c j1  -> 1 
                | _      ,b::d::_ when isHouse b d j2  -> -1 
                | a::_, b::_ when isThree a j1 && isThree b j2 -> compareRule2 hand1 hand2 
                | a::_, _ when isThree a j1 -> 1
                | _   , b::_ when isThree b j2 -> -1
                | 2::a::_, 2::b::_ when isTwoPairs 2 a j1 && isTwoPairs 2 b j2 -> compareRule2 hand1 hand2 
                | 2::a::_,_        when isTwoPairs 2 a j1 -> 1 
                | _      , 2::b::_ when isTwoPairs 2 b j2 -> -1
                | a::_,b::_ when isOnePair a j1 && isOnePair b j2 -> compareRule2 hand1 hand2 
                | a::_,_  when isOnePair a j1  -> 1
                | _, b::_ when isOnePair b j2 -> -1
                | _ -> compareRule2 hand1 hand2 
       
    let countChar (c: char) (str: string) =
        str
        |> Seq.filter (fun x -> x = c)
        |> Seq.length


    let groupHand (hand:string) = 
        hand.ToCharArray() 
            |> Array.groupBy (fun x -> x) 
            |> Array.map (fun (x,cards) -> cards.Length )
            |> Array.sort 
            |> Array.rev
            |> Array.toList

    let countJokers hand =  countChar 'n' hand 
    let filterOutJokers hand = hand |> Seq.filter (fun x -> x <> 'n') |> String.Concat 
    let compareRule1 ((hand1,_):string*int) ((hand2,_):string*int): int =
        let jokersIn1 = countJokers hand1 
        let jokersIn2 = countJokers hand2 
        let hand1WithoutJokers = filterOutJokers hand1 
        let hand2WithoutJokers = filterOutJokers hand2 
        let grouped1 = groupHand hand1WithoutJokers 
        let grouped2 = groupHand hand2WithoutJokers 
        findWinner grouped1 grouped2 hand1 hand2 jokersIn1 jokersIn2

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
        Assert.Equal(5905, productSum) 

    [<Fact>]
    let sumRankedprod() = 
        let ranked = rankCards input 
        let productSum = productSum ranked 
        Assert.Equal(248750248, productSum) 

    let [<EntryPoint>] main _ = 0
