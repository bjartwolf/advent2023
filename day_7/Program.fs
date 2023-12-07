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

    //let countEqual (hand: string) -> 
    let testinput = readInit "testinput.txt" 
    let input = readInit "input.txt" 

    let compareRule2 (hand1:string) (hand2:string): int =
        let a = String.Compare(hand1, hand2)
        let b = hand1.CompareTo(hand2)
        let foo = [hand1;hand2] |> List.sort
        if hand1 = hand2 then 
            failwith (sprintf "booom %s %s" hand1 hand2)
        if foo[0] = hand1 then
            1
        else
            -1
 
    let findWinner (group1: int list) (group2: int list) (hand1: string) (hand2: string) j1 j2 =
        if (j1 > 0 || j2 > 0) then
            printfn "joker"
        match group1, group2 with
        // fem like, har du masse jokere går du hit
            | a::_,b::_ when a+j1 >= 5 && b + j2 >= 5 -> compareRule2 hand1 hand2
            | a::_,_ when a + j1 >= 5 -> 1
            | _,b::_ when b + j2 >=5 -> -1
         // fire like, har du tre jokere går du hit
            | a::_, b::_ when a + j1 >= 4 && b + j2 >= 4-> compareRule2 hand1 hand2 
            | a::_,_ when a + j1 >=4 -> 1 
            | _,b::_ when b + j2 >= 4 -> -1 
            // hus, har du ett par og  to jokere kan du får tre like
            | a::2::_, b::2::_ when a+j1 >=3 && b + j2 >=3   -> compareRule2 hand1 hand2 
            | a::2::_, _ when a + j1 >=3 -> 1 
            | _      , b::2::_ when b + j2 >=2 -> -1
            // husvariant
            | 2::a::_, 2::b::_ when a+j1 >=3 && b + j2 >=3   -> compareRule2 hand1 hand2 
            | 2::a::_, _ when a + j1 >=3 -> 1 
            | _      , 2::b::_ when b + j2 >=2 -> -1
             // 3 like
            | a::_, b::_ when a + j1 >= 3 && b+j2 >= 3 -> compareRule2 hand1 hand2 
            | a::_, _ when a + j1 >= 3 -> 1
            | _   , b::_ when b + j2 >= 3-> -1
            // 2o par
            | 2::a::_, 2::b::_ when a + j1 >= 2 && b+j2 >= 2-> compareRule2 hand1 hand2 
            | 2::a::_,_        when a + j1 >= 2 -> 1
            | _      , 2::b::_ when b + j2 >= 2 -> -1
            // 1 par
            | a::_,b::_ when a + j1 >= 2 && b + j2 >=2 -> compareRule2 hand1 hand2 
            | a::_,_  when a + j1 >= 2 -> 1
            | _, b::_ when b + j2 >= 2-> -1
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
        let hand1WithoutJokers = hand1 |> Seq.filter (fun x -> x <> 'n') |> String.Concat 
        let hand2WithoutJokers = hand2 |> Seq.filter (fun x -> x <> 'n') |> String.Concat 
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
        Assert.Equal(5905, productSum) 

    let [<EntryPoint>] main _ = 0