module Program =
    open System
    open Xunit 

    // lowerranked ASCII, remember when sorting by value
    let replaceWithAsciiValues (input:string) =  
        input.ToCharArray() 
            |> Seq.map (fun (x:Char) -> match x with
                                            | 'A' -> '=' 
                                            | 'K' -> '<' 
                                            | 'Q' -> ';' 
                                            | 'T' -> ':' 
                                            | 'J' -> '1' 
                                            | x -> x)
            |> Seq.toArray
            |> System.String

    let readInit filePath: (string*int) [] = 
        IO.File.ReadAllLines filePath
            |> Array.map (fun x -> let a = x.Split(" ")
                                   (replaceWithAsciiValues a[0]),int a[1])

    let testinput = readInit "testinput.txt" 
    let input = readInit "input.txt" 

    let compareRule2 (hand1:string) (hand2:string): int =
         String.Compare(hand1, hand2, StringComparison.Ordinal)

    let scoreHand (hand: int list) = 
       match hand with 
                | [5] -> 6
                | [4;1] -> 5
                | [3;2] -> 4
                | 3::_ -> 3
                | [2;2;1] -> 2
                | 2::_ -> 1
                | _ -> 0

    let findWinner (group1: int list) (group2: int list) (hand1: string) (hand2: string) j1 j2 =
        let group1WithJokers = if group1.IsEmpty then [j1] 
                               else group1[0] + j1 :: group1.Tail
        let group2WithJokers = if group2.IsEmpty then [j2] 
                               else group2[0] + j2 :: group2.Tail
        let score1, score2 = scoreHand group1WithJokers, scoreHand group2WithJokers  
        if score1 > score2 then 1
        else if score1 < score2 then -1
        else compareRule2 hand1 hand2 
       
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

    let countJokers hand =  countChar '1' hand 
    let filterOutJokers hand = hand |> Seq.filter (fun x -> x <> '1') |> String.Concat 
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
