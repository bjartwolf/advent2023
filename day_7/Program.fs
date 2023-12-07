module Program =
    open System
    open Xunit 

    let joker = '1'
    let replaceWithAsciiValues (input:string) =  
        input.ToCharArray() 
            |> Seq.map (fun (x:Char) -> match x with
                                            | 'A' -> '=' 
                                            | 'K' -> '<' 
                                            | 'Q' -> ';' 
                                            | 'T' -> ':' 
                                            | 'J' -> joker 
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
                | 4::_ -> 5
                | [3;2] -> 4
                | 3::_ -> 3
                | [2;2;1] -> 2
                | 2::_ -> 1
                | _ -> 0

    let findWinner cards1 cards2 hand1 hand2 =  
        let score1, score2 = scoreHand cards1, scoreHand cards2 
        if score1 > score2 then 1
        else if score1 < score2 then -1
        else compareRule2 hand1 hand2 
       
    let groupHand (hand:string) = 
        hand.ToCharArray() 
            |> Array.groupBy id
            |> Array.map (fun (_,cards) -> cards.Length )
            |> Array.sort 
            |> Array.rev
            |> Array.toList

    let countJokers hand = 
        hand |> Seq.filter (fun x -> x = joker)
             |> Seq.length

    let filterOutJokers hand = 
        hand |> Seq.filter (fun x -> x <> joker) 
             |> String.Concat 

    let compareRule1 ((hand1,_):string*int) ((hand2,_):string*int): int =
        let j1, j2 = countJokers hand1, countJokers hand2 
        let handNoJokers1, handNoJokers2 = filterOutJokers hand1, filterOutJokers hand2 
        let group1, group2 = groupHand handNoJokers1, groupHand handNoJokers2 
        let groupWithJokers1 = if group1.IsEmpty then [j1] 
                               else group1[0] + j1 :: group1.Tail
        let groupWithJokers2 = if group2.IsEmpty then [j2] 
                               else group2[0] + j2 :: group2.Tail
        findWinner groupWithJokers1 groupWithJokers2 hand1 hand2

    let rankCards cards =
        cards |> Array.toList 
              |> List.sortWith compareRule1 
              |> List.mapi (fun i (_,bet) -> (i+1, bet))

    let productSum cards = 
        cards 
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
