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

    let addJokers cards jokers = 
        match cards with
            | h::tail -> h+jokers::tail
            | [] -> [jokers]

    let compareRule1 (score1:int,hand1:string,_) (score2:int, hand2:string,_): int =
       if score1 > score2 then 1
       else if score1 < score2 then -1
       else compareRule2 hand1 hand2

    let calculateScore hand1 : int = 
        let jokers = countJokers hand1
        let filteredJokers = filterOutJokers hand1
        let groupedCards = groupHand filteredJokers 
        let addedJokers = addJokers groupedCards jokers 
        scoreHand addedJokers

    let rankCards cards =
        cards |> Array.toList 
              |> List.map (fun (hand,bet) -> (calculateScore hand,hand,bet) )
              |> List.sortWith compareRule1 
              |> List.mapi (fun i (_,_,bet) -> (i+1, bet))

    let productSum cards = cards |> List.map (fun (x,y) -> x * y) |> List.sum 

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
