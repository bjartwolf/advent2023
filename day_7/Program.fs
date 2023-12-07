module Program =
    open System
    open Xunit 

    let joker = '1'

    let readInit filePath: (string*int) [] = 
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

        IO.File.ReadAllLines filePath
            |> Array.map (fun x -> let a = x.Split(" ")
                                   (replaceWithAsciiValues a[0]),int a[1])

    let calculateScore hand : int = 
        let filterOutJokers hand = 
            hand |> Seq.filter (fun x -> x <> joker) |> String.Concat 

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

        let addJokers jokers cards = 
            match cards with
                | h::tail -> h+jokers::tail
                | [] -> [jokers]

        let scoreHand (hand: int list) = 
           match hand with 
                    | [5] -> 6
                    | 4::_ -> 5
                    | [3;2] -> 4
                    | 3::_ -> 3
                    | [2;2;1] -> 2
                    | 2::_ -> 1
                    | _ -> 0

        hand |> filterOutJokers 
             |> groupHand 
             |> addJokers (countJokers hand)
             |> scoreHand 

    let compareRule (score1:int,hand1:string,_) (score2:int, hand2:string,_): int =
        let compareCards (hand1:string) (hand2:string): int =
            String.Compare(hand1, hand2, StringComparison.Ordinal)
        if score1 > score2 then 1
            else if score1 < score2 then -1
            else compareCards hand1 hand2

    let rankCards cards =
        cards |> Array.toList 
              |> List.map (fun (hand,bet) -> (calculateScore hand,hand,bet) )
              |> List.sortWith compareRule 
              |> List.mapi (fun i (_,_,bet) -> (i+1, bet))

    let productSum cards = cards |> List.map (fun (x,y) -> x * y) 
                                 |> List.sum 

    [<Fact>]
    let sumRanked() = 
        let sum = readInit "testinput.txt" 
                    |> rankCards 
                    |> productSum 
        Assert.Equal(5905, sum) 

    [<Fact>]
    let sumRankedprod() = 
        let sum = readInit "input.txt" 
                    |> rankCards 
                    |> productSum 
        Assert.Equal(248750248, sum) 

    let [<EntryPoint>] main _ = 0
