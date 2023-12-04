open System.Collections.Generic


module Input =
    open System
    open Xunit 

    let readInit (filePath: string) = 
        System.IO.File.ReadAllLines filePath

    type CardId = int
    type Card = { Id: CardId ; Winning: int list; Scratched : int list}
    let example_card = { Id = 1; Winning = [41;48;83;86;17]; Scratched = [83;86;6;31;17;9;48;53] } 

    let getCardValue (card: Card): int = card.Id 
    let getCardIdValue (id: CardId): int = id 

    let parseCard (s:string): Card = 
        let card_numbers = s.Split(":")
        let nr = int (card_numbers[0].Replace("Card ",""))
        let numbers = card_numbers[1].Split("|")
        let parseNrs (numTxt:string[]) = numTxt |> Array.map (fun s -> s.Replace(" ", "")) |> Array.where (fun s -> not (String.IsNullOrEmpty(s))) |> Array.map int |> Array.toList
        let winningNumbers = numbers[0].Split(" ")  |> parseNrs
        let scracthed = numbers[1].Split(" ")  |> parseNrs
        {
            Id = nr 
            Winning = winningNumbers;
            Scratched = scracthed 
        }

    let parseCards (s: string[]): Card list = 
        s |> Array.map parseCard |> Array.toList

    let memoize f =
        let dict = Dictionary<_, _>();
        fun c ->
            let exist, value = dict.TryGetValue c
            match exist with
            | true -> value
            | _ -> 
                let value = f c
                dict.Add(c, value)
                value

    let countWinning (card: Card): CardId list =
        let nrWins = Set.intersect (Set.ofList card.Winning) (Set.ofList card.Scratched) |> Seq.toList |> Seq.length
        let nextIds = [(getCardValue card + 1) .. (getCardValue card + nrWins )] |> List.take nrWins
        nextIds 

    let countWinningById (cards: Card list) (cardId: CardId): CardId list =
        let card = cards[cardId - 1 ]
        let nrWins = Set.intersect (Set.ofList card.Winning) (Set.ofList card.Scratched) |> Seq.toList |> Seq.length
        let nextIds = [(getCardValue card + 1) .. (getCardValue card + nrWins )] |> List.take nrWins
        nextIds

    let memoedWinning (cards: Card list) =
        memoize (countWinningById cards)

    let getNextRound (cards: Card list) : CardId list =
        cards |> List.map countWinning  |> List.collect id

    let rec countCards (cards: Card list) (hand: CardId list) (score: int): int =
        let memoCounter = memoedWinning cards
        let newScore = score + hand.Length 
        let nextWins = hand |> List.map (fun x -> memoCounter x) |> List.collect id
        if nextWins |> List.isEmpty then 
            newScore 
        else 
            newScore + (countCards cards nextWins score)

    let countScore (cardList: Card list): int =
        countCards cardList (cardList |> List.map (fun x -> x.Id)) 0

    [<Fact>]
    let countWinningTest() =
        let input = readInit "testinput.txt" |> parseCards
        let winningCount = countScore input 
        Assert.Equal(30, winningCount)

    [<Fact>]
    let countWinningTest2() =
        let input = readInit "input.txt" |> parseCards
        let winningCount = countScore input 
        Assert.Equal(8467762, winningCount)


module Program = let [<EntryPoint>] main _ = 0
