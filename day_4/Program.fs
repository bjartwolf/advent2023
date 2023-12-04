module Input =
    open System
    open Xunit 

    let readInit (filePath: string) = 
        System.IO.File.ReadAllLines filePath

    type CardId = CardId of int
    type Card = { Id: CardId ; Winning: int list; Scratched : int list}
    let example_card = { Id = CardId 1; Winning = [41;48;83;86;17]; Scratched = [83;86;6;31;17;9;48;53] } 

    let getCardValue (card: Card): int = match card.Id with | CardId value -> value
    let getCardIdValue (id: CardId): int = match id with | CardId value -> value

    let parseCard (s:string): Card = 
        let card_numbers = s.Split(":")
        let nr = int (card_numbers[0].Replace("Card ",""))
        let numbers = card_numbers[1].Split("|")
        let parseNrs (numTxt:string[]) = numTxt |> Array.map (fun s -> s.Replace(" ", "")) |> Array.where (fun s -> not (String.IsNullOrEmpty(s))) |> Array.map int |> Array.toList
        let winningNumbers = numbers[0].Split(" ")  |> parseNrs
        let scracthed = numbers[1].Split(" ")  |> parseNrs
        {
            Id = CardId nr 
            Winning = winningNumbers;
            Scratched = scracthed 
        }

    let parseCards (s: string[]): Card list = 
        s |> Array.map parseCard |> Array.toList

    let countWinning (card: Card): CardId list =
        let nrWins = Set.intersect (Set.ofList card.Winning) (Set.ofList card.Scratched) |> Seq.toList |> Seq.length
        let nextIds = [(getCardValue card + 1) .. (getCardValue card + nrWins )] |> List.take nrWins
        nextIds |> List.map CardId 

    let getNextRound (cards: Card list) : CardId list =
        cards |> List.map countWinning  |> List.collect id

    let rec countCards (cards: Card list) (hand: CardId list) (score: int): int =
        let newScore = score + hand.Length 
        let cardsOnHand = hand |> List.map getCardIdValue |> List.map (fun x -> cards[x - 1])
        let nextWins = getNextRound cardsOnHand 
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

module Program = let [<EntryPoint>] main _ = 0
