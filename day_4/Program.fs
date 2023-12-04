module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string) = 
        System.IO.File.ReadAllLines filePath

    // first winning numbers, then numbers you have
    // can be split on lines and newlines easier than regex
    let example_line = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    type CardId = CardId of int
    type Card = { Id: CardId ; Winning: int list; Scratched : int list}
    let example_card = { Id = CardId 1; Winning = [41;48;83;86;17]; Scratched = [83;86;6;31;17;9;48;53] } 

    let getCardValue (card: Card): int = match card.Id with | CardId value -> value

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

    [<Fact>]
    let parseLine_card1() = 
        Assert.Equal(example_card, parseCard example_line)

    let countWinning (card: Card): CardId list =
        let nrWins = Set.intersect (Set.ofList card.Winning) (Set.ofList card.Scratched) |> Seq.toList |> Seq.length
        let nextIds = [(getCardValue card + 1) .. (getCardValue card + nrWins )] |> List.take nrWins
        nextIds |> List.map CardId 

    [<Fact>]
    let testCountWinning1() =
        let cards = readInit "testinput.txt" |> parseCards 
        let wins1 = countWinning cards[0]
        let wins2 = countWinning cards[1]
        let wins3 = countWinning cards[2]
        let wins4 = countWinning cards[3]
        let wins5 = countWinning cards[4]
        let wins6 = countWinning cards[5]
        Assert.Equal<CardId list>([2;3;4;5] |> List.map CardId, wins1)
        Assert.Equal<CardId list>([3;4] |> List.map CardId, wins2)
        Assert.Equal<CardId list>([4;5] |> List.map CardId, wins3)
        Assert.Equal<CardId list>([5] |> List.map CardId, wins4)
        Assert.Equal<CardId list>([] |> List.map CardId, wins5)
        Assert.Equal<CardId list>([] |> List.map CardId, wins6)

    let countWinnings (cards: Card list) (hand: CardId list): CardId list list =
        cards |> List.map countWinning 

    let playRound (cards: Card list) (start: CardId list): CardId list =
        start |> countWinnings cards |> List.collect id 

(* 
    [<Fact>]
    let countWinningTest() =
        let input = readInit "testinput.txt" |> parseCards
        let winningCount = countWinnings input
        Assert.Equal<int list>([4;2;2;1;0;0], winningCount)
*)
    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(6, input.Length) 

    [<Fact>]
    let test3 () = 
        let input = readInit "input.txt" 
        Assert.Equal(220, input.Length) 


module Program = let [<EntryPoint>] main _ = 0