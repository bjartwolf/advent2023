module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string) = 
        System.IO.File.ReadAllLines filePath

    // first winning numbers, then numbers you have
    // can be split on lines and newlines easier than regex
    let example_line = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    type Card = { Id: int ; Winning: int list; Scratched : int list}
    let example_card = { Id = 1; Winning = [41;48;83;86;17]; Scratched = [83;86;6;31;17;9;48;53] } 

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

    [<Fact>]
    let parseLine_card1() = 
        Assert.Equal(example_card, parseCard example_line)

    let countWinning (card: Card): int =
        Set.intersect (Set.ofList card.Winning) (Set.ofList card.Scratched) |> Seq.toList |> List.length

    let countWinnings (card: Card list): int list =
        card |> List.map countWinning 

    let parseCards (s: string[]): Card list = 
        s |> Array.map parseCard |> Array.toList

    let scoreWinning (wins: int): int =
        if wins = 0 then 0
        else pown 2 (wins - 1)

    [<Fact>]
    let scoreWinningsTest () = 
        Assert.Equal(0,scoreWinning 0)
        Assert.Equal(1,scoreWinning 1)
        Assert.Equal(2,scoreWinning 2)
        Assert.Equal(4,scoreWinning 3)
        Assert.Equal(8,scoreWinning 4)

    let scoreWinnings (wins: int list): int =
        wins |>List.map scoreWinning |> List.sum

    [<Fact>]
    let countWinningsScoresTest() =
        let input = readInit "testinput.txt" |> parseCards
        let winningCount = countWinnings input
        let scores = scoreWinnings winningCount 
        Assert.Equal(13, scores)

    [<Fact>]
    let countWinningsScoresReal() =
        let input = readInit "input.txt" |> parseCards
        let winningCount = countWinnings input
        let scores = scoreWinnings winningCount 
        Assert.Equal(26346, scores)


    [<Fact>]
    let countWinningTest() =
        let input = readInit "testinput.txt" |> parseCards
        let winningCount = countWinnings input
        Assert.Equal<int list>([4;2;2;1;0;0], winningCount)

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(6, input.Length) 

    [<Fact>]
    let test3 () = 
        let input = readInit "input.txt" 
        Assert.Equal(220, input.Length) 


module Program = let [<EntryPoint>] main _ = 0