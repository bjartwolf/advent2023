open System.Collections.Generic


module Input =
    open System
    open Xunit 

    let readInit (filePath: string) = 
        System.IO.File.ReadAllLines filePath

    type CardId = int
    type Card = { Id: CardId ; Winning: int list; Scratched : int list}

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

    let countWinning (card: Card): int =
        Set.intersect (Set.ofList card.Winning) (Set.ofList card.Scratched) |> Seq.length

        
    let countScore (cardList: Card list): int =
        let dict = Dictionary<int,int>()
        cardList.Length +
            ([ for i in [(cardList.Length - 1) .. -1 .. 0] do
                   let mutable wins = countWinning cardList[i]
                   [ for j in [(i + 1) .. i + wins] do
                            wins <- wins + dict[j]
                   ] |> ignore
                   dict.Add(i, wins)
                   yield wins 
            ] |> List.sum )
         

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
