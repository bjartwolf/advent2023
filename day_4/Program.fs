open System.Collections.Generic
open System.Text.RegularExpressions

module Input =
    open Xunit 

    let readInit (filePath: string) = 
        System.IO.File.ReadAllLines filePath

    type CardId = int
    type Card = int 

    // Borrowed this from Einar after I had solved it, cute parser
    let parseCard (line : string) : int = 
        let folder (seen, dups) x = 
            if List.contains x seen then (seen, x :: dups) else (x :: seen, dups)
        let findDuplicates lst = 
            lst |> List.fold folder ([], []) |> snd
        Regex.Matches(line, "\d+") 
        |> Seq.map (fun m -> m.Value)
        |> Seq.tail 
        |> Seq.toList
        |> findDuplicates
        |> List.length 

    let parseCards (s: string[]): Card list = s |> Array.map parseCard |> Array.toList
        
    // not original, tried a faster way
    let countScore (cardList: Card list): int =
        let dict = Dictionary<int,int>()
        cardList.Length +
            ([ for i in [(cardList.Length - 1) .. -1 .. 0] do
                   let mutable wins = cardList[i] 
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
