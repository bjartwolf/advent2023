module Input =
    open System
    open System.IO
    open Xunit 

    let square = 1
    let round = 2
    let space = 3

    let readInit (filePath: string): int array list = 
        let lines = File.ReadAllLines filePath
        let lengthOfLine = lines[0].Length
        [
            for i in 0 .. lengthOfLine - 1 do
                // i'th character of all lines into 
                let col = lines |> Array.map (fun x -> x.[i])
                                |> Array.map (fun c -> if c = '.' then space 
                                                       else if c = '#' then square
                                                       else if c = 'O' then round
                                                       else failwith "whoopsy")
                yield col 
        ]


    let row1 = [|1;3;3;2;2;1;3;3;2;2;3;2|] 

    let splitRowAtRocks (input: int[]) : int list list =
        let mutable tmpGroup: int list = []
        let mutable readingGroup = false

        [ for i = 0 to Array.length input - 1 do
              let current = input[i]
              if (not readingGroup) then
                tmpGroup <- [current] 
                readingGroup <- true
              else if (readingGroup && current = square) then
                yield tmpGroup
                tmpGroup <- [square] 
              else if (readingGroup && current <> square) then
                tmpGroup <- tmpGroup @ [current]
              if i = Array.length input - 1 then
                yield tmpGroup
       ] 

    let sortRowAndJoin (splitRow: int list list) =
        splitRow |> List.map List.sort
                 |> List.collect id
    
    let splitAndSort (input: int[]) : int []=
        splitRowAtRocks input
            |> sortRowAndJoin
            |> List.toArray

 
    let splitAndSortMatrix (input: (int[] list)): (int[] list) =
        input |> List.map splitAndSort
    

    let calcLoad (input: int []): int =
        input |> Array.rev
              |> Array.mapi (fun i elem -> if elem = round then i + 1 else 0)
              |> Array.sum

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        let sum = input |> splitAndSortMatrix 
                        |> List.map calcLoad 
                        |> List.sum
        Assert.Equal(136, sum) 

    [<Fact>]
    let testprod () = 
        let input = readInit "input.txt" 
        let sum = input |> splitAndSortMatrix 
                        |> List.map calcLoad
                        |> List.sum
        Assert.Equal(109638, sum) 


module Program = let [<EntryPoint>] main _ = 0