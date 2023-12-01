open System.Text.RegularExpressions


module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): string array = 
        System.IO.File.ReadAllLines filePath

    let numbers = ["zero", "0"; "one", "1"; "two", "2"; "three", "3"; "four", "4"; "five", "5"; "six", "6";"seven", "7"; "eight", "8"; "nine","9" ] 
    let numbersMap = numbers |> Map.ofList 
    let textNumbers = numbers |> List.map fst

    let textNumberInLine (word: string): bool =
        let matchingNumbers = textNumbers |> List.where (fun nr -> word.Contains(nr)) 
        matchingNumbers|> List.isEmpty|> not

    let replaceOnce (line: string) (textNr: string) (digit: string) =
        let pos = line.IndexOf(textNr)
        if (pos >= 0) then
            line.Substring(0,pos) + digit + line.Substring(pos+textNr.Length)
        else 
            line

    let firstNumber (line: string): string =
        let positions = textNumbers |> List.map (fun nr -> line.IndexOf(nr)) 
        let positionsFound = positions |> List.where (fun x -> x >=0)
        let minPositionFound = positionsFound |> List.min
        let indexOfMin = List.findIndex (fun x -> x = minPositionFound) positions
        textNumbers[indexOfMin] 

    [<Fact>]
    let testFirstNumber () = 
        Assert.Equal("seven", firstNumber "sevenseven") 

    [<Fact>]
    let testFirstNumber2 () = 
        Assert.Equal("seven", firstNumber "dfadsevenseven") 


    [<Fact>]
    let testreplace () = 
        Assert.Equal("7seven", replaceOnce "sevenseven" "seven" "7") 
        Assert.Equal("77", replaceOnce "7seven" "seven" "7") 

    [<Fact>]
    let testreplace2 () = 
        Assert.Equal("8wothree", replaceOnce "eightwothree" "eight" "8") 
        Assert.Equal("8wo3", replaceOnce "8wothree" "three" "3") 

    let firstAndLast (inputline: string): int=
        let mutable line: string = inputline 

        while (textNumberInLine line) do
            let nrToReplace = firstNumber line 
            line <- replaceOnce line nrToReplace (numbersMap.[nrToReplace])
           
        let mutable firstNumber : int option = None 
        let mutable lastNumber: int option = None 
        
        for char in line do
            ()
            let mutable parsedValue = 0 
            let couldParse = Int32.TryParse(string char, &parsedValue)
            if couldParse then
                if firstNumber.IsNone then
                    firstNumber <- Some parsedValue

        let backWardsLine = String.Concat(Seq.rev line)
        for char in backWardsLine do
            ()
            let mutable parsedValue = 0 
            let couldParse = Int32.TryParse(string char, &parsedValue)
            if couldParse then
                if lastNumber.IsNone then
                    lastNumber<- Some parsedValue

        match (firstNumber, lastNumber) with
            | Some nr1, Some nr2  -> int (sprintf "%i%i" nr1 nr2)
            | _,_ -> failwith "Did not find numbers"
    [<Fact>]
    let testi1 () = 
        let input = "two1nine" 
        Assert.Equal(29, firstAndLast input) 

    [<Fact>]
    let testi2 () = 
        let input = "eightwothree" 
        Assert.Equal(83, firstAndLast input) 

    [<Fact>]
    let testi3 () = 
        let input = "abcone2threexyz" 
        Assert.Equal(13, firstAndLast input) 

    [<Fact>]
    let testi4 () = 
        let input = "xtwone3four" 
        Assert.Equal(24, firstAndLast input) 

    [<Fact>]
    let testi5 () = 
        let input = "4nineeightseven2" 
        Assert.Equal(42, firstAndLast input) 

    [<Fact>]
    let testi6 () = 
        let input = "zoneight234" 
        Assert.Equal(14, firstAndLast input) 

    [<Fact>]
    let testi7 () = 
        let input = "7pqrstsixteen" 
        Assert.Equal(76, firstAndLast input) 


    [<Fact>]
    let testsum () = 
        let input = readInit "testinput.txt" 
        let sum = input |> Array.map (fun line -> firstAndLast line) |> Array.sum
        Assert.Equal(281, sum) 

    [<Fact>]
    let testsum2 () = 
        let input = readInit "input1.txt" 
        let sum = input |> Array.map (fun line -> firstAndLast line) |> Array.sum
        Assert.Equal(53896, sum) 

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(1000, input.Length) 
module Program = let [<EntryPoint>] main _ = 0
