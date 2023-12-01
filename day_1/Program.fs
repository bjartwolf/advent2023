module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): string array = 
        System.IO.File.ReadAllLines filePath

    let firstAndLast (line: string): int=
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
        let input = "1abc2" 
        Assert.Equal(12, firstAndLast input) 

    [<Fact>]
    let testi2 () = 
        let input = "pqr3stu8vwx" 
        Assert.Equal(38, firstAndLast input) 

    [<Fact>]
    let testi3 () = 
        let input = "a1b2c3d4e5f" 
        Assert.Equal(15, firstAndLast input) 

    [<Fact>]
    let testi4 () = 
        let input = "treb7uchet" 
        Assert.Equal(77, firstAndLast input) 

    [<Fact>]
    let testsum () = 
        let input = readInit "testinput.txt" 
        let sum = input |> Array.map (fun line -> firstAndLast line) |> Array.sum
        Assert.Equal(142, sum) 

    [<Fact>]
    let testsum2 () = 
        let input = readInit "input1.txt" 
        let sum = input |> Array.map (fun line -> firstAndLast line) |> Array.sum
        Assert.Equal(53651, sum) 


    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(1000, input.Length) 

module Program = let [<EntryPoint>] main _ = 0