module Input =
    open Xunit 

    let readInit (filePath: string): string array = 
        System.IO.File.ReadAllLines filePath

    let numbers = ["zero", "0"; "one", "1"; "two", "2"; "three", "3"; "four", "4"; "five", "5"; "six", "6";"seven", "7"; "eight", "8"; "nine","9" ] 
    let numbersMap = numbers |> Map.ofList 
    let textNumbers = numbers |> List.map fst
    let digits = numbers |> List.map snd 

    let findTxtNumberBy (comparer : int list -> int) (line: string): (int * string) option =
        try 
            let positions = textNumbers |> List.map (fun nr -> line.IndexOf(nr)) 
            let positionsFound = positions |> List.where (fun x -> x >=0)
            let minPositionFound = positionsFound |> comparer 
            let indexOfMin = List.findIndex (fun x -> x = minPositionFound) positions
            Some(minPositionFound, textNumbers[indexOfMin])
        with _ ->
            None

    let firstTxtNumber (line: string): (int * string) option =
        findTxtNumberBy List.min line 

    let lastTxtNumber(line: string): (int * string) option =
        findTxtNumberBy List.max line 

    [<Fact>]
    let firstText() = 
        let input = "7pqrstsixteen" 
        Assert.Equal(Some (6,"six"), firstTxtNumber input) 

    let findDigitNumberBy (comparer: int list -> int) (line: string): (int * string) option =
        try 
            let positions = digits |> List.map (fun nr -> line.IndexOf(nr)) 
            let positionsFound = positions |> List.where (fun x -> x >=0)
            let minPositionFound = positionsFound |> comparer 
            let indexOfMin = List.findIndex (fun x -> x = minPositionFound) positions
            Some (minPositionFound, digits[indexOfMin])
        with _ ->
            None


    let firstDigitNumber (line: string): (int * string) option =
        findDigitNumberBy List.min line

    let lastDigitNumber (line: string): (int * string) option =
        findDigitNumberBy List.max line

    [<Fact>]
    let firstDigitNumberTest() = 
        let input = "7pqrstsixteen" 
        Assert.Equal(Some (0,"7"), firstDigitNumber input) 

    [<Fact>]
    let firstDigitNumberTest2() = 
        let input = "foo7pqrstsixteen" 
        Assert.Equal(Some (3,"7"), firstDigitNumber input) 

    [<Fact>]
    let Lastdigittest1() = 
        let input = "i8xteen" 
        Assert.Equal(Some (1,"8"), lastDigitNumber input) 

    let firstAndLast (inputline: string): int=
        let firstDigit = firstDigitNumber inputline
        let lastDigit = lastDigitNumber inputline
        let firstTxt = firstTxtNumber inputline
        let lastTxt = lastTxtNumber inputline

        let firstNumber = match (firstDigit, firstTxt) with
                                    | Some (i,x), Some (j,_) when i < j -> x 
                                    | Some (i,_), Some (j,y) when i > j -> numbersMap.[y] 
                                    | None, Some (j,y) -> numbersMap.[y] 
                                    | Some(i,x), None -> x 
                                    | _ -> failwith "oopsy"

        let lastNumber = match (lastDigit, lastTxt) with
                                    | Some (i,x), Some (j,y) when i > j -> x 
                                    | Some (i,x), Some (j,y) when i < j -> numbersMap.[y] 
                                    | None, Some (j,y) -> numbersMap.[y] 
                                    | Some(i,x), None -> x 
                                    | _ -> failwith "oopsy"

        int(firstNumber + lastNumber)

    [<Fact>]
    let testi1 () = 
        let input = "two1nine" 
        Assert.Equal(29, firstAndLast input) 

    [<Fact>]
    let testi1x () = 
        let input = "twone" 
        Assert.Equal(21, firstAndLast input) 

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
        Assert.Equal(53888, sum) 

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(1000, input.Length) 
module Program = let [<EntryPoint>] main _ = 0
