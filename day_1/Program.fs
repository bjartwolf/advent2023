module Input =
    open Xunit 

    let readInit (filePath: string): string array = 
        System.IO.File.ReadAllLines filePath

    let numbers = ["zero", "0"; "one", "1"; "two", "2"; "three", "3"; "four", "4"; "five", "5"; "six", "6";"seven", "7"; "eight", "8"; "nine","9" ] 
    let numbersMap = numbers |> Map.ofList 
    let textNumbers = numbers |> List.map fst
    let digits = numbers |> List.map snd 

    let indexOfNumber (inputString: string) (substring: string) =
        let rec findIndexes (startIndex: int) (acc: int list) =
            match inputString.IndexOf(substring, startIndex) with
            | -1 -> List.rev acc
            | index -> findIndexes (index + substring.Length) (index::acc)
        findIndexes 0 []

    let firstTxtNumber (line: string): (int * int) option =
        try 
            let ofoo = textNumbers |> List.mapi (fun i number -> (i, indexOfNumber line number))  |> List.map (fun (x,y) -> if y.IsEmpty then (x,None ) else (x, Some (List.min y)))
            let (y,i)= ofoo |> List.minBy (fun (_, y) -> match y with | Some x -> x | None -> 999)
            match i with 
                | Some i -> Some(i, y)
                | None -> None
        with ex ->
            None

    let lastTxtNumber(line: string): (int * int) option =
        try 
            let ofoo = textNumbers |> List.mapi (fun i number -> (i, indexOfNumber line number))  |> List.map (fun (x,y) -> if y.IsEmpty then (x,None ) else (x, Some (List.max y)))
            let (y,i)= ofoo |> List.maxBy (fun (_, y) -> match y with | Some x -> x | None -> -999)
            match i with 
                | Some i -> Some(i, y)
                | None -> None
        with ex ->
            None

    let isNumber (char: char) : bool =
        digits |> List.contains (char.ToString())

    let findDigitNumberBy (comparer: int list -> int) (line: string): (int * string) option =
        try 
            let txtThatAreNumbers = line |> Seq.toList |> List.mapi (fun i x -> ((isNumber x), i)) |> List.filter (fun (isNum, _) -> isNum) |> List.map (fun (_,i) -> i)
            let position = txtThatAreNumbers |> comparer
            let lastDigit = line |> Seq.toList |> List.item position
            Some (position, lastDigit.ToString())
        with _ ->
            None

    let firstDigitNumber (line: string): (int * string) option =
        findDigitNumberBy List.min line

    let lastDigitNumber (line: string): (int * string) option =
        findDigitNumberBy List.max line

    let firstAndLast (inputline: string): int=
        let firstDigit = firstDigitNumber inputline
        let firstTxt = firstTxtNumber inputline

        let lastDigit = lastDigitNumber inputline
        let lastTxt = lastTxtNumber inputline

        let firstNumber = match (firstDigit, firstTxt) with
                                    | Some (i,x), Some (j,_) when i < j -> x 
                                    | Some (i,_), Some (j,y) when i > j -> y.ToString() 
                                    | None, Some (_,y) -> y.ToString()
                                    | Some(_,x), None -> x.ToString()
                                    | _ -> failwith "oopsy"

        let lastNumber = match (lastDigit, lastTxt) with
                                    | Some (i,x), Some (j,_) when i > j -> x 
                                    | Some (i,_), Some (j,y) when i < j -> y.ToString()
                                    | None, Some (_,y) -> y.ToString() 
                                    | Some(_,x), None -> x 
                                    | _ -> failwith "oopsy"

        int(firstNumber + lastNumber)

    [<Fact>]
    let testsum2 () = 
        let input = readInit "input1.txt" 
        let sum = input |> Array.map (fun line -> firstAndLast line) |> Array.sum
        Assert.Equal(53894, sum) 

module Program = let [<EntryPoint>] main _ = 0
