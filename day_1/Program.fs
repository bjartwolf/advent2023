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

    [<Fact>]
    let indexestest() = 
        let line = "eightsevenfooneeight"
        let nr = "eight"
        let indexes = indexOfNumber line nr 
        Assert.True([0;15] = indexes)

    let firstTxtNumber (line: string): (int * int) option =
        try 
            let ofoo = textNumbers |> List.mapi (fun i number -> (i, indexOfNumber line number))  |> List.map (fun (x,y) -> if y.IsEmpty then (x,None ) else (x, Some (List.min y)))
            let (y,i)= ofoo |> List.minBy (fun (_, y) -> match y with | Some x -> x | None -> 999)
            match i with 
                | Some i -> Some(i, y)
                | None -> None
        with ex ->
            None
    [<Fact>]
    let testsss1 () = 
        let input = "two1nine" 
        Assert.Equal(Some(0,2), firstTxtNumber input) 

    let lastTxtNumber(line: string): (int * int) option =
        try 
            let ofoo = textNumbers |> List.mapi (fun i number -> (i, indexOfNumber line number))  |> List.map (fun (x,y) -> if y.IsEmpty then (x,None ) else (x, Some (List.max y)))
            let (y,i)= ofoo |> List.maxBy (fun (_, y) -> match y with | Some x -> x | None -> -999)
            match i with 
                | Some i -> Some(i, y)
                | None -> None
        with ex ->
            None

    [<Fact>]
    let firstText() = 
        let input = "7pqrstsixteen" 
        Assert.Equal(Some (6,6), firstTxtNumber input) 

    [<Fact>]
    let LastText() = 
        let input = "5hrlslhpnine4seven4bzjtteightveighttv4" 
        Assert.Equal(Some (30,8), lastTxtNumber input) 
    
    let isNumber (char: char) : bool =
        digits |> List.contains (char.ToString())

    [<Fact>]
    let IsNrTest() = 
        let input = char '7' 
        Assert.Equal(true, isNumber input) 

    [<Fact>]
    let IsNrTest2() = 
        let input = char 's' 
        Assert.Equal(false, isNumber input) 

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


    [<Fact>]
    let LastDigitTest() = 
        let input = "5hrlslhpnine4seven4bzjttvtv4" 
        Assert.Equal(Some ((input.Length - 1),"4"), lastDigitNumber input) 

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
    let testi7x () = 
        let input = "5hrlslhpnine4seven4bzjttvtv4" 
        Assert.Equal(54, firstAndLast input) 

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
        Assert.Equal(53894, sum) 

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(1000, input.Length) 

module Program = let [<EntryPoint>] main _ = 0
