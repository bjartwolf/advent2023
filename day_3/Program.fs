open System.Collections.Generic
open System


module Input =
    open Xunit 

    let digitCharCodes = [0..9] |> Seq.map (fun x -> byte x + 48uy) |> Seq.toArray
    let dotCharCode = byte '.'

    let readLinesAsTxt (filePath: string) : string[] = 
        System.IO.File.ReadAllLines(filePath)

    let readInit (filePath: string) : byte [][] = 
        let lines = System.IO.File.ReadAllLines(filePath)
        lines |> Array.map (fun x -> x |> Seq.toArray |> Seq.map byte |> Seq.toArray)

    let keepSymbols (char:byte) : byte =
        if Seq.contains char digitCharCodes then 0uy
        else if char = dotCharCode then 0uy
        else 255uy 

    [<Fact>]
    let keepSymbolsDeleteNumbers() =
        Assert.Equal(0uy, keepSymbols digitCharCodes.[3])

    [<Fact>]
    let keepSymbolsKeepSymbol() =
        Assert.Equal(255uy, keepSymbols (byte '$'))

    [<Fact>]
    let keepSymbolsDeleteDots() =
        Assert.Equal(0uy, keepSymbols (byte '.'))

    (* get symbols in the mask, no neighbourhood *)
    let getSymbolMask (input: byte[][]): byte[][] =
        input |> Array.map (fun x -> x |> Array.map keepSymbols)


    let copy (input: byte[][]) =
        input |> Array.map (fun x -> Array.copy x)

    (* expand symbolmask to set all neighbours to 255uy *) 
    // quadratic, so cheating
    let getSymbolMaskNeighbors (input: byte[][]): byte[][] =
         let newMask = copy input
         let iterend = input.Length - 1
         for i in 0..iterend do
            for j in 0.. iterend do
                if input[i][j] = 255uy then
                    let x = [-1 .. 1]
                    let y = [-1 .. 1]
                    for deltaX in x do
                        for deltaY in y do
                            if i + deltaX >= 0 && i + deltaX <= iterend && j + deltaY >= 0 && j + deltaY <= iterend then
                                newMask[i+deltaX][j+deltaY] <- 255uy
         newMask 

    (* this does not really work, any part of the number must touch the bitmask*)
    (* OR if I just keep on pushing what if I can check which numbers where not mutilated by this and then keep those*)
    let applyBitMask (input: byte[][]) (mask: byte[][]): byte[][] = 
        let keepCode code =  
            if code = 0uy then 
                dotCharCode 
            else if not (Seq.contains code digitCharCodes) then
                    dotCharCode 
            else code

        let maskLine (line: byte[]) (maskLine: byte[]): byte[] = Array.map2 (&&&) line maskLine |> Array.map keepCode
        let output = copy input
        for i in 0..input.Length-1 do
             output[i] <- maskLine input[i] mask[i] 
        output

    (* This could use some refactoring *)
    let groupPredicateWithPosition predicate (input: byte[]) : (int * byte[]) list =
        let groups: Dictionary<int, byte[]> =  Dictionary<int,byte[]>()
        let mutable currentFoundIndex = -1
        let mutable foundNr = false

        for i = 0 to Array.length input - 1 do
            if predicate input.[i] && foundNr then
                groups.[currentFoundIndex] <- Array.append groups.[currentFoundIndex] [| input.[i] |] 
            else if (predicate input.[i]) && (not foundNr) then
                foundNr <- true
                currentFoundIndex <- i
                groups.Add(i,[| input.[i] |])
                ()
            else
                foundNr <- false
        groups |> Seq.map (fun key -> (key.Key, key.Value)) |> Seq.toList

    [<Fact>]
    let testGrouper() =
        let predicate x = x >= 3uy && x <=9uy
        let input = [| 2uy;3uy;9uy;10uy;2uy;4uy;6uy |]
        let expected = [(1, [|3uy;9uy|]); (5, [|4uy;6uy|])]
        let groupedNumbers = groupPredicateWithPosition predicate input
        Assert.Equal<(int*byte[]) list>(expected, groupedNumbers )
 
    [<Fact>]
    let testBitMask () = 
        let input = [| [| 49uy; 12uy|]; [| 21uy; 50uy|]  |]
        let mask = [| [| 255uy; 0uy|]; [| 0uy; 255uy|]  |]
        let filtered = applyBitMask input mask
        Assert.Equal(49uy, filtered[0][0])
        Assert.Equal(dotCharCode, filtered[0][1])
        Assert.Equal(dotCharCode, filtered[1][0])
        Assert.Equal(50uy, filtered[1][1])

    let matrixToText (input: byte[][]): string[] = 
        input |> Array.map (fun x -> System.String (x |> Seq.map char |> Seq.toArray))

    [<Fact>]
    let test2 () = 
        let input = readInit "input.txt" 
        Assert.Equal(140, input.Length) 

    let getFilteredNumbers (input: byte[][]): byte[][] =
        let symbols = getSymbolMask input
        let mask = getSymbolMaskNeighbors symbols 
        applyBitMask input mask

    let getNumbersWithPositions (input: byte[][]): (int*byte[]) list =
        let length= input.Length
        let foo = input |> Array.map (fun a -> a |> groupPredicateWithPosition (fun x -> Seq.contains x digitCharCodes))  |> Array.toList
        foo |> List.mapi (fun i x ->  x |> List.map (fun (j,y) -> (j+i*length,y))) |>List.collect (id) 

    (* should do this but requirs padding and stuff*)
    let toInt (bytes : byte []) =
        bytes |> Array.map char |> System.String |> int

    [<Fact>]
    let makeIntFromBytes() = 
        let bytes = [| 52uy;54uy;55uy|]
        let nr = toInt bytes 
        Assert.Equal(467, nr)

    [<Fact>]
    let getNumbersWithPositionsTest () = 
        let input = [| [| 49uy; 12uy|]; [| 21uy; 50uy|]  |]
        let numbers = getNumbersWithPositions input
        let (_,firstNumber) = List.find (fun (x,y) -> x = 0) numbers
        Assert.Equal<byte[]>([|49uy|], firstNumber)
        let (_,secondNumber) = List.find (fun (x,y) -> x = 3) numbers
        Assert.Equal<byte[]>([|50uy|], secondNumber)

    let getNumbers (input: byte[][]): int list =
        let initialNumbers = getNumbersWithPositions input |> Set.ofList
        let filteredNumbers = getNumbersWithPositions (getFilteredNumbers input) |> Set.ofList

        let uniqueElements = Set.intersect initialNumbers filteredNumbers 
        let foo = uniqueElements |> Set.map (fun (x,y) -> toInt y) |> Set.toList
        foo
    
    [<Fact>]
    let getNumbersWithPositionsTestData () = 
        let input = readInit "testinput.txt" 
        let numbers = getNumbersWithPositions input
        let (_,firstNumber) = List.find (fun (x,y) -> x = 0) numbers
        Assert.Equal<byte[]>([|52uy;54uy;55uy|], firstNumber)
        let (_,secondNumber) = List.find (fun (x,y) -> x = 5) numbers
        Assert.Equal<byte[]>([|49uy;49uy;52uy|], secondNumber)

    [<Fact>]
    let getNumbersWithPositionsPostFilterTestData () = 
        let input = readInit "testinput.txt" 
        let numbers = getNumbersWithPositions (getFilteredNumbers input)
        let (_,firstNumber) = List.find (fun (x,y) -> x = 2) numbers
        Assert.Equal<byte[]>([|55uy|], firstNumber)
        let (_,secondNumber) = List.find (fun (x,y) -> x = 22) numbers
        Assert.Equal<byte[]>([|51uy;53uy|], secondNumber)
  
    [<Fact>]
    let insane() = 
        let input = readInit "testinput.txt"
        let foo = getNumbers input 
        Assert.True(true)

    [<Fact>]
    let checkBitMask() = 
        let input = readInit "testinput.txt"
        let filteredInput = getFilteredNumbers input
        let filteredAsTxt = matrixToText filteredInput
        Assert.True(true)


    [<Fact>]
    let checkText() = 
        let txt = matrixToText (readInit "testinput.txt") |> Array.toList
        let rawText = readLinesAsTxt "testinput.txt" |> Array.toList
        let foo = matrixToText (getSymbolMaskNeighbors(getSymbolMask (readInit "testinput.txt")))
        Assert.Equal<string list>(rawText, txt)

module Program = let [<EntryPoint>] main _ = 0