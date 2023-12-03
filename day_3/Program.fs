open System.Collections.Generic

module Input =
    open Xunit 

    let digitCharCodes = [0..9] |> Seq.map (fun x -> byte x + 48uy) |> Seq.toArray
    let dotCharCode = byte '.'

    let readLinesAsTxt (filePath: string) : string[] = 
        System.IO.File.ReadAllLines(filePath)

    let readInit (filePath: string) : byte [][] = 
        let lines = System.IO.File.ReadAllLines(filePath)
        lines |> Array.map (fun x -> x |> Seq.toArray |> Seq.map byte |> Seq.toArray)

    let keepSymbols (char:byte) : bool =
        if Seq.contains char digitCharCodes then false 
        else if char = dotCharCode then false 
        else true 

    [<Fact>]
    let keepSymbolsDeleteNumbers() =
        Assert.Equal(false, keepSymbols digitCharCodes.[3])

    [<Fact>]
    let keepSymbolsKeepSymbol() =
        Assert.Equal(true, keepSymbols (byte '$'))

    [<Fact>]
    let keepSymbolsDeleteDots() =
        Assert.Equal(false, keepSymbols (byte '.'))

    (* get symbols in the mask, no neighbourhood *)
    let getSymbolMask (input: byte[][]): bool[][] =
        input |> Array.map (fun x -> x |> Array.map keepSymbols)


    let copy (input: bool[][]) =
        input |> Array.map (fun x -> Array.copy x)

    (* expand symbolmask to set all neighbours to 255uy *) 
    // quadratic, so cheating
    let getSymbolMaskNeighbors (input: bool[][]): bool[][] =
         let newMask = copy input
         let iterend = input.Length - 1
         for i in 0..iterend do
            for j in 0.. iterend do
                if input[i][j] = true then
                    let x = [-1 .. 1]
                    let y = [-1 .. 1]
                    for deltaX in x do
                        for deltaY in y do
                            if i + deltaX >= 0 && i + deltaX <= iterend && j + deltaY >= 0 && j + deltaY <= iterend then
                                newMask[i+deltaX][j+deltaY] <- true 
         newMask 

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
 
    let matrixToText (input: byte[][]): string[] = 
        input |> Array.map (fun x -> System.String (x |> Seq.map char |> Seq.toArray))

    [<Fact>]
    let test2 () = 
        let input = readInit "input.txt" 
        Assert.Equal(140, input.Length) 

    let getBitMask (input: byte[][]): bool[][] =
        let symbols = getSymbolMask input
        let mask = getSymbolMaskNeighbors symbols 
        mask

    let getNumbersWithPositions (input: byte[][]): ((int*int)*byte[]) list =
        let length= input.Length
        let foo = input |> Array.map (fun a -> a |> groupPredicateWithPosition (fun x -> Seq.contains x digitCharCodes))  |> Array.toList
        foo |> List.mapi (fun i x ->  x |> List.map (fun (j,y) -> ((i,j),y) )) |>List.collect (id) 

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
        let (_,firstNumber) = List.find (fun (x,y) -> x = (0,0)) numbers
        Assert.Equal<byte[]>([|49uy|], firstNumber)
        let (_,secondNumber) = List.find (fun (x,y) -> x = (1,1)) numbers
        Assert.Equal<byte[]>([|50uy|], secondNumber)

    let getNumbers (input: byte[][]): int list =
        let initialNumbers = getNumbersWithPositions input 
        let mask = getBitMask(input)

        let foo = initialNumbers |> List.where (fun ((x,y),bytes) -> 
            let coordinates = bytes |> Array.mapi ( fun i _  -> (x,y+i))
            coordinates |> Array.exists (fun (x',y') -> mask[x'][y'] = true)) 
            
        foo |> List.map (fun (x,y) -> toInt y)

    [<Fact>]
    let getNumbersWithPositionsTestData () = 
        let input = readInit "testinput.txt" 
        let numbers = getNumbersWithPositions input
        let (_,firstNumber) = List.find (fun (x,y) -> x = (0,0)) numbers
        Assert.Equal<byte[]>([|52uy;54uy;55uy|], firstNumber)
        let (_,secondNumber) = List.find (fun (x,y) -> x = (0,5)) numbers
        Assert.Equal<byte[]>([|49uy;49uy;52uy|], secondNumber)

    [<Fact>]
    let insane() = 
        let input = readInit "testinput.txt"
        let sum = getNumbers input |> List.sum
        Assert.Equal(4361, sum)

    [<Fact>]
    let insanereal() = 
        let input = readInit "input.txt"
        let sum = getNumbers input |> List.sum
        Assert.Equal(543867, sum)


module Program = let [<EntryPoint>] main _ = 0