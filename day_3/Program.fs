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
                            if i + deltaX > 0 && i + deltaX < iterend && j + deltaY > 0 && j + deltaY < iterend then
                                newMask[i+deltaX][j+deltaY] <- 255uy
         newMask 

    (* this does not really work, any part of the number must touch the bitmask*)
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

    [<Fact>]
    let checkBitMask() = 
        let input = readInit "testinput.txt"
        let mask = getSymbolMaskNeighbors(getSymbolMask input) 
        let filteredInput = applyBitMask input mask
        let filteredAsTxt = matrixToText filteredInput
        Assert.True(true)


    [<Fact>]
    let checkText() = 
        let txt = matrixToText (readInit "testinput.txt") |> Array.toList
        let rawText = readLinesAsTxt "testinput.txt" |> Array.toList
        let foo = matrixToText (getSymbolMaskNeighbors(getSymbolMask (readInit "testinput.txt")))
        Assert.Equal<string list>(rawText, txt)

module Program = let [<EntryPoint>] main _ = 0