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


    let matrixToText (input: byte[][]): string[] = 
        input |> Array.map (fun x -> System.String (x |> Seq.map char |> Seq.toArray))

    [<Fact>]
    let test2 () = 
        let input = readInit "input.txt" 
        Assert.Equal(140, input.Length) 

    [<Fact>]
    let checkText() = 
        let txt = matrixToText (readInit "testinput.txt") |> Array.toList
        let rawText = readLinesAsTxt "testinput.txt" |> Array.toList
        let foo = matrixToText (getSymbolMaskNeighbors(getSymbolMask (readInit "testinput.txt")))
        Assert.Equal<string list>(rawText, txt)

module Program = let [<EntryPoint>] main _ = 0