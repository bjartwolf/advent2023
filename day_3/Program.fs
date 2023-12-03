module Input =
    open System
    open System.IO
    open Xunit 
    (* 10x10 testinput, 140x140 real input, quadratic 
    - add up parts numbers
    - any number adjacent to a symbol, even diagonally, is a part number
    - periods are not symbols
   
   - find all symbols and create a filter mask to map it with.
   - a symbol and its neighbourhood gets a . can operate on charcodes.
    *)

    let digitCharCodes = [0..9] |> Seq.map (fun x -> byte x + 48uy) |> Seq.toArray
    let dotCharCode = byte '.'

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

    (* Sets symobls *)
    let setSymbolMask (input: byte[][]): byte[][] =
        input |> Array.map (fun x -> x |> Array.map (fun code -> code))

    [<Fact>]
    let test2 () = 
        let input = readInit "input.txt" 
        Assert.Equal(140, input.Length) 

module Program = let [<EntryPoint>] main _ = 0