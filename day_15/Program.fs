module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string) = 
        let line = File.ReadAllLines filePath
                        |> Array.head
        line.Split(",")
            |> Array.toList
        
    let testInput = readInit "testinput.txt"

    let wordToNumbers (word:string): int list =
        word.ToCharArray() |> Array.map int |> Array.toList

    [<Fact>]
    let makeWordTest () = 
        Assert.Equal<int list>([72;65;83;72],wordToNumbers "HASH")

    let hashLabel (word: string) : int =
        let rec calcWordNrInner (wordDigits: int list) (current: int): int =
            match wordDigits with
                | [] -> current
                | h::t -> calcWordNrInner t (((current + h) * 17) % 256)
        calcWordNrInner (wordToNumbers word) 0 

    [<Fact>]
    let calcWordTest () = 
        Assert.Equal(52, hashLabel "HASH")
        Assert.Equal(0, hashLabel "rn")
        Assert.Equal(0, hashLabel "cm")
        Assert.Equal(1, hashLabel "qp")
        Assert.Equal(3, hashLabel "pc")
        Assert.Equal(3, hashLabel "ot")
            
    let calcWordList (words: string list) : int list =
        words |> List.map hashLabel 

    [<Fact>]
    let calcWordListTest () = 
        let input = readInit "testinput.txt" 
        Assert.Equal<int list>([30;253;97;47;14;180;9;197;48;214;231], calcWordList input)

    [<Fact>]
    let calcSumForTest () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(1320, input |> calcWordList |> List.sum) 

        // focal lenth fra 1 - 9
    let focalLengths = [1 .. 9]
    type LabeledLens = string*int
    type Command = Remove of string | Add of LabeledLens 
    type LabeledLenses = LabeledLens list
    type Boxes = Map<int,LabeledLenses> 
    let boxes: Boxes = Map.empty

    let removeFromBoxes (boxes: Boxes) ((label,focalLength): LabeledLens): Boxes =
        let hash = hashLabel label
        let findLenses = Map.tryFind hash boxes 
        match findLenses with
              | Some lenses -> let updated  = lenses |> List.filter (fun (l,focalLength) -> l <> label)
                               Map.add hash updated boxes
              | None -> boxes 

    let addToBoxes (boxes: Boxes) ((label,focalLength): LabeledLens): Boxes =
        let hash = hashLabel label
        let existsSameLabel = Map.exists (fun x y -> x = hash && y |> List.contains (label,focalLength)) boxes
        if existsSameLabel then 
            boxes 
        else
            boxes 

    (* 
        REMOVE LABEL | ADD LABEL FOCALLENGTH
        KEY = HASH(LABEL)
        VALUE = LABEL,FOCALLENGTH
        
    *)

    [<Fact>]
    let calcSumForReal () = 
        let input = readInit "input.txt" 
        Assert.Equal(518107, input |> calcWordList |> List.sum) 

    [<Fact>]
    let test2 () = 
        let input = readInit "input.txt" 
        Assert.Equal(4000, input.Length) 

module Program = let [<EntryPoint>] main _ = 0