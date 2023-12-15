open System.Collections.Specialized
open System.Collections


module Input =
    open System.IO
    open Xunit 

    let readInit (filePath: string) = 
        let line = File.ReadAllLines filePath
                        |> Array.head
        line.Split(",")
            |> Array.toList
        
    let wordToNumbers (word:string): int list =
        word.ToCharArray() |> Array.map int |> Array.toList

    let hashLabel (word: string) : int =
        let rec calcWordNrInner (wordDigits: int list) (current: int): int =
            match wordDigits with
                | [] -> current
                | h::t -> calcWordNrInner t (((current + h) * 17) % 256)
        calcWordNrInner (wordToNumbers word) 0 

    let calcWordList (words: string list) : int list =
        words |> List.map hashLabel 

    type LabeledLens = string*int
    type Command = Remove of string | Add of LabeledLens 
    type LabeledLenses = OrderedDictionary
    type Boxes = Map<int,LabeledLenses> 

    let parseCommand (cmd: string): Command =
        if cmd.Contains('-') then
            let label = cmd.Replace("-","")
            Remove label 
        else
            let split= cmd.Split("=")
            Add (split[0],int split[1])

    let removeFromBoxes (boxes: Boxes) (label: string): Boxes =
        let hash = hashLabel label
        let findLenses = Map.tryFind hash boxes 
        match findLenses with
              | Some lenses -> lenses.Remove(label) 
                               boxes 
              | None -> boxes 

    let addToBoxes (boxes: Boxes) ((label,focalLength): LabeledLens): Boxes =
        let hash = hashLabel label
        let findLenses = Map.tryFind hash boxes 
        match findLenses with
            | None ->   let orderedDict = OrderedDictionary() 
                        orderedDict.Add(label, (label, focalLength))
                        Map.add hash orderedDict boxes
            | Some lenses -> if lenses.Contains(label) then
                                let mutable count = 0
                                let mutable i = 0
                                for de in (lenses |> Seq.cast<DictionaryEntry>) do
                                    if (de.Key = label) then
                                        i <- count 
                                    count  <- count + 1  
                                lenses.RemoveAt(i)
                                lenses.Insert(i, label, (label,focalLength))
                                //lenses[i] <- (label, focalLength)
                             else 
                                lenses.Add(label, (label,focalLength)) 
                             boxes

    let parseCommands (fileName: string): Command list = 
        readInit fileName 
            |> List.map parseCommand

    let processCommands (commands: Command list): Boxes = 
        let rec processCommandsInner (commands: Command list) (box: Boxes) =
           match commands with
                | [] -> box
                | h ::t -> match h with
                                | Remove label -> processCommandsInner t (removeFromBoxes box label)
                                | Add (label,lense) -> processCommandsInner t (addToBoxes box (label, lense) )
        processCommandsInner commands Map.empty 

    let focusPowerForLens (lenses: LabeledLenses): int =
        let mutable i = 0;
        [ for de in (lenses |> Seq.cast<DictionaryEntry>) do
            i <- i + 1 
            yield (i,de.Value :?> LabeledLens)
        ]
            |> List.map (fun (i, (_,num)) -> num*(i) )
            |> List.sum

    let findFocusPower (boxes: Boxes) : int =
        boxes |> Map.map (fun i lenses -> (i + 1)* focusPowerForLens lenses) 
              |> Map.values
              |> Seq.sum
    [<Fact>]
    let test_procsessTestDataSum() = 
        let cmds = parseCommands "testinput.txt" 
        let processedCommands = processCommands cmds 
        let sum = findFocusPower processedCommands
        Assert.Equal(145, sum)

    [<Fact>]
    let test_procsessDataSum() = 
        let cmds = parseCommands "input.txt" 
        let processedCommands = processCommands cmds 
        let sum = findFocusPower processedCommands
        Assert.Equal(303404, sum)

    [<Fact>]
    let calcSumForReal () = 
        let input = readInit "input.txt" 
        Assert.Equal(518107, input |> calcWordList |> List.sum) 

module Program = let [<EntryPoint>] main _ = 0