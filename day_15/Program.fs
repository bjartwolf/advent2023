module Input =
    open System.IO
    open Xunit 

    let readInit (filePath: string) = 
        let line = File.ReadAllLines filePath
                        |> Array.head
        line.Split(",")
            |> Array.toList
        
    let wordToNumbers (word:string) =
        word.ToCharArray() |> Array.map int |> Array.toList

    let hashLabel (word: string) = 
        let rec calcWordNrInner (wordDigits: int list) (current: int): int =
            match wordDigits with
                | [] -> current
                | h::t -> calcWordNrInner t (((current + h) * 17) % 256)
        calcWordNrInner (wordToNumbers word) 0 

    let calcWordList (words: string list) : int list =
        words |> List.map hashLabel 

    type LabeledLens = string*int
    type Command = Remove of string | Add of LabeledLens 
    type LabeledLenses = LabeledLens list
    type Boxes = Map<int,LabeledLenses> 

    let parseCommand (cmd: string) =
        if cmd.Contains('-') then
            let label = cmd.Replace("-","")
            Remove label 
        else
            let split= cmd.Split("=")
            Add (split[0],int split[1])

    let removeFromBoxes boxes label =  
        let hash = hashLabel label
        let findLenses = Map.tryFind hash boxes 
        match findLenses with
              | Some lenses -> let updated  = lenses |> List.filter (fun (l,_) -> l <> label)
                               Map.add hash updated boxes
              | None -> boxes 

    let addToBoxes boxes (label,focalLength) = 
        let hash = hashLabel label
        let findLenses = Map.tryFind hash boxes 
        let lenseExists l = l |> List.exists (fun (l,_) -> l = label)
        let updateLenses l = l |> List.map (fun (l,f) -> if l = label then 
                                                            (label,focalLength) 
                                                         else 
                                                            (l,f)) 
        match findLenses with
            | None -> Map.add hash [(label,focalLength)] boxes
            | Some lenses -> if (lenseExists lenses) then
                                let updatedLenses = lenses |> updateLenses
                                Map.add hash updatedLenses boxes
                             else
                                Map.add hash (lenses @ [(label,focalLength)]) boxes

    let parseCommands fileName =  
        readInit fileName 
            |> List.map parseCommand

    let processCommands commands = 
        let rec processCommandsInner commands box = 
           match commands with
                | [] -> box
                | h ::t -> match h with
                                | Remove label -> processCommandsInner t (removeFromBoxes box label)
                                | Add (label,lense) -> processCommandsInner t (addToBoxes box (label, lense) )
        processCommandsInner commands Map.empty 

    let focusPowerForLens lenses = 
        lenses |> List.mapi (fun i (_,nr) -> nr*(i+1)) |> List.sum

    let findFocusPower (boxes: Boxes) : int =
        boxes |> Map.map (fun i lenses -> (i+1)* focusPowerForLens lenses) 
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
    let test_procsessTestData() = 
        let cmds = parseCommands "testinput.txt" 
        let processedCommands = processCommands cmds 
        Assert.Equal<Boxes>(Map.ofList [ 0, [("rn",1); ("cm",2)];1,[]; 3,[("ot",7);("ab",5);("pc",6)]], processedCommands) 

    [<Fact>]
    let calcSumForReal () = 
        let input = readInit "input.txt" 
        Assert.Equal(518107, input |> calcWordList |> List.sum) 

module Program = let [<EntryPoint>] main _ = 0