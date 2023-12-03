open System.Collections.Generic

module Input =
    open Xunit 

    let digitCharCodes = [0..9] |> Seq.map (fun x -> byte x + 48uy) |> Seq.toArray
    let dotCharCode = byte '.'
    let gearCharCode = byte '*'

    let readInit (filePath: string) : byte [][] = 
        let lines = System.IO.File.ReadAllLines(filePath)
        lines |> Array.map (fun x -> x |> Seq.toArray |> Seq.map byte |> Seq.toArray)

    let keepSymbols (char:byte) : bool =
        char = gearCharCode

    let getSymbolMask (input: byte[][]): bool[][] =
        input |> Array.map (fun x -> x |> Array.map keepSymbols)

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

    let getNumbersWithPositions (input: byte[][]): ((int*int)*byte[]) list =
        let length= input.Length
        let foo = input |> Array.map (fun a -> a |> groupPredicateWithPosition (fun x -> Seq.contains x digitCharCodes))  |> Array.toList
        foo |> List.mapi (fun i x ->  x |> List.map (fun (j,y) -> ((i,j),y) )) |>List.collect (id) 

    let toInt (bytes : byte []) =
        bytes |> Array.map char |> System.String |> int

    type Position = int * int
    type Positions = Position list
    type Number = Positions * byte[]

    let getPositionsFromNumber (((x,y),bytes): Position * byte[]): Positions*byte[] = 
        let coordinates: Positions = bytes |> Array.mapi ( fun i _  -> (x,y+i)) |> Array.toList
        (coordinates, bytes)
    
    let getPositionsFromMask (mask: bool[][]): Positions =
        let size = mask.Length
        let positions = [ for i in 0 .. size - 1 do
                            for j in 0 .. size - 1 do
                                if mask[i][j] then yield (i,j)
        ]
        positions

    let overlap (p1s: Positions) (p2s: Positions) : bool =
        let s1 = p1s |> Set.ofList
        let s2 = p2s |> Set.ofList
        let intersection = Set.intersect s1 s2 
        intersection |> Set.isEmpty |> not
        
    let getStars mask = getPositionsFromMask mask 

    let getNeighbors (max_size: int) ((x,y): Position): Positions = 
        let xs = [-1 .. 1]
        let ys = [-1 .. 1]
        [ for deltaX in xs do
            for deltaY in ys do
                if x + deltaX >= 0 && x + deltaX <= max_size - 1 && y + deltaY >= 0 && y + deltaY <= max_size - 1 then
                    yield (x+deltaX,y+deltaY) ]


    let getGears (input: byte[][]): (int*int) list =
        let numbersWithPositions = getNumbersWithPositions input |> List.map getPositionsFromNumber 
        let size = input.Length
        let stars = getSymbolMask(input) |> getStars
        let gears = [ 
            for star in stars do
                let neighbors = getNeighbors size star
                let numbersInHood = numbersWithPositions |> List.where (fun (p,_) -> overlap neighbors p)
                if (numbersInHood.Length = 2) then
                    let _, num1 = numbersInHood[0]
                    let _,num2 = numbersInHood[1]
                    yield (toInt num1, toInt num2)
        ]
        gears
                  

    [<Fact>]
    let testinput() = 
        let input = readInit "testinput.txt"
        let gears = getGears input 
        let sum = gears |> List.map (fun (x,y) -> x*y) |> List.sum 
        Assert.Equal(467835, sum)

    [<Fact>]
    let problem() = 
        let input = readInit "input.txt"
        let gears = getGears input 
        let sum = gears |> List.map (fun (x,y) -> x*y) |> List.sum 
        Assert.Equal(79613331, sum)

module Program = let [<EntryPoint>] main _ = 0