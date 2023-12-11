module Input =
    open System
    open System.IO
    open Xunit 

    let findZeroRows (filePath: string): int list = 
        let lines = IO.File.ReadAllLines(filePath)
        [
            let height = lines.Length
            for i in 0 .. height - 1 do
                if lines[i] |> String.forall (fun c -> c = '.') then
                    yield i
        ]

    let findZeroColumns(filePath: string): int list = 
        let lines = IO.File.ReadAllLines(filePath)
        [
            let width = lines[0].Length
            for i in 0 .. width - 1 do
                if (lines |> Array.forall (fun l -> l[i] = '.')) then
                    yield i
        ]

    let readInit (filePath: string): (int*int) list = 
        let lines = IO.File.ReadAllLines(filePath)
        [
            let height = lines.Length
            let width = lines[0].Length
            for i in 0 .. height - 1 do
                for j in 0 .. width - 1 do
                    if lines[i][j] = '#' then
                        yield (i,j)
        ]

    let distance ((a,b): (int*int)*(int*int) ) (zeroCols: int list) (zeroRows:int list) =
        let ay,ax = a
        let by,bx = b
        let minY, maxY = min ay by, max ay by 
        let minX, maxX = min ax bx, max ax bx 
        let spacesInX = zeroCols |> List.where (fun c -> c > minX && c < maxX) |> List.length
        let spacesInY = zeroRows|> List.where (fun c -> c > minY && c < maxY) |> List.length
        (maxY-minY) + (maxX - minX) + spacesInX + spacesInY


        // distX

     //  the sum of the shortest path
     //  path is absolute value of taxicab distance all the pairs with added value, can count itself because it is zero anyway 
    let rec findPairs (galaxies: (int*int) list): ((int*int)*(int*int)) list =
        match galaxies with 
            | g1::g2 -> List.allPairs [g1] g2 @ findPairs g2
            | [] -> []

    let distanceAllPairs(inputPath:string) =
        let zeroRows = findZeroRows inputPath
        let zeroCols = findZeroColumns inputPath
        let galaxies = readInit inputPath
        let allPairs = findPairs galaxies 
        let allDistances = allPairs |> List.map (fun pair -> distance pair zeroCols zeroRows)
        allDistances 

    let sumDistanceAllPairs (inputPath:string) =
        let distances = distanceAllPairs inputPath
        distances |> List.sum 

    [<Fact>]
    let countDistances () = 
        let input = distanceAllPairs "testinput.txt" 
        Assert.Equal(36, input.Length) 


    [<Fact>]
    let testSum() = 
        let input = sumDistanceAllPairs "testinput.txt" 
        Assert.Equal(374, input)

    [<Fact>]
    let testDistances() = 
        let input = sumDistanceAllPairs "input.txt" 
        Assert.Equal(10490062, input)


    [<Fact>]
    let testFindRows() = 
        let input = findZeroRows "testinput.txt" 
        Assert.Equal<int list>([3;7], input)

    [<Fact>]
    let testFindColumns () = 
        let input = findZeroColumns "testinput.txt" 
        Assert.Equal<int list>([2;5;8], input)


    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(9, input.Length) 
        Assert.True(input |> List.contains (9,0))
        Assert.True(input |> List.contains (9,4))

module Program = let [<EntryPoint>] main _ = 0