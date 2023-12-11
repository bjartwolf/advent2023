module Input =
    open System
    open System.IO
    open Xunit 

    let findZeroRows (filePath: string): int64 list = 
        let lines = IO.File.ReadAllLines(filePath)
        [
            let height = lines.Length
            for i in 0 .. height - 1 do
                if lines[i] |> String.forall (fun c -> c = '.') then
                    yield i
        ]

    let findZeroColumns(filePath: string): int64 list = 
        let lines = IO.File.ReadAllLines(filePath)
        [
            let width = lines[0].Length
            for i in 0 .. width - 1 do
                if (lines |> Array.forall (fun l -> l[i] = '.')) then
                    yield i
        ]

    let readInit (filePath: string): (int64*int64) list = 
        let lines = IO.File.ReadAllLines(filePath)
        [
            let height = lines.Length
            let width = lines[0].Length
            for i in 0 .. height - 1 do
                for j in 0 .. width - 1 do
                    if lines[i][j] = '#' then
                        yield (i,j)
        ]

    let distance ((a,b): (int64*int64)*(int64*int64) ) (zeroCols: int64 list) (zeroRows:int64 list) (factor: int64)=
        let ay,ax = a
        let by,bx = b
        let minY, maxY = min ay by, max ay by 
        let minX, maxX = min ax bx, max ax bx 
        let spacesInX:int64 = zeroCols |> List.where (fun c -> c > minX && c < maxX) |> List.length |> int64
        let spacesInY = zeroRows|> List.where (fun c -> c > minY && c < maxY) |> List.length |> int64
        (maxY-minY) + (maxX - minX) +  (spacesInX+spacesInY)*(factor - 1L)
        // distX

     //  the sum of the shortest path
     //  path is absolute value of taxicab distance all the pairs with added value, can count itself because it is zero anyway 
    let rec findPairs (galaxies: (int64*int64) list): ((int64*int64)*(int64*int64)) list =
        match galaxies with 
            | g1::g2 -> List.allPairs [g1] g2 @ findPairs g2
            | [] -> []

    let distanceAllPairs(inputPath:string) (factor:int64)=
        let zeroRows = findZeroRows inputPath
        let zeroCols = findZeroColumns inputPath
        let galaxies = readInit inputPath
        let allPairs = findPairs galaxies 
        let allDistances = allPairs |> List.map (fun pair -> distance pair zeroCols zeroRows factor)
        allDistances 

    let sumDistanceAllPairs (inputPath:string) (factor: int64)=
        let distances = distanceAllPairs inputPath factor
        distances |> List.sum 

    [<Fact>]
    let countDistances () = 
        let input = distanceAllPairs "testinput.txt" 2L 
        Assert.Equal(36, input.Length) 

    [<Fact>]
    let testSum() = 
        let input = sumDistanceAllPairs "testinput.txt" 2L
        Assert.Equal(374L, input)

    [<Fact>]
    let testSumFactor10() = 
        let input = sumDistanceAllPairs "testinput.txt" 10L
        Assert.Equal(1030L, input)
    // begge mangler 82...

    [<Fact>]
    let testSumFactor100() = 
        let input = sumDistanceAllPairs "testinput.txt" 100L
        Assert.Equal(8410L, input)

    [<Fact>]
    let testDistancesFactorOne() = 
        let input = sumDistanceAllPairs "input.txt" 2L 
        Assert.Equal(10490062L, input)

    [<Fact>]
    let testDistancesFactormillion() = 
        let input = sumDistanceAllPairs "input.txt" 1000000L
        Assert.Equal(3829724122L, input)

    [<Fact>]
    let testFindRows() = 
        let input = findZeroRows "testinput.txt" 
        Assert.Equal<int64 list>([3L;7L], input)

    [<Fact>]
    let testFindColumns () = 
        let input = findZeroColumns "testinput.txt" 
        Assert.Equal<int64 list>([2L;5L;8L], input)

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        Assert.Equal(9, input.Length) 
        Assert.True(input |> List.contains (9L,0L))
        Assert.True(input |> List.contains (9L,4L))

module Program = let [<EntryPoint>] main _ = 0