module Input =
    open System
    open Xunit 

    let findZeroRows filePath :int64 list = 
        let lines = IO.File.ReadAllLines(filePath)
        [
            let height = lines.Length
            for i in 0 .. height - 1 do
                if lines[i] |> String.forall (fun c -> c = '.') then
                    yield i
        ]

    let findZeroColumns filePath : int64 list = 
        let lines = IO.File.ReadAllLines(filePath)
        [
            let width = lines[0].Length
            for i in 0 .. width - 1 do
                if (lines |> Array.forall (fun l -> l[i] = '.')) then
                    yield i
        ]

    let readInit filePath : (int64*int64) list= 
        let lines = IO.File.ReadAllLines(filePath)
        [
            let height = lines.Length
            let width = lines[0].Length
            for i in 0 .. height - 1 do
                for j in 0 .. width - 1 do
                    if lines[i][j] = '#' then
                        yield (i,j)
        ]

    let distance ((ay,ax),(by,bx)) zeroCols zeroRows factor =
        let minY, maxY = min ay by, max ay by 
        let minX, maxX = min ax bx, max ax bx 
        let spacesInX:int64 = zeroCols |> List.where (fun c -> c > minX && c < maxX) |> List.length |> int64
        let spacesInY = zeroRows|> List.where (fun c -> c > minY && c < maxY) |> List.length |> int64
        (maxY-minY) + (maxX - minX) +  (spacesInX+spacesInY)*(factor)

    let rec findPairs galaxies = 
        match galaxies with 
            | g1::g2 -> List.allPairs [g1] g2 @ findPairs g2
            | [] -> []

    let sumDistanceAllPairs inputPath factor = 
        let zeroRows = findZeroRows inputPath
        let zeroCols = findZeroColumns inputPath
        let galaxies = readInit inputPath
        galaxies |> findPairs
                 |> List.map (fun pair -> distance pair zeroCols zeroRows factor)
                 |> List.sum 

    [<Fact>]
    let testSum() = 
        let input = sumDistanceAllPairs "testinput.txt" 1L
        Assert.Equal(374L, input)

    [<Fact>]
    let testSumFactor10() = 
        let input = sumDistanceAllPairs "testinput.txt" (10L-1L)
        Assert.Equal(1030L, input)

    [<Fact>]
    let testSumFactor100() = 
        let input = sumDistanceAllPairs "testinput.txt" (100L-1L)
        Assert.Equal(8410L, input)

    [<Fact>]
    let testDistancesFactorOne() = 
        let input = sumDistanceAllPairs "input.txt" 1L 
        Assert.Equal(10490062L, input)

    [<Fact>]
    let testDistancesFactormillion() = 
        let input = sumDistanceAllPairs "input.txt" (1000000L-1L)
        Assert.Equal(382979724122L, input)

module Program = let [<EntryPoint>] main _ = 0