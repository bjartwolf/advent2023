module Input =
    open System.IO
    open Xunit 

    let readInit (filePath: string): int64 list list= 
        let parseLine (l:string): int64 list =  l.Split(" ") |> Array.map int64 |> Array.toList
        let file = File.ReadAllLines(filePath)
        file |> Array.map parseLine |> Array.toList

    let testinput = readInit "testinput.txt" 
    let input = readInit "input.txt" 

    let rec findDiffSeq (lst: int64 list): int64 seq =
        seq {
            match lst with 
                | a :: b :: tail -> yield b - a
                                    yield! findDiffSeq (b :: tail)
                | _ -> () 
        } 

    let findDiff lst = 
        let diffList = findDiffSeq lst |> Seq.toList
        if diffList.Length <> lst.Length - 1 then failwith "Lists should be one shorter"
        diffList

    let allZeros lst = lst |> List.forall (fun x -> x = 0L)

    let rec makePyramidUntilZeroSeq lst : int64 list seq =
        seq {
            if not (allZeros lst) then 
                let diffed = findDiff lst
                yield diffed
                yield! makePyramidUntilZeroSeq diffed 
        }

    let makePyramidUntilZero lst : int64 list list =
       let pyramidBottom = lst |> makePyramidUntilZeroSeq |> Seq.toList
       if pyramidBottom = [] then []
       else lst :: pyramidBottom

    let findPyramidExpansionSeq (pyramid: int64 list list): int64 seq =
        let mutable lastElement = 0L
        [
            for line in pyramid do
                let linesLastElement = List.last line
                yield lastElement + linesLastElement 
                lastElement <- linesLastElement  + lastElement
        ]

    let rec findPyramidExpansion (pyramid: int64 list list): int64 list =
        findPyramidExpansionSeq (pyramid |> List.rev) |> Seq.toList |> List.rev

    let findPyramidExpansionBackwardsSeq (pyramid: int64 list list): int64 seq =
        let mutable lastElement = 0L
        [
            for line in pyramid do
                let linesFirstElement = List.head line 
                let el = linesFirstElement - lastElement
                yield el
                lastElement <- el 
        ]

    let rec findPyramidExpansionBackwards (pyramid: int64 list list): int64 list =
        findPyramidExpansionBackwardsSeq (pyramid |> List.rev) |> Seq.toList |> List.rev

    [<Fact>]
    let pyramidExpansionBackwardsTest3() = 
        let testPyramid3 = makePyramidUntilZero testinput[2]
        let expansion = findPyramidExpansionBackwards testPyramid3 
        Assert.Equal<int64 list> ([5L;5L;-2L;2L;0L], expansion)

    let sumOfPyramidExpansionBack lst : int64 =
        lst |> List.map makePyramidUntilZero 
            |> List.map findPyramidExpansionBackwards
            |> List.map List.head
            |> List.sum

    [<Fact>]
    let testPyramidSumBack() = 
        Assert.Equal(1066L, sumOfPyramidExpansionBack input)
          
    let sumOfPyramidExpansion lst : int64 =
        lst |> List.map makePyramidUntilZero 
            |> List.map findPyramidExpansion
            |> List.map List.head
            |> List.sum

    [<Fact>]
    let testPyramidSum() = 
        Assert.Equal(114L, sumOfPyramidExpansion testinput)

    [<Fact>]
    let part1test() = 
        Assert.Equal(1762065988L, sumOfPyramidExpansion input)

module Program = let [<EntryPoint>] main _ = 0