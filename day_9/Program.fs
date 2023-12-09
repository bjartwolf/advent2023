module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): int64 list list= 
        let parseLine (l:string): int64 list =  l.Split(" ") |> Array.map int64 |> Array.toList
        let file = File.ReadAllLines(filePath)
        file |> Array.map parseLine |> Array.toList

    // maybe not use lists if slow
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


    [<Fact>]
    let pyramidExpansionTest1() = 
        let testPyramid1 = makePyramidUntilZero testinput[0]
        let expansion = findPyramidExpansion testPyramid1 
        Assert.Equal<int64 list> ([18L;3L;0L], expansion)

    [<Fact>]
    let pyramidExpansionTest2() = 
        let testPyramid = makePyramidUntilZero testinput[1]
        let expansion = findPyramidExpansion testPyramid 
        Assert.Equal<int64 list> ([28L;7L;1L;0L], expansion)

    [<Fact>]
    let pyramidExpansionTest3() = 
        let testPyramid = makePyramidUntilZero testinput[2]
        let expansion = findPyramidExpansion testPyramid 
        Assert.Equal<int64 list> ([68L;23L;8L;2L;0L], expansion)

    let printPyramid (pyramid: int64 list list) (desc:string) =
        printfn "Pyramid **** %s" desc
        let mutable lineindent = ""
        for line in pyramid do
            printf "%s" lineindent
            for num in line do
                printf "%i " num 
            lineindent <- " " + lineindent
            printfn ""

    [<Fact>]
    let pyramidTest1() = 
        Assert.Equal<int64 list list> ([], makePyramidUntilZero [])
        let testPyramid1 = makePyramidUntilZero testinput[0]
        let expectedPyramid1 = [testinput[0]; [3L;3L;3L;3L;3L]; [0L;0L;0L;0L]]
        printPyramid testPyramid1 "test"
        printPyramid expectedPyramid1 "expected"
        Assert.Equal<int64 list list> (expectedPyramid1, testPyramid1) 

    [<Fact>]
    let pyramidTest2() = 
        let testPyramid = makePyramidUntilZero testinput[1]
        let expectedPyramid = [testinput[1]; [2L;3L;4L;5L;6L]; [1L; 1L; 1L; 1L]; [0L;0L;0L]] 
        printPyramid testPyramid "test 2"
        printPyramid expectedPyramid "expected 2"
        Assert.Equal<int64 list list> (expectedPyramid, testPyramid) 

    [<Fact>]
    let pyramidTest3() = 
        let testPyramid = makePyramidUntilZero testinput[2]
        let expectedPyramid = [testinput[2]; [3L;3L;5L;9L;15L]; [0L;2L;4L;6L]; [2L;2L;2L]; [0L;0L] ]
        printPyramid testPyramid "test 3"
        printPyramid expectedPyramid "expected 3"
        Assert.Equal<int64 list list> (expectedPyramid, testPyramid) 
   
    [<Fact>]
    let allZeroTest() = 
        Assert.True (allZeros [0L; 0L] )
        Assert.False (allZeros [0L; 1L; 0L] )
 
    [<Fact>]
    let testdiffer () = 
        Assert.Equal<int64 list>([3L], findDiff [3L; 6L]) 
        Assert.Equal<int64 list>([3L; 3L], findDiff [3L; 6L; 9L]) 
        Assert.Equal<int64 list>([2L;3L;4L;5L;6L;7L], findDiff [1L; 3L; 6L; 10L; 15L; 21L; 28L])
        Assert.Equal<int64 list>([], findDiff [0L]) // think it should be empty list
        Assert.Equal<int64 list>([0L], findDiff [0L; 0L]) 

    [<Fact>]
    let test2 () = 
        Assert.Equal(3, testinput.Length) 

module Program = let [<EntryPoint>] main _ = 0