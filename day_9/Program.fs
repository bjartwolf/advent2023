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
       lst |> makePyramidUntilZeroSeq |> Seq.toList

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
    let pyramidTest() = 
        Assert.Equal<int64 list list> ([], makePyramidUntilZero [])
//        Assert.Equal<int64 list list> ([[1L];[0L]], makePyramidUntilZero [1L]) // not sure about this one
        let testPyramid1 = makePyramidUntilZero testinput[0]
        let expectedPyramid1 = [testinput[0]; [3L;3L;3L;3L;3L]; [0L;0L;0L;0L]]
        printPyramid testPyramid1 "test"
        printPyramid expectedPyramid1 "expected"
        Assert.Equal<int64 list list> (expectedPyramid1, testPyramid1) 
 
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