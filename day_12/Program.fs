open System.Collections.Generic


module Input =
    open System
    open System.IO
    open Xunit 

    let q = '?'

    let readInit (filePath: string): string []= 
        IO.File.ReadAllLines(filePath)

    let rec findAllCombos (x:string) : string seq =
        seq {
            let index = x.IndexOf(q)
            if index <> -1 then
                let prefix = x.Substring(0, index)
                let suffix = x.Substring(index + 1)
                let replacedStringWithDot = prefix + "." + suffix
                let replacedStringWithHash = prefix + "#" + suffix
                yield! findAllCombos replacedStringWithDot
                yield! findAllCombos replacedStringWithHash
            else yield x
        }

    let findAll x =
        findAllCombos x |> Set.ofSeq

    open System.Collections.Generic
    let countHashGroupsInner (input: Char []) : int seq =
        let groups: Dictionary<int, byte[]> =  Dictionary<int,byte[]>()
        let mutable currentFoundHashes = 0 
        let mutable foundHash = false
        seq {
         for i = 0 to Array.length input - 1 do
            if input.[i] = '#' && foundHash then
                currentFoundHashes <- currentFoundHashes + 1
            else if input.[i] = '#' && not foundHash then
                foundHash <- true
                currentFoundHashes <- 1 
            else if input.[i] = '.' && foundHash then
                yield currentFoundHashes 
                foundHash <- false
                currentFoundHashes <- 0
                // ignore if just . 
        }

    let countHashGroups (input: string): int list =
        let inputMod = "." + input + "."
        countHashGroupsInner (inputMod.ToCharArray()) |> Seq.toList

    [<Fact>]
    let countHashGroupsTest () = 
        let test1 = ".#.##.###."
        Assert.Equal<int list>([1;2;3], countHashGroups test1)
 
    let countLegal (inputDamaged: string) (counts: int list) : int =
        let combos = findAll inputDamaged
        let legalCombos = combos |> Seq.filter (fun c -> countHashGroups c = counts) 
        legalCombos |> Seq.length 

    [<Fact>]
    let countLegalTest() = 
        Assert.Equal(1, countLegal "???.###" [1;1;3])
        Assert.Equal(4, countLegal ".??..??...?##." [1;1;3])
        Assert.Equal(4, countLegal "????.######..#####." [1;6;5])
        Assert.Equal(10, countLegal "?###????????" [3;2;1])

    // ingen spørsmålstegn lenger, har bare prikker og grupper, kan
    // kanskje gruppere antall på rad
    //let rec isMatching (x: string) (groups: int list) : bool =
    //    if x = "" && groups = [] then true
    //    else false

    [<Fact>]
    let yieldCombinationsTest() = 
        let test = "??"
        let fasit = ["..";".#";"#.";"##"] |> Set.ofList
        let combos = findAll test
        Assert.Equal<Set<string>>(fasit, combos)

    [<Fact>]
    let yieldCombinationsTest2() = 
        let test = "??#"
        let fasit = ["..#";".##";"#.#";"###"] |> Set.ofList
        let combos = findAll test
        Assert.Equal<Set<string>>(fasit, combos)

    [<Fact>]
    let yieldCombinationsTest3() = 
        let test = ".??#"
        let fasit = ["...#";"..##";".#.#";".###"] |> Set.ofList
        let combos = findAll test
        Assert.Equal<Set<string>>(fasit, combos)


    [<Fact>]
    let test2 () = 
        let input = readInit "input.txt" 
        Assert.Equal(1000, input.Length) 

module Program = let [<EntryPoint>] main _ = 0