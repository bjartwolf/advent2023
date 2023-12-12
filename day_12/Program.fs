module Input =
    open System
    open System.IO
    open Xunit 

    let q = '?'

    let readInit (filePath: string): string []= 
        IO.File.ReadAllLines(filePath)

    // strategi 1, lage alle komboer, filtrere ut 
    // bruke mengder
    // strategi 2, regulært uttrykk
    // compile noe catchgroups, blir fort vanskelig.

    // 2 i n antall spørsmålstegn, men det er ikke så mange ? i input

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

    [<Fact>]
    let yieldCombinationsTest() = 
        let test = "??"
        let fasit = ["..";".#";"#.";"##"] |> Set.ofList
        let combos = findAll test
        Assert.Equal<Set<string>>(fasit, combos)

    [<Fact>]
    let test2 () = 
        let input = readInit "input.txt" 
        Assert.Equal(1000, input.Length) 

module Program = let [<EntryPoint>] main _ = 0