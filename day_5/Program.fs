open System.Collections.Generic


module Input =
    open System
    open System.IO
    open Xunit 
    type line = { dst: int; src: int; rng: int}
    let groupByWhiteSpace (input: string[]) : string list list =
        let mutable tmpGroup: string list = []
        let mutable readingGroup = false

        [ for i = 0 to Array.length input - 1 do
              let crntLine = input[i]
              if (not readingGroup && crntLine.Contains(":")) then
                tmpGroup <- [] 
                readingGroup <- true
              elif readingGroup && crntLine <> "" then
                tmpGroup <- tmpGroup @ [crntLine]
              else
                if crntLine = "" then
                    readingGroup <- false
                    yield tmpGroup
        ] 

    let readGroups (input: string[]): line list list =
        let groups = groupByWhiteSpace input 
        groups |> List.map (fun map -> map |> List.map (fun l -> 
            let nums =  l.Split(" ") |> Array.map (fun n -> int n)
            { dst=nums[0]; src=nums[1]; rng = nums[2]} ))

    let test_seeds = [79;14;55;13]
    let readInit (filePath: string): (int list* line list list) = 
        let input = System.IO.File.ReadAllLines filePath
        let seeds = input[0].Replace("seeds: ","").Split(" ") |> Array.map (fun x -> int x) |> Array.toList
        let maps = readGroups input[2..] 
        seeds, [[{src=1;dst=1;rng=1}]]

    let isInRange (seed: int) (map: line) : bool = 
        [map.src .. map.src + map.rng - 1] |> List.contains seed

    [<Fact>]
    let testRanges () =
        let testMap = {dst=50;src=98;rng=2}
        Assert.False(isInRange 97 testMap)
        Assert.True(isInRange 98 testMap)
        Assert.True(isInRange 99 testMap)
        Assert.False(isInRange 100 testMap)
        
    let mapSeedToNextMap (maps: line list) (seed:int): int =
        [ for map in maps do
                if isInRange seed map then
                    yield (map.dst + seed - map.src)
          yield seed
        ] |> List.head

    [<Fact>]
    let testMapToNext () =
        let testMap = {dst=50;src=98;rng=2}
        Assert.Equal(50, mapSeedToNextMap [testMap] 98)
        Assert.Equal(51, mapSeedToNextMap [testMap] 99)
             

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        let seeds,_= input
        Assert.Equal<int list>(test_seeds, seeds) 

module Program = let [<EntryPoint>] main _ = 0