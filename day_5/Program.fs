open System.Collections.Generic


module Input =
    open System
    open System.IO
    open Xunit 
    type line = { dst: int64; src: int64; rng: int64}
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
            let nums =  l.Split(" ") |> Array.map (fun n -> int64 n)
            { dst=nums[0]; src=nums[1]; rng = nums[2]} ))

    let test_seeds = [79L;14L;55L;13L]
    let readInit (filePath: string): (int64 list* line list list) = 
        let input = System.IO.File.ReadAllLines filePath
        let seedsArray = input[0].Replace("seeds: ","").Split(" ") |> Array.map (fun x -> int64 x) 
        let seeds = seedsArray|> Array.toList
        let maps = readGroups input[2..] 
        seeds, maps 

    let isInRange (seed: int64) (map: line) : bool = 
        seed >= map.src && seed <= map.src+map.rng - 1L 

    [<Fact>]
    let testRanges () =
        let testMap = {dst=50L;src=98L;rng=2L}
        Assert.False(isInRange 97 testMap)
        Assert.True(isInRange 98 testMap)
        Assert.True(isInRange 99 testMap)
        Assert.False(isInRange 100 testMap)
        
    let mapSeedToNextMap (maps: line list) (seed:int64): int64 =
        [ for map in maps do
                if isInRange seed map then
                    yield (map.dst + seed - map.src)
          yield seed
        ] |> List.head
            
    let mapSeedThroughMaps (maps: line list list) (seed:int64): int64 =
        let mutable mapped = seed 
        for map in maps do
            mapped <- mapSeedToNextMap map mapped 
        mapped 

    [<Fact>]
    let testMapToNext () =
        let testMap = {dst=50L;src=98L;rng=2L}
        Assert.Equal(50L, mapSeedToNextMap [testMap] 98L)
        Assert.Equal(51L, mapSeedToNextMap [testMap] 99L)
        Assert.Equal(10L, mapSeedToNextMap [testMap] 10L)

    [<Fact>]
    let testsoil () = 
        let input = readInit "testinput.txt" 
        let seeds,maps = input
        let soils = [81L;14L;57L;13L]
        let locations  = seeds |> List.map (fun s -> mapSeedThroughMaps [maps[0]] s)
        Assert.Equal<int64 list>(soils, locations)
             
    [<Fact>]
    let test2 () = 
        let input = readInit "testinput.txt" 
        let seeds,maps = input
        let locations  = seeds |> List.map (fun s -> mapSeedThroughMaps maps s)
        Assert.Equal<int64 list>(test_seeds, seeds) 
        Assert.Equal<int64 list>([82L;43L;86L;35L], locations) 

    [<Fact>]
    let test_testdata() = 
        let input = readInit "testinput.txt" 
        let seeds,maps = input
        let locations  = seeds |> List.map (fun s -> mapSeedThroughMaps maps s)
        Assert.Equal(35L, locations |> List.min)

    [<Fact>]
    let part1() = 
        let input = readInit "input.txt" 
        let seeds,maps = input
        let locations  = seeds |> List.map (fun s -> mapSeedThroughMaps maps s)
        Assert.Equal(214922730L, locations |> List.min)

module Program = let [<EntryPoint>] main _ = 0