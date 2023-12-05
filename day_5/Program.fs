open System.Collections.Generic
open System


module Input =
    open System
    open System.IO
    open Xunit 
    type line = { dst: int64; src: int64; rng: int64}
    type range = int64*int64 
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

    let rec pair list =
        match list with
        | x :: y :: rest -> (x, y) :: pair rest
        | _ -> []

    let partsInRange ((start,length): range) (map: line) : range list= 
       [
           let nrElementsSmallerThanMap = Math.Max(Math.Min(map.src-start, length),0L)
           if (nrElementsSmallerThanMap > 0L) then yield (start,nrElementsSmallerThanMap)
       
           let nrElementsInMap = Math.Max(Math.Min(map.rng-nrElementsSmallerThanMap,map.rng),0L)
           if nrElementsInMap > 0L then 
                yield (map.dst,nrElementsInMap)

           let nrElementsOutsideMap = Math.Max(Math.Min(length-nrElementsSmallerThanMap - nrElementsInMap,0L), length)
           if (nrElementsOutsideMap > 0) then
                yield (start + nrElementsSmallerThanMap + nrElementsInMap , length-nrElementsOutsideMap-nrElementsInMap)
       ]

    [<Fact>]
    let testmappingparts7 () = 
        Assert.Equal<range>( (100L,10L), (partsInRange (100L,10L)  {dst=1L;src=500L;rng=4L})|> List.head)
  //      Assert.Equal<range>( (0L,10L), (partsInRange (0L,10L)  {dst=1L;src=20L;rng=10L})|> List.head)
        Assert.Equal<range>( (100L,10L), (partsInRange (100L,10L)  {dst=1L;src=0L;rng=4L})|> List.head)
        Assert.Equal<range>( (1000L,100L), (partsInRange (1000L,100L)  {dst=1L;src=5L;rng=4L})|> List.head)

    [<Fact>]
    let testmappingparts4 () = 
        Assert.Equal<range>( (1L,1L), (partsInRange (0L,1L)  {dst=1L;src=0L;rng=1L})|> List.head)
        Assert.Equal<range>( (0L,1L), (partsInRange (0L,1L)  {dst=0L;src=0L;rng=1L})|> List.head)

    [<Fact>]
    let testmappingpartsX () = 
        let range = { dst=100L;src=3L;rng=10L}
        let partsInRange = partsInRange (1L,5L) range
        Assert.Equal(2, partsInRange.Length)
        Assert.Equal<range>( (1L,2L), partsInRange[0])
        Assert.Equal<range>( (100L,3L), partsInRange[1])

    [<Fact>]
    let testmappingparts2 () = 
        //let input = readInit "testinput.txt" 
        //let seeds,maps = input
        let range = { dst=100L;src=3L;rng=4L}
        let partsInRange = partsInRange (1L,10L) range
        Assert.Equal<range>( (1L,2L), partsInRange[0])
        Assert.Equal<range>( (100L,4L), partsInRange[1])
        Assert.Equal<range>( (7L,4L), partsInRange[2])

    [<Fact>]
    let testmappingparts41 () = 
        Assert.Equal<range>( (0L,1L), (partsInRange (0L,1L)  {dst=0L;src=0L;rng=1L})|> List.head)
        Assert.Equal<range>( (1L,1L), (partsInRange (0L,1L)  {dst=1L;src=0L;rng=1L})|> List.head)
        Assert.Equal<range>( (10L,1L), (partsInRange (0L,1L) {dst=10L;src=0L;rng=1L})|> List.head)

    let rangesFromInputs (input:int64 list): range list =
        let pairs = pair input
        pairs |> List.map (fun (x,y) -> (x, y ))
        

    [<Fact>]
    let ranges_stuff() = 
        let input = readInit "testinput.txt" 
        let seeds,maps= input
        let pairs = pair seeds 
        let ranges = pairs |> List.map (fun (first,number) -> [first .. first + number - 1L]) |> List.collect (id) 
        let locations  = ranges |> List.map (fun s -> mapSeedThroughMaps maps s)
        Assert.Equal(27L, locations.Length)
        Assert.Equal(46L, locations |> List.min)
 

    [<Fact>]
    let part1() = 
        let input = readInit "input.txt" 
        let seeds,maps = input
        ()
//        let locations  = seeds |> List.map (fun s -> mapSeedThroughMaps maps s)
//        Assert.Equal(214922730L, locations |> List.min)

module Program = 
    open Input
    let [<EntryPoint>] main _ = 
        let input = readInit "input.txt" 
        let seeds,maps = input
        let locations  = seeds |> List.map (fun s -> mapSeedThroughMaps maps s)
        Console.WriteLine(locations)
        Console.ReadLine() |> ignore
        0