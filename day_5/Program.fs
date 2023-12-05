open System


module Input =
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

    let rec pair list =
        match list with
        | x :: y :: rest -> (x, y) :: pair rest
        | _ -> []

    let partsInRange ((start,length): range) (map: line) : range list= 
       [
           let nrElementsSmallerThanMap = Math.Max(Math.Min(map.src-start,length),0L)
           if (nrElementsSmallerThanMap > 0L) then yield (start,nrElementsSmallerThanMap)
       
           let (start',length') = (start+nrElementsSmallerThanMap, length-nrElementsSmallerThanMap)
           if (length' < 0) then failwith "ooop"

           let nrElementsInMap = if (start' > map.src + map.rng) then 0L
                                 else Math.Min(length', map.rng)
           if nrElementsInMap > 0L then 
                yield (start'-map.src+map.dst,nrElementsInMap)

           let (start'',length'') = (start+nrElementsSmallerThanMap + nrElementsInMap, length'-nrElementsInMap)
           if (length'' < 0) then failwith "ooops"

           let nrElementsOutsideMap = Math.Min(length'', map.src+map.rng-nrElementsInMap)
           if (nrElementsOutsideMap > 0) then
                yield (start'', length'')
       ]

    [<Fact>]
    let testmappingpartsTouchingRight() = 
        let range = { dst=100L;src=10L;rng=10L}
        let partsInRange = partsInRange (15L,15L) range
        Assert.Equal(2, partsInRange.Length)
        Assert.Equal<range>( (105L,10L), partsInRange[0])
        Assert.Equal<range>( (25L,5L), partsInRange[1])

    let partsInRanges (x: range) (maps: line list): range list =
        maps |> List.collect (fun m -> partsInRange x m)

    let mapRangeThroughMap (maps: line list list) (seed:range): range list=
        let mutable mapped:range list = [seed]
        for map in maps do
            let nextSeed = mapped |> List.collect (fun x -> partsInRanges x map)  
            mapped <- nextSeed 
        mapped 

    [<Fact>]
    let ranges_stuff_soil() = 
        let input = readInit "testinput.txt" 
        let seeds,maps= input
        let pairs = [(82L,1L)]
        let (x,l)= pairs|> List.map (fun s -> mapRangeThroughMap maps s) |> List.collect (id) |> List.minBy (fun (x,y) -> x) 
        Assert.Equal(46L,x) 
 
    [<Fact>]
    let testmappingpartsBug () = 
        let testMap = {dst=50L;src=98L;rng=2L}
        Assert.Equal<range>( (50L,1L), (partsInRange (98L,1L) testMap )[0])
        Assert.Equal<range>( (51L,1L), (partsInRange (99L,1L) testMap )[0])
        Assert.Equal<range>((10L,1L),  (partsInRange (10L,1L) testMap )[0])
 
    [<Fact>]
    let testmappingpartsXA () = 
        Assert.Equal<range>( (1L,4L), (partsInRange (5L,5L)  {dst=1L;src=5L;rng=4L})[0])
        Assert.Equal<range>( (9L,1L), (partsInRange (5L,5L)  {dst=1L;src=5L;rng=4L})[1])
 
    [<Fact>]
    let testmappingpartsTouchingLeft() = 
        let range = { dst=100L;src=10L;rng=10L}
        let partsInRange = partsInRange (5L,15L) range
        Assert.Equal(2, partsInRange.Length)
        Assert.Equal<range>( (5L,5L), partsInRange[0])
        Assert.Equal<range>( (100L,10L), partsInRange[1])

    [<Fact>]
    let testmappingpartsLarger() = 
        let range = { dst=100L;src=10L;rng=10L}
        let partsInRange = partsInRange (50L,10L) range
        Assert.Equal(1, partsInRange.Length)
        Assert.Equal<range>( (50L,10L), partsInRange[0])
 
    [<Fact>]
    let testmappingpartsInTheMiddle() = 
        let range = { dst=100L;src=10L;rng=10L}
        let partsInRange = partsInRange (10L,10L) range
        Assert.Equal(1, partsInRange.Length)
        Assert.Equal<range>( (100L,10L), partsInRange[0])

    [<Fact>]
    let testmappingpartsBefore () = 
        let range = { dst=100L;src=10L;rng=10L}
        let partsInRange = partsInRange (1L,5L) range
        Assert.Equal(1, partsInRange.Length)
        Assert.Equal<range>( (1L,5L), partsInRange[0])


    [<Fact>]
    let testmappingparts7 () = 
        Assert.Equal<range>( (9L,1L), (partsInRange (5L,5L)  {dst=1L;src=5L;rng=4L})[1])
        Assert.Equal<range>( (100L,10L), (partsInRange (100L,10L)  {dst=1L;src=0L;rng=4L})|> List.head)
        Assert.Equal<range>( (100L,10L), (partsInRange (100L,10L)  {dst=1L;src=500L;rng=4L})|> List.head)
        Assert.Equal<range>( (0L,10L), (partsInRange (0L,10L)  {dst=1L;src=20L;rng=10L})|> List.head)
        Assert.Equal<range>( (1000L,100L), (partsInRange (1000L,100L) {dst=1L;src=5L;rng=4L})|> List.head)
        Assert.Equal<range>( (1L,4L), (partsInRange (5L,5L)  {dst=1L;src=5L;rng=4L})|> List.head)
        Assert.Equal<range>( (5L,5L), (partsInRange (5L,5L)  {dst=1L;src=1L;rng=10L})[0])

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
        let range = { dst=100L;src=3L;rng=4L}
        let partsInRange = partsInRange (1L,10L) range
        Assert.Equal<range>( (1L,2L), partsInRange[0])
        Assert.Equal<range>( (100L,4L), partsInRange[1])

    [<Fact>]
    let testmappingparts41 () = 
        Assert.Equal<range>( (0L,1L), (partsInRange (0L,1L)  {dst=0L;src=0L;rng=1L})|> List.head)
        Assert.Equal<range>( (1L,1L), (partsInRange (0L,1L)  {dst=1L;src=0L;rng=1L})|> List.head)
        Assert.Equal<range>( (10L,1L), (partsInRange (0L,1L) {dst=10L;src=0L;rng=1L})|> List.head)

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
    let ranges_stuff2() = 
        let input = readInit "testinput.txt" 
        let seeds,maps= input
        let ranges = pair seeds 
        let foo= ranges |> List.map (fun s -> mapRangeThroughMap maps s) 
        let (x,l)= ranges |> List.map (fun s -> mapRangeThroughMap maps s) |> List.collect (id) |> List.minBy (fun (x,y) -> x) 
        //Assert.Equal(46L, x) 
        //Console.WriteLine(x)
        ()
  

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