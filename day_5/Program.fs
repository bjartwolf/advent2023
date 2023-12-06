open System


module Program =
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
    [<Fact>]
    let part1() = 
        let input = readInit "input.txt" 
        let seeds,maps = input
        let locations  = seeds |> List.map (fun s -> mapSeedThroughMaps maps s)
        Assert.Equal(214922730L, locations |> List.min)

    open FSharp.Collections.ParallelSeq


    let [<EntryPoint>] main _ = 
        printf "HEI" 
        let input = readInit "input.txt" 
        let seeds,maps = input
        let pairs = pair seeds 
        let ranges = pairs |> PSeq.map (fun (first,number) -> [first .. first + number - 1L]) |> PSeq.toList|> List.collect (id) 
        let locations  = ranges |> PSeq.map (fun s -> mapSeedThroughMaps maps s) |> PSeq.toList |>List.min
        printf "%A" locations
        Console.ReadLine() |> ignore
        0