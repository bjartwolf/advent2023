﻿module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): string []= 
        IO.File.ReadAllLines(filePath)

    type Position = int*int
    type Pipe = NS | EW | NE | NW | SW | SE 
    type Direction = W | N | E | S 
    type PipeMap = Map<Position, Pipe>

    type Tile = PipeTile of Pipe | StartPosition of Position

    let parsePipeMap (mapinput: string[]): PipeMap*Position=
            let pipeTiles : (Position*Pipe) list = [
                for i in 0 .. (mapinput.Length - 1) do
                    let chars = mapinput[i].ToCharArray()
                    for charIndex in 0 .. (chars.Length - 1) do
                        let char = chars[charIndex]
                        let tile:Tile option = match char with 
                                                    | '|' -> Some (PipeTile NS)
                                                    | '-' -> Some (PipeTile EW)
                                                    | 'L' -> Some (PipeTile NE)
                                                    | 'J' -> Some (PipeTile NW)
                                                    | '7' -> Some (PipeTile SW)
                                                    | 'F' -> Some (PipeTile SE)
                                                    | _ -> None
                        match tile with 
                            | Some (PipeTile p) -> yield ((i,charIndex), p)
                            | _ -> () 
            ]
            let startPosition = [
                for i in 0 .. (mapinput.Length - 1) do
                    let chars = mapinput[i].ToCharArray()
                    for charIndex in 0 .. (chars.Length - 1) do
                        let char = chars[charIndex]
                        match char with 
                              | 'S' -> yield (i,charIndex)
                              | _ -> () 
            ] 
            let pipeMap = Map.ofList pipeTiles 
            (pipeMap, startPosition.Head)

    [<Fact>]
    let testMapParserTestMap1 () = 
        let input = readInit "testinput1.txt" 
        let pipeMap,startPosition = parsePipeMap input
        Assert.Equal(EW, Map.find (0,0) pipeMap) 
        Assert.Equal(SE, Map.find (4,4) pipeMap) 
        Assert.Equal<Position>((1,1), startPosition)

    [<Fact>]
    let testMapParserTestMap2 () = 
        let input = readInit "testinput2.txt" 
        let pipeMap,startPosition = parsePipeMap input
        Assert.Equal(SW, Map.find (0,0) pipeMap) 
        Assert.Equal(NW, Map.find (4,4) pipeMap) 
        Assert.Equal<Position>((2,0), startPosition)


    let move (direction: Direction) ((y,x): Position): Position  =
        match direction with 
            | N -> (y-1,x)
            | W -> (y,x-1)
            | E -> (y,x+1)
            | S -> (y+1,x)

    let nextPosition (pipe: Pipe) (position: Position ) (enteringDirection: Direction): Position * Direction =
        match pipe, enteringDirection with 
            // the straights
            | NS, S -> move S position, S 
            | NS, N -> move N position, N 
            | EW, E -> move E position, E 
            | EW, W -> move W position, W 
            // the bends
            // └ 
            | NE, S -> move E position, E 
            | NE, W -> move N position, N  
            //  ┘
            | NW, S -> move W position, W 
            | NW, E -> move N position, N  
            //  ┐
            | SW, N -> move W position, W
            | SW, E -> move S position, S
            //  ┍ 
            | SE, N -> move E position, E
            | SE, W -> move S position, S 
            | _ -> failwithf "%A %A does not work" pipe enteringDirection 

    let findStartDir (position: Position) (map: Map<Position,Pipe>): Direction =
        // check north
        let north = move N position
        let south = move S position
        let east = move E position
        let west = move W position
        let northTile = Map.tryFind north map 
        let southTile = Map.tryFind south map
        let eastTile = Map.tryFind east map 
        let westTile = Map.tryFind west map 
        match northTile, southTile, eastTile, westTile with
            | Some n,_,_,_ when n = NS || n = SW || n = SE -> N 
            | _,Some s,_,_ when s = NS || s = NW || s = NE -> S 
            | _,_,Some e,_ when e = EW || e = NW || e = SW -> E 
            | _,_,_,Some w when w = EW || w = NE || w = SE -> W 
            | _ -> failwithf "There has to be a direction from %A" position

    [<Fact>]
    let testStartFindDirectionTest1() = 
        let input = readInit "testinput1.txt" 
        let pipeMap,startPosition = parsePipeMap input
        let startDirection = findStartDir startPosition pipeMap 
        Assert.Equal<Direction>(S, startDirection)

    [<Fact>]
    let testStartFindDirectionTest2() = 
        let input = readInit "testinput2.txt" 
        let pipeMap,startPosition = parsePipeMap input
        let startDirection = findStartDir startPosition pipeMap 
        Assert.Equal<Direction>(S, startDirection)

    [<Fact>]
    let testStartFindDirectionInput() = 
        let input = readInit "input.txt" 
        let pipeMap,startPosition = parsePipeMap input
        let startDirection = findStartDir startPosition pipeMap 
        Assert.Equal<Direction>(N, startDirection)
          
    let walkMap (startPosition: Position) (map: PipeMap): int =
        let rec walkMapInner (steps: int) (position: Position) (direction: Direction) =
            if position = startPosition then steps
            else 
                // I am at 2,1 going south
                let pipe = Map.find position map
                // hitting a pipe going south
                let nextPosition, nextDirection = nextPosition pipe position direction
                // then I end up in 3,1, entering direction is south
                walkMapInner (steps + 1) nextPosition nextDirection
        let startDirection = findStartDir startPosition map
        let nextPosition = move startDirection startPosition 
        let steps = walkMapInner 1 nextPosition startDirection 
        steps/2

    [<Fact>]
    let testWalkMap1() = 
        let input = readInit "testinput1.txt" 
        let pipeMap,startPosition = parsePipeMap input
        let steps = walkMap startPosition pipeMap
        Assert.Equal(4, steps)

    [<Fact>]
    let testWalkMap2() = 
        let input = readInit "testinput2.txt" 
        let pipeMap,startPosition = parsePipeMap input
        let steps = walkMap startPosition pipeMap
        Assert.Equal(8, steps)

    [<Fact>]
    let testWalkMapLarge() = 
        let input = readInit "input.txt" 
        let pipeMap,startPosition = parsePipeMap input
        let steps = walkMap startPosition pipeMap
        Assert.Equal(6828, steps)


    [<Fact>]
    let testDirectionSEGoingW() = 
        let nextPosition, nextDirection = nextPosition SE (3,4) W
        Assert.Equal<Position>( (4,4), nextPosition)
        Assert.Equal<Direction>(S, nextDirection) 

    [<Fact>]
    let testDirectionSEGoingN() = 
        let nextPosition, nextDirection = nextPosition SE (3,4) N
        Assert.Equal<Position>( (3,5), nextPosition)
        Assert.Equal<Direction>(E, nextDirection) 

    [<Fact>]
    let testDirectionSWGoingN() = 
        let nextPosition, nextDirection = nextPosition SW (3,4) N
        Assert.Equal<Position>( (3,3), nextPosition)
        Assert.Equal<Direction>(W, nextDirection) 

    [<Fact>]
    let testDirectionSWGoingE() = 
        let nextPosition, nextDirection = nextPosition SW (3,4) E
        Assert.Equal<Position>( (4,4), nextPosition)
        Assert.Equal<Direction>(S, nextDirection) 

    [<Fact>]
    let testDirectionEWGoingE() = 
        let nextPosition, nextDirection = nextPosition EW (3,4) E
        Assert.Equal<Position>( (3,5), nextPosition)
        Assert.Equal<Direction>(E, nextDirection) 

    [<Fact>]
    let testDirectionEWGoingW() = 
        let nextPosition, nextDirection = nextPosition EW (3,4) W
        Assert.Equal<Position>( (3,3), nextPosition)
        Assert.Equal<Direction>(W, nextDirection) 

    [<Fact>]
    let testDirectionNEGoingW() = 
        let nextPosition, nextDirection = nextPosition NE (3,4) W
        Assert.Equal<Position>( (2,4), nextPosition)
        Assert.Equal<Direction>(N, nextDirection) 

    [<Fact>]
    let testDirectionNEGoingS() = 
        let nextPosition, nextDirection = nextPosition NE (3,4) S
        Assert.Equal<Position>( (3,5), nextPosition)
        Assert.Equal<Direction>(E, nextDirection) 
  
    [<Fact>]
    let testDirectionNWGoingS() = 
        let nextPosition, nextDirection = nextPosition NW (3,4) S
        Assert.Equal<Position>( (3,3), nextPosition)
        Assert.Equal<Direction>(W, nextDirection) 

    [<Fact>]
    let testDirectionNWGoingE() = 
        let nextPosition, nextDirection = nextPosition NW (2,5) E
        Assert.Equal<Position>( (1,5), nextPosition)
        Assert.Equal<Direction>(N, nextDirection) 
    
    [<Fact>]
    let testDirectionSWGoingW() = 
        let moveNextPos () = nextPosition SW (3,4) W |> ignore
        Assert.Throws<Exception>(Action( fun () -> moveNextPos () ))
 
    [<Fact>]
    let test2 () = 
        let input = readInit "testinput1.txt" 
        Assert.Equal(5, input.Length) 

module Program = let [<EntryPoint>] main _ = 0