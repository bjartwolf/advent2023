module Input =
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


    let move (direction: Direction) ((x,y): Position): Position  =
        match direction with 
            | N -> (x,y+1)
            | W -> (x-1,y)
            | E -> (x+1,y)
            | S -> (x,y-1)

    let nextPosition (pipe: Pipe) (position: Position ) (direction: Direction): Position * Direction =
        match pipe, direction with 
            | NS, S -> move S position, S 
            | NS, N -> move N position, N 
            | EW, E -> move E position, E 
            | EW, W -> move W position, W 
            | NE, S -> move S position, E 
            | NE, W -> move W position, N  
            | NW, S -> move S position, W 
            | NW, E -> move E position, N  
            | SW, N -> move N position, W
            | SW, E -> move E position, S
            | SE, N -> move N position, E
            | SE, W -> move W position, S 
            | _ -> failwithf "%A %A does not work" pipe direction 

    let findStartDir (position: Position) (map: Map<Position,Pipe>): Direction =
        // check north
        let northTile = Map.tryFind (move N position) map 
        let southTile = Map.tryFind (move S position) map
        let eastTile = Map.tryFind (move E position) map 
        let westTile = Map.tryFind(move W position) map 
        match northTile, southTile, eastTile, westTile with
            | Some n,_,_,_ when n = NS || n = SW || n = SE -> N 
            | _,Some s,_,_ when s = NS || s = NW || s = NE -> S 
            | _,_,Some e,_ when e = EW || e = NW || e = SW -> E 
            | _,_,_,Some w when w = EW || w = NE || w = SE -> W 
            | _ -> failwithf "There has to be a direction from %A" position
        



    [<Fact>]
    let testDirectionSEGoingW() = 
        let nextPosition, nextDirection = nextPosition SE (3,4) W
        Assert.Equal<Position>( (2,4), nextPosition)
        Assert.Equal<Direction>(S, nextDirection) 

    [<Fact>]
    let testDirectionSEGoingN() = 
        let nextPosition, nextDirection = nextPosition SE (3,4) N
        Assert.Equal<Position>( (3,5), nextPosition)
        Assert.Equal<Direction>(E, nextDirection) 

    [<Fact>]
    let testDirectionSWGoingN() = 
        let nextPosition, nextDirection = nextPosition SW (3,4) N
        Assert.Equal<Position>( (3,5), nextPosition)
        Assert.Equal<Direction>(W, nextDirection) 

    [<Fact>]
    let testDirectionSWGoingE() = 
        let nextPosition, nextDirection = nextPosition SW (3,4) E
        Assert.Equal<Position>( (4,4), nextPosition)
        Assert.Equal<Direction>(S, nextDirection) 

    [<Fact>]
    let testDirectionEWGoingE() = 
        let nextPosition, nextDirection = nextPosition EW (3,4) E
        Assert.Equal<Position>( (4,4), nextPosition)
        Assert.Equal<Direction>(E, nextDirection) 

    [<Fact>]
    let testDirectionEWGoingW() = 
        let nextPosition, nextDirection = nextPosition EW (3,4) W
        Assert.Equal<Position>( (2,4), nextPosition)
        Assert.Equal<Direction>(W, nextDirection) 

    [<Fact>]
    let testDirectionNEGoingW() = 
        let nextPosition, nextDirection = nextPosition NE (3,4) W
        Assert.Equal<Position>( (2,4), nextPosition)
        Assert.Equal<Direction>(N, nextDirection) 

    [<Fact>]
    let testDirectionNEGoingS() = 
        let nextPosition, nextDirection = nextPosition NE (3,4) S
        Assert.Equal<Position>( (3,3), nextPosition)
        Assert.Equal<Direction>(E, nextDirection) 
  
    [<Fact>]
    let testDirectionNWGoingS() = 
        let nextPosition, nextDirection = nextPosition NW (3,4) S
        Assert.Equal<Position>( (3,3), nextPosition)
        Assert.Equal<Direction>(W, nextDirection) 

    [<Fact>]
    let testDirectionNWGoingE() = 
        let nextPosition, nextDirection = nextPosition NW (3,4) E
        Assert.Equal<Position>( (4,4), nextPosition)
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