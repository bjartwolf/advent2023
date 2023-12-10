module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): string []= 
        IO.File.ReadAllLines(filePath)

    let North = (0,1)
    let South = (0,-1)
    let West = (1,0)
    let East = (-1,0)

    type Position = int*int
    type Pipe = NS | EW | NE | NW | SW | SE 
    type Direction = W | N | E | S 

    let move (direction: Direction) ((x,y): Position)  =
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