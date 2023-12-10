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
            | NE, N -> move N position, E 
            | NE, W -> move E position, S  // blir snudd sydover baklengs om du kommer fra west
            | NW, N -> move N position, W 
            | NW, E -> move E position, S  
            | SW, S -> move S position, W
            | SW, E -> move E position, N
            | SE, S -> move S position, E
            | SE, W -> move W position, N 
            | _ -> failwithf "%A %A does not work" pipe direction 


    [<Fact>]
    let testDirectionEWGoingW() = 
        let nextPosition, nextDirection = nextPosition EW (3,4) W
        Assert.Equal<Position>( (2,4), nextPosition)
        Assert.Equal<Direction>(W, nextDirection) 
 

    [<Fact>]
    let test2 () = 
        let input = readInit "testinput1.txt" 
        Assert.Equal(5, input.Length) 

module Program = let [<EntryPoint>] main _ = 0