module Input =
    open System
    open System.IO
    open Xunit 

    let Empty = '.'
    let VerticalSplit = '|'
    let HorizontalSplit = '-'
    let Mirror1 = '/'
    let Mirror2 = '\\'

    type Map = char list list
    let readInit (filePath: string): Map =  
        File.ReadAllLines filePath
            |> Array.toList
            |> List.map (fun x -> x.ToCharArray() |> Array.toList)

    let prettyPrint (m: Map) =
        [for line in m do
            line |> String.Concat |> printfn "%A"
        ]

    
    let (+) (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
    // alle posisjoner i y, der y er negativ akse og x
    let North = (-1,0)
    let South = (1,0)
    let East = (0,1)
    let West = (0,-1)
    type Direction = N|S|E|W
    type Position = int*int // kan lage record 
    type Beam = Position * Direction 
    type Beams = Beam list

    let move ((p,d): Beam): Beam =
        match d with 
            | N -> (p+North, d) 
            | E -> (p+East, d) 
            | W -> (p+West, d) 
            | S -> (p+South, d) 

    let lookup (m:Map) ((y,x): Position) : Char option =
        if (y < 0 || y > m.Length - 1 || x < 0 || x > m[0].Length - 1) then
            None
        else
           Some (m[y][x])

    let eval (map:Map) (beam: Beam): Beam list =
        let p,d = beam
        let (y,x) = p
        let tile = lookup map p 
        match tile with
            | None -> []
            | Some tile -> match tile with 
                                | '.' -> [beam]
                                | '-' when d = E || d = W -> [beam]
                                | '-' when d = S || d = N -> [(p,E);(p,W)]

    [<Fact>]
    let testMove() = 
        Assert.Equal<Beam>(((2,1),S), move ((1,1),S) )
        Assert.Equal<Beam>(((0,1),N), move ((1,1),N) )
 
    [<Fact>]
    let test2 () = 
        let map = readInit "testinput.txt" 
        prettyPrint map |> ignore 
        Assert.Equal(10, map.Length) 

module Program = let [<EntryPoint>] main _ = 0