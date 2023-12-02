module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): string [] = 
      System.IO.File.ReadAllLines filePath 
        //let numbers = line.Split(",")
        
        //numbers |> Array.map(fun f -> Int32.Parse(f)) |> Array.toList

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(100, input.Length) 

module Advent = 
  type Draw = { Red: int; Green: int; Blue: int}
  type Game = { Id: int; Draws: Draw list }

  type PossibleGame = { Id: int }
  type GameResult = PossibleGame option 

  let maxCubes = { Red = 12; Green = 13; Blue = 14}

  let game1 = {Id = 1; Draws = [ { Blue = 3; Red = 4; Green = 0 } ; { Red = 1; Green = 2; Blue =6 }; { Green = 2; Red = 0; Blue = 1} ]}
  let game2 = {Id = 2; Draws = [ { Blue = 1; Green = 2; Red = 0}; {Green = 3; Blue = 4; Red = 1}; { Green = 1; Blue = 1; Red = 0 }] }
  let game3 = {Id = 3; Draws = [ { Green = 8; Blue = 6; Red = 20}; { Blue = 5; Red = 4; Green = 13}; { Green = 5; Red = 1; Blue = 0} ] }
  let game4 = {Id = 4; Draws = [ { Green = 1; Red = 3; Blue = 6 }; {Green = 3; Red = 6; Blue = 0}; {Green = 3; Blue = 15; Red = 14}] }
  let game5 = {Id = 5; Draws = [ { Red = 6; Blue = 1; Green = 3}; {Blue = 2; Red = 1; Green = 2}] }

  // game 1, 2 og 5 er possible

  // sum of ids of games
//  type Bag = 

module Program = let [<EntryPoint>] main _ = 0

