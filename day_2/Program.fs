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
  (*Each time, secret cubes in bg
    Game is row
   *)
  //type Cube = Red | Green | Blue
  type Draw = { Red: int; Green: int; Blue: int}
  type Game = { Id: int; Draws: Draw list }

  type PossibleGame = { Id: int}

  let constraint = { Red = 12; Green = 13; Blue = 14}


  // sum of ids of games
//  type Bag = 

module Program = let [<EntryPoint>] main _ = 0

