module Input =
    type Draw = { Red: int; Green: int; Blue: int}
    type Game = { Id: int; Draws: Draw list }
    let game1 = {Id = 1; Draws = [ { Blue = 3; Red = 4; Green = 0 } ; { Red = 1; Green = 2; Blue =6 }; { Green = 2; Red = 0; Blue = 0} ]}
    let game2 = {Id = 2; Draws = [ { Blue = 1; Green = 2; Red = 0}; {Green = 3; Blue = 4; Red = 1}; { Green = 1; Blue = 1; Red = 0 }] }
    let game3 = {Id = 3; Draws = [ { Green = 8; Blue = 6; Red = 20}; { Blue = 5; Red = 4; Green = 13}; { Green = 5; Red = 1; Blue = 0} ] }
    let game4 = {Id = 4; Draws = [ { Green = 1; Red = 3; Blue = 6 }; {Green = 3; Red = 6; Blue = 0}; {Green = 3; Blue = 15; Red = 14}] }
    let game5 = {Id = 5; Draws = [ { Red = 6; Blue = 1; Green = 3}; {Blue = 2; Red = 1; Green = 2}] }


    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): string [] = 
      let lines = System.IO.File.ReadAllLines filePath 
      lines

    let rawGame1 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
    let rawGame2 = "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
    let rawGame3 = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red" 
    let rawGame4 = "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red" 
    let rawGame5 = "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" 

    open System.Text.RegularExpressions
    let parseDraw (draw: string): Draw = 
        let redPattern = "(\d+)\s* red"
        let redMatch = Regex.Match(draw, redPattern)
        let red =if redMatch.Success && redMatch.Groups.[1].Success then
                    int redMatch.Groups.[1].Value
                 else
                    0 
        let greenPattern = "(\d+)\s* green"
        let greenMatch = Regex.Match(draw, greenPattern)
        let green =if greenMatch.Success && greenMatch.Groups.[1].Success then
                    int greenMatch.Groups.[1].Value
                   else
                    0 
        let bluePattern = "(\d+)\s* blue"
        let blueMatch = Regex.Match(draw, bluePattern)
        let blue =if blueMatch.Success && blueMatch.Groups.[1].Success then
                    int blueMatch.Groups.[1].Value
                  else
                    0 
        { Red = red; Green = green; Blue = blue}

    [<Fact>]
    let game1_draw1_parsesCorrectly() = 
        let draw = parseDraw "3 blue, 4 red;"
        Assert.Equal(4, draw.Red)
        Assert.Equal(0, draw.Green)
        Assert.Equal(3, draw.Blue)

    let parseLine (line: string): Game =
        let gamesAndIds = line.Split(":")
        let gameId = int (gamesAndIds[0].Replace("Game ", ""))
        let draws = gamesAndIds[1].Split(";") |> Array.map (fun x -> parseDraw x) |> Array.toList
        { Id = gameId; Draws = draws}

    let parseAllGames (path: string): Game list =
      let lines = System.IO.File.ReadAllLines path 
      let games = lines |> Array.map (fun x -> parseLine x)
      games |> Array.toList

    [<Fact>]
    let allGamesParsesCorrectly () =
        let allGames = parseAllGames "testinput.txt"
        Assert.Equal<Game list>([game1; game2; game3; game4; game5], allGames)

    [<Fact>]
    let game1_parsesCorrectly() = 
        let game = parseLine rawGame1 
        Assert.Equal(game1, game)

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(100, input.Length) 

module Advent = 
  open Xunit 
  open Input
  type PossibleGame = { Id: int }
  type GameResult = PossibleGame option 

  let maxCubes = { Red = 12; Green = 13; Blue = 14}


  let processGame (game: Game): GameResult =
    let allGamesPass = game.Draws |> List.forall(fun draw -> match draw with
                                         | draw when draw.Red <= maxCubes.Red && draw.Green <= maxCubes.Green && draw.Blue <= maxCubes.Blue -> true 
                                         | _ -> false)
    if allGamesPass then 
      Some { Id = game.Id}
    else 
      None

  [<Fact>]
  let sum_of_all_games() =
    let sum = [game1; game2; game3; game4; game5] |> List.choose (processGame) |> List.map (fun r -> r.Id) |> List.sum
    Assert.Equal(8, sum)

  [<Fact>]
  let game1_is_possible() = 
    let gamePassed = processGame game1
    Assert.Equal(game1.Id, gamePassed.Value.Id)
    
  [<Fact>]
  let game2_is_possible() = 
    let gamePassed = processGame game2
    Assert.Equal(game2.Id, gamePassed.Value.Id)
 
  [<Fact>]
  let game5_is_possible() = 
    let gamePassed = processGame game5
    Assert.Equal(game5.Id, gamePassed.Value.Id)

  [<Fact>]
  let game3_is_NOT_possible() = 
    let gamePassed = processGame game3
    Assert.Equal(None, gamePassed )

  [<Fact>]
  let game4_is_NOT_possible() = 
    let gamePassed = processGame game4
    Assert.Equal(None, gamePassed )

  // sum of ids of games
//  type Bag = 

module Program = let [<EntryPoint>] main _ = 0

