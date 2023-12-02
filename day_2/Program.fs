module Input =
    open System

    type Draw = { Red: int; Green: int; Blue: int}
    type Game = { Id: int; Draws: Draw list }

    module AdventParser =  
        open System.Text.RegularExpressions
        let colors = [|"red"; "green"; "blue"|] 
        let pattern = (colors |> Array.map (fun color -> sprintf "(?<%s>\d+)\s* %s" color color))
        let combinedPattern = String.Join('|', pattern)
        let regex = new Regex(combinedPattern)
        let parseDraw (draw: string): Draw = 
            let matches = regex.Matches(draw)

            let res = [for regexMatch in matches do
                        for groupName in colors do
                            let group = regexMatch.Groups.[groupName]
                            if group.Success && group.Value <> "" then
                                yield groupName, group.Value]

            let redMatch = res |> List.tryFind(fun (name,value) -> name = "red")
            let red = match redMatch with
                            | Some (_,value) -> int value 
                            | None -> 0

            let greenMatch = res |> List.tryFind(fun (name,_) -> name = "green")
            let green = match greenMatch with
                            | Some (_,value) -> int value 
                            | None -> 0

            let blueMatch= res |> List.tryFind(fun (name,_) -> name = "blue")
            let blue = match blueMatch with
                            | Some (_,value) -> int value 
                            | None -> 0

            { Red = red; Green = green; Blue = blue}

    open AdventParser

    let parseLine (line: string): Game =
        let gamesAndIds = line.Split(":")
        let gameId = int (gamesAndIds[0].Replace("Game ", ""))
        let draws = gamesAndIds[1].Split(";") |> Array.map (fun x -> parseDraw x) |> Array.toList
        { Id = gameId; Draws = draws}

    let parseAllGames (path: string): Game list =
      let lines = System.IO.File.ReadAllLines path 
      let games = lines |> Array.map (fun x -> parseLine x)
      games |> Array.toList

module Advent = 
  open Xunit 
  open Input

  type PossibleGame = { Id: int }
  type GameResult = PossibleGame option 

  let maxCubes = { Red = 12; Green = 13; Blue = 14}


  let processGame (game: Game): GameResult =
    let allGamesPass = game.Draws |> List.forall(
        fun draw -> match draw with
                     | draw when draw.Red <= maxCubes.Red &&
                                 draw.Green <= maxCubes.Green 
                                 && draw.Blue <= maxCubes.Blue -> true 
                     | _ -> false)
    if allGamesPass then 
      Some { Id = game.Id}
    else 
      None

  [<Fact>]
  let sum_of_all_games_testdata() =
    let allGames = parseAllGames "testinput.txt"
    let sum = allGames |> List.choose (processGame) |> List.map (fun r -> r.Id) |> List.sum
    Assert.Equal(8, sum)

  [<Fact>]
  let sum_of_all_games_input() =
    let allGames = parseAllGames "input1.txt"
    let sum = allGames |> List.choose (processGame) |> List.map (fun r -> r.Id) |> List.sum
    Assert.Equal(2617, sum)

  let powerOfGame (game: Game): int =
    let minRed = game.Draws |> List.map (fun g -> g.Red) |> List.max
    let minGreen = game.Draws |> List.map (fun g -> g.Green) |> List.max
    let minBlue = game.Draws |> List.map (fun g -> g.Blue) |> List.max
    minRed * minGreen * minBlue
    
  [<Fact>]
  let sum_of_power_of_games_test_input() =
    let allGames = parseAllGames "testinput.txt"
    let sum = allGames |> List.map (powerOfGame)|> List.sum
    Assert.Equal(2286, sum)

  [<Fact>]
  let sum_of_power_of_games_real_input() =
    let allGames = parseAllGames "input1.txt"
    let sum = allGames |> List.map (powerOfGame)|> List.sum
    Assert.Equal(59795, sum)

module Program = let [<EntryPoint>] main _ = 0
