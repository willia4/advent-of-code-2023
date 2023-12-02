open System.Text.RegularExpressions

let input =
  match fsi.CommandLineArgs with
  | [| _; filename |] -> System.IO.File.ReadAllLines(filename)
  | _ -> System.IO.File.ReadAllLines("sample.txt")
  |> Array.toList

type Handful = {
  Red: int
  Green: int
  Blue: int
}

module Handful =
  let power h = (int64 h.Red) * (int64 h.Blue) * (int64 h.Green)

type Game = {
  Id: int
  Turns: Handful list
}

let parseHandful (s: string) : Handful =
  let handfulMap =
    s.Split(",")
    |> Seq.map (fun s -> s.Trim())
    |> Seq.map (fun s ->
        Regex("^(?<count>\\d+?) (?<color>.*)$").Match(s)
        |> function
           | m when m.Success -> (m.Groups["color"].Value, (int m.Groups["count"].Value))
           | _ -> failwithf "Could not parse \"%s\" as a handful" s)
    |> Map.ofSeq

  {Red = (handfulMap |> Map.tryFind "red" |> Option.defaultValue 0)
   Green = (handfulMap |> Map.tryFind "green" |> Option.defaultValue 0)
   Blue = (handfulMap |> Map.tryFind "blue" |> Option.defaultValue 0)}

let parseTurns (s: string) : Handful list =
  s.Split(";")
  |> Seq.map (fun s -> s.Trim())
  |> Seq.map parseHandful
  |> Seq.toList

let parseLine (line: string)=
  let gameRegex = Regex("^Game (?<gameId>\\d+?):(?<turns>.*)$")
  let (gameId, turns) =
    gameRegex.Match(line)
    |> function
       | m when m.Success -> m.Groups["gameId"].Value, m.Groups["turns"].Value
       | _ -> failwithf "Could not parse line \"%s\" as a game" line
    |> function (gameId, turns) -> (int gameId, parseTurns turns)

  { Id = gameId; Turns = turns }

let isPossible (test: Handful) (h: Handful list) =
  h
  |> List.forall (fun h -> h.Red <= test.Red && h.Blue <= test.Blue && h.Green <= test.Green)

let smallestPossible (h: Handful list) =
  {
    Red = h |> List.map (fun h -> h.Red) |> List.max
    Green = h |> List.map (fun h -> h.Green) |> List.max
    Blue = h |> List.map (fun h -> h.Blue) |> List.max
  }

let part1 lines =
  let games =
    lines
    |> List.map parseLine

  games
  |> List.filter (fun g -> g.Turns |> isPossible { Red = 12; Green = 13; Blue = 14 })
  |> List.map (fun g -> g.Id)
  |> List.sum


let part2 lines =
  let games =
    lines
    |> List.map parseLine

  games
  |> List.map (fun g -> smallestPossible g.Turns)
  |> List.map Handful.power
  |> List.sum

part1 input |> printfn "%d"
part2 input |> printfn "%d"