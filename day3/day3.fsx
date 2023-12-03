let input =
  match fsi.CommandLineArgs with
  | [| _; filename |] -> System.IO.File.ReadAllLines(filename)
  | _ -> System.IO.File.ReadAllLines("sample.txt")
  |> Array.toList

type CharacterSlot =
  | Empty
  | Digit of char
  | Symbol of char

module CharacterSlot =
  let isSymbol = function
                 | Symbol _ -> true
                 | _ -> false
  let isPossibleGear = function
                       | Symbol c when c = '*' -> true
                       | _ -> false
  let toString =
    function
    | Symbol c -> string c
    | Digit c -> string c
    | Empty -> "."

type SchematicNumber = {
  Value: int
  StartX: int
  EndX: int
  Y: int
}

let matrixCoordinates (m: 'a array array) = seq {
  let arraylength = m.Length - 1
  for y = 0 to arraylength do
    let rowLength = m[y].Length - 1
    for x = 0 to rowLength do
      yield (x, y)
}

module SchematicNumber =
  let length x = (x.EndX - x.StartX) + 1

  let coordinatesToCheck (xMax, yMax) n =
    let isValid (x, y) = x >= 0 && x <= xMax && y >= 0 && y <= yMax

    [
      yield (n.StartX - 1, n.Y)
      yield (n.EndX + 1, n.Y)
      for x = n.StartX - 1 to n.EndX + 1 do
        yield (x, n.Y - 1)
        yield (x, n.Y + 1)
    ]
    |> List.filter isValid

  let coordinatesInNumber n =
    [
      for x = n.StartX to n.EndX do
        yield (x, n.Y)
    ]

let digitRuns (row: CharacterSlot array) =
  seq {
    let mutable currentRunX = None
    let mutable currentRun = ""

    for x = 0 to row.Length - 1 do
      match row[x] with
      | Digit c ->
        currentRunX <-
          match currentRunX with
          | None -> Some x
          | Some x' -> Some x'
        currentRun <- currentRun + (string c)
      | _ ->
        match currentRunX with
        | Some x' -> yield (x', currentRun)
        | None -> ()
        currentRunX <- None
        currentRun <- ""
    match currentRunX with
    | Some x' -> yield (x', currentRun)
    | None -> ()
  }
  |> Seq.toList


let parseSchematicRowNumbers (rowIndex: int) (row: CharacterSlot array) =
  digitRuns row
  |> List.map (fun (x, s) -> {
    Value = (int s)
    StartX = x
    EndX = x + (s.Length - 1)
    Y = rowIndex
  })


let parseInput (lines: string seq) =
  let parseCharacter c =
    match c with
    | '.' -> Empty
    | c when System.Char.IsDigit(c) -> Digit c
    | _ -> Symbol c

  let characterSlotMatrix =
    lines
    |> Seq.map (fun s -> s.Trim() |> Seq.map parseCharacter |> Seq.toArray)
    |> Seq.toArray

  let schematicRowNumbers =
    characterSlotMatrix
    |> Array.toList
    |> List.mapi (fun y row -> parseSchematicRowNumbers y row)
    |> List.collect id

  (characterSlotMatrix, schematicRowNumbers)

let part1 input =
  let (matrix, numbers) = parseInput input

  if matrix.Length > 0 then
    let matrixMax = (matrix[0].Length - 1, matrix.Length - 1)

    let partNumbers =
      numbers
      |> List.filter (fun n ->
          let coords = SchematicNumber.coordinatesToCheck matrixMax n
          coords
          |> List.exists (fun (x, y) -> CharacterSlot.isSymbol (matrix[y][x])))

    partNumbers
    |> Seq.map (fun pn -> pn.Value)
    |> Seq.sum
  else 0

let part2 input =
  let (matrix, numbers) = parseInput input

  if matrix.Length > 0 then
    let (maxX, maxY) = (matrix[0].Length - 1, matrix.Length - 1)

    let partNumberMap =
      numbers
      |> Seq.collect (fun pn -> SchematicNumber.coordinatesInNumber pn |> Seq.map (fun c -> (c, pn)))
      |> Map.ofSeq

    let surroundingCoordinates (x, y) =
      [
        (x - 1, y)
        (x + 1, y)
        (x - 1, y - 1)
        (x - 1, y + 1)
        (x + 1, y - 1)
        (x + 1, y + 1)
        (x, y - 1)
        (x, y + 1)
      ]
      |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && x <= maxX && y <= maxY)

    let gearCoordinates =
      matrix
      |> matrixCoordinates
      |> Seq.filter (fun (x, y) ->
        CharacterSlot.isPossibleGear (matrix[y][x]))

    gearCoordinates
    |> Seq.map (fun (x, y) ->
        let surroundingPartNumbers =
          surroundingCoordinates (x, y)
          |> List.map (fun c -> partNumberMap |> Map.tryFind c)
          |> List.choose id
          |> List.distinct

        match surroundingPartNumbers with
        | [part1; part2] -> Some (part1.Value * part2.Value)
        | _ -> None
    )
    |> Seq.choose id
    |> Seq.sum
  else 0

part1 input |> printfn "%d"
part2 input |> printfn "%d"