let input =
  match fsi.CommandLineArgs with
  | [| _; filename |] -> System.IO.File.ReadAllLines(filename)
  | _ -> System.IO.File.ReadAllLines("sample.txt")
  |> Array.toList

let firstDigitSimple (line: string) =
  line
  |> Seq.find (System.Char.IsDigit)
  |> string

let lastDigitSimple (line: string) =
  line
  |> Seq.rev
  |> Seq.find (System.Char.IsDigit)
  |> string

let allSubstringsUpToLength (l: int) (s: string) = seq {
  let stringLength = s.Length
  for i = 0 to (stringLength) do
    for l' = 1 to l do
      if i + l' <= stringLength then
        yield s.Substring(i, l')
}

let allDigitsComplex (line: string) =
  let possibleDigits =
    let addStringToMap (s: string) v m =
      m
      |> Map.add s {|Length = s.Length; Value = v |}
      |> Map.add (string v) {| Length = 1; Value = v |}

    Map.empty
    |> addStringToMap "one" 1
    |> addStringToMap "two" 2
    |> addStringToMap "three" 3
    |> addStringToMap "four" 4
    |> addStringToMap "five" 5
    |> addStringToMap "six" 6
    |> addStringToMap "seven" 7
    |> addStringToMap "eight" 8
    |> addStringToMap "nine" 9
    |> addStringToMap "ten" 0

  let longestPossibleDigit =
    possibleDigits
    |> Map.values
    |> Seq.map (fun v -> v.Length)
    |> Seq.max

  line
    |> allSubstringsUpToLength longestPossibleDigit
    |> Seq.map (fun s -> possibleDigits |> Map.tryFind s)
    |> Seq.choose id
    |> Seq.map (fun v -> v.Value)
    |> Seq.toList


let part1 lines =
  let makeInt (firstDigit, lastDigit) = (System.Int32.Parse(firstDigit) * 10) + System.Int32.Parse(lastDigit)

  lines
  |> List.map (fun line -> ((firstDigitSimple line), (lastDigitSimple line)))
  |> List.map makeInt
  |> List.sum
  |> sprintf "%A"

let part2 lines =
  lines
  |> List.map (fun l ->
      let digits = allDigitsComplex l
      (digits |> List.head, digits |> List.rev |> List.head ))
  |> List.map (fun (first, last) -> (first * 10) + last)
  |> List.sum
  |> string

part1 input |> printfn "%s"
part2 input |> printfn "%s"