// Learn more about F# at http://fsharp.org

open System

let A_Parser str : (Boolean * string) =
  if String.IsNullOrEmpty(str) then
    (false, "")
  else if str.[0] = 'A' then
    let remaining: string = str.[1..]
    (true, remaining)
  else
    (false, str)

type Result<'a> =
  | Success of 'a
  | Failure of string

type Parser<'T> = Parser of (string -> Result<'T * string>)

let pchar charToMatch =
  let innerFn str =
    if String.IsNullOrEmpty(str) then
      Failure "No more input"
    else
      let first = str.[0]
      if first = charToMatch then
        let remaining = str.[1..]
        Success (charToMatch,remaining)
      else
        let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
        Failure msg
  Parser innerFn

let run parser input =
  let (Parser innerFn) = parser
  innerFn input

let orElse parser1 parser2 =
  let innerFn input =
    let result1 = run parser1 input

    match result1 with
    | Success result ->
      result1
    | Failure err ->
      let result2 = run parser2 input
      result2

  Parser innerFn

let ( <|> ) = orElse

let choice listOfParsers =
  List.reduce ( <|> ) listOfParsers

let anyOf listOfChars =
  listOfChars
  |> List.map pchar
  |> choice

let bindP f p =
  let innerFn input =
    let result1 = run p input
    match result1 with
    | Failure err ->
      Failure err
    | Success (value1, remainingInput) ->
      let p2 = f value1
      run p2 remainingInput

  Parser innerFn

let ( >>= ) p f = bindP f p

let returnP x =
  let innerFn input =
    Success (x,input)

  Parser innerFn

let mapP f =
  bindP (f >> returnP)

let ( |>> ) x f = mapP f x

let andThen p1 p2 =
  p1 >>= (fun p1Result -> p2 >>= (fun p2Result -> returnP (p1Result, p2Result)))

let ( .>>. ) = andThen


let parseDigit = anyOf ['0'..'9']

let parseThreeDigitsAsStr =
  let tupleParser =
    parseDigit .>>. parseDigit .>>. parseDigit

  let transformTuple ((c1, c2), c3) =
    String [| c1; c2; c3 |]

  mapP transformTuple tupleParser

let applyP fP xP =
  fP >>= (fun f -> xP >>= (fun x -> returnP (f x)))

let ( <*> ) = applyP

let lift2 f xP yP =
  returnP f <*> xP <*> yP

let addP =
  lift2 (+)

let startsWith (str:string) (prefix:string) =
  str.StartsWith(prefix)

let startsWithP =
  lift2 startsWith

let rec sequence parserList =
  let cons head tail = head::tail

  let consP = lift2 cons

  match parserList with
  | [] ->
    returnP []
  | head::tail ->
    consP head (sequence tail)

let charListToStr charList =
  String(List.toArray charList)

let pstring str =
  str
  |> List.ofSeq
  |> List.map pchar
  |> sequence
  |> mapP charListToStr

let rec parseZeroOrMore parser input =
  let firstResult = run parser input
  match firstResult with
  | Failure err ->
    ([], input)
  | Success (firstValue, inputAfterFirstParse) ->
    let (subsequentValues, remainingInput) =
      parseZeroOrMore parser inputAfterFirstParse
    let values = firstValue::subsequentValues
    (values, remainingInput)


let many parser =
  let rec innerFn input =
    Success (parseZeroOrMore parser input)

  Parser innerFn

let many1 p =
  p >>= (fun head -> many p >>= (fun tail -> returnP (head::tail)))

let opt p =
  let some = p |>> Some
  let none = returnP None
  some <|> none

let pint =
  let resultToInt (sign, digitList) =
    let i = String(List.toArray digitList) |> int
    match sign with
    | Some ch -> -i
    | None -> i

  let digit = anyOf ['0'..'9']

  let digits = many1 digit

  opt (pchar '-') .>>. digits |>> resultToInt

let (.>>) p1 p2 =
  p1 .>>. p2 |> mapP (fun (a,b) -> a)

let (>>.) p1 p2 =
  p1 .>>. p2 |> mapP (fun (a,b) -> b)

let between p1 p2 p3 =
  p1 >>. p2 .>> p3

let sepBy1 p sep =
  let sepThenP = sep >>. p
  p .>>. many sepThenP |>> fun (p, pList) -> p::pList

let sepBy p sep =
  sepBy1 p sep <|> returnP []

[<EntryPoint>]
let main argv =
  let (_,b) = (A_Parser "ABC")
  printfn "%s" b
  0 // return an integer exit code
