// Learn more about F# at http://fsharp.org

open System

let A_Parser (str: string) : (Boolean * string) =
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

let andThen parser1 parser2 =
  let innerFn input =
    let result1 = run parser1 input

    match result1 with
    | Failure err ->
      Failure err
    | Success (value1, remaining1) ->
      let result2 = run parser2 remaining1

      match result2 with
      | Failure err ->
        Failure err

      | Success (value2, remaining2) ->
        let newValue = (value1,value2)
        Success (newValue, remaining2)

  Parser innerFn

let ( .>>. ) = andThen

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

[<EntryPoint>]
let main argv =
    let (_,b:string) = (A_Parser "ABC")
    printfn "%s" b
    0 // return an integer exit code
