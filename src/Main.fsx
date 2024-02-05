open System.Text
open FSharp.Text.Lexing
open System.IO

open AbSyn
open RegexConversion

let parse (s : string) : Regex =
    Parser.Start Lexer.Token
    <| LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)

let parseRegex (filename : string) : Regex =
    let inStream = File.OpenText filename
    let txt = inStream.ReadToEnd()
    inStream.Close() 

    if txt <> "" then
        parse txt
    else failwith "invalid input"

[<EntryPoint>]
let main (args : string[]) : int =
    let regex = parseRegex args.[0]
    printfn "Syntax tree:\n%A\n" regex
    printfn "Original regex:\n%s" (RegexToString regex)
    0