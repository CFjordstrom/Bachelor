open System.Text
open FSharp.Text.Lexing
open System.IO

open AbSyn

let parse (s : string) : Regex =
    Parser.Prog Lexer.Token
    <| LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)

[<EntryPoint>]
let main (filename : string) : Regex =
    let input = 
        try
            let inStream = File.OpenText filename
            let txt = inStream.ReadToEnd()
            inStream.Close()
            txt
        with
            | ex -> ""
    if input <> "" then
        let regex = 
            try 
                parse input
            with
                | Lexer.LexicalError (info,(line,col)) ->
                    printfn "%s at line %d, position %d\n" info line col
                    System.Environment.Exit 1
                    []
                | ex ->
                    if ex.Message = "parse error"
                    then printPos Parser.ErrorContextDescriptor
                    else printfn "%s" ex.Message
                    System.Environment.Exit 1
                    []
    regex
  else failwith "Invalid file name or empty file"