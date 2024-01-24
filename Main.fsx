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
    else failwith "idk"

    (*let txt = try  // read text from file given as parameter with added extension
                let inStream = File.OpenText (filename + ".fo")
                let txt = inStream.ReadToEnd()
                inStream.Close()
                txt
            with  // or return empty string
                
    if txt <> "" then // valid file content
        let regex =
            try
                parse txt
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
    else failwith "Invalid file name or empty file"*)

[<EntryPoint>]
let main (args : string[]) : int =
    (*if args.Length <> 1 then
        printfn "dotnet run ../tests/filename"
        1
    else
        let filename = args.[0]
        let regex = parseRegex filename
        0*)
    let regex = parseRegex args.[0]
    printfn "%s" (RegexToString regex)
    0