open System.Text
open FSharp.Text.Lexing
open System.IO

open AbSyn
open PrettyPrinter
open RegexToNFA
open NFAToDFA
open MinimiseDFA
open XFAToRegex
open CheckTransitions

let parse (s : string) : RegLang =
    Parser.Start Lexer.Token
    <| LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)

let parseRegLang (input : string) : RegLang =
    let text =
        if System.IO.File.Exists input then
            let inStream = File.OpenText input
            let txt = inStream.ReadToEnd()
            inStream.Close()
            txt
        else
            input
    if text <> "" then
        try
            parse text
        with
        | Lexer.LexicalError (info,(line, col)) ->
            printfn "%s at line %d, position %d\n" info line col
            System.Environment.Exit 1
            ([], Epsilon)
        | err -> 
            printfn "%s" err.Message
            System.Environment.Exit 1
            ([], Epsilon)
    else
        failwith "invalid input"

[<EntryPoint>]
let main (args : string[]) : int =
    match args with
    | [|flag; input|] ->
        let (transitions, regex) = parseRegLang input
        if checkTransitions transitions then
            let nfa = regexToNFA transitions regex
            printfn "%s" (ppNFA nfa)
            let dfa = minimiseDFA <| nfaToDFA nfa
            printfn "%s" (ppDFA dfa)
            let re = xfaToRegex dfa
            printfn "%s" (ppRegex re)
        else
            printfn "Illegal input"
    | _ -> printfn "Usage: dotnet run <options> <filename or regex>"
    0