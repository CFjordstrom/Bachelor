open System.Text
open FSharp.Text.Lexing
open System.IO

open AbSyn
open PrettyPrinter
open RegexToNFA
open NFAToDFA
open MinimiseDFA
open XFAToRegex

let parse (s : string) : ExtendedRegex =
    Parser.Start Lexer.Token
    <| LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)

let parseRegLang (input : string) : ExtendedRegex =
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
            Epsilon
        | err -> 
            printfn "%A" err
            System.Environment.Exit 1
            Epsilon
    else
        failwith "invalid input"

[<EntryPoint>]
let main (args : string[]) : int =
    match args.Length with
    | 1 ->
        let regex = parseRegLang args.[0]
        //printfn "Regex Syntax tree:\n%A\n" regex
        printfn "Regex:\n%s\n" (ppRegex regex)
        let nfa = regexToNFA regex
        let (start, map, alphabet) = nfa
        //printfn "NFA:\n%A\n" nfa
        printfn "NFA:\n%s\n" (ppNFA nfa)
        
        let dfa = nfaToDFA nfa
        //printfn "DFA:\n%A\n" dfa
        printfn "DFA:\n%s\n" (ppDFA dfa)
        
        //let minimisedDFA = minimiseDFA dfa
        //printfn "Minimised DFA:\n%s\n" (ppDFA minimisedDFA)
        (*
        let backToRegex = xfaToRegex minimisedDFA
        printfn "Regex Syntax Tree after converting back from minimised DFA:\n%A\n" backToRegex
        printfn "Regex after converting back from minimised DFA:\n%s\n" (ppRegex backToRegex)*)
        0
    | _ -> printfn "Usage: dotnet run <filename or regex>"; 1