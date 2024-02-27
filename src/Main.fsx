open System.Text
open FSharp.Text.Lexing
open System.IO

open AbSyn
open PrettyPrinter
open RegexToNFA
open NFAToDFA
open MinimiseDFA

let parse (s : string) : Regex =
    Parser.Start Lexer.Token
    <| LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes s)

let parseRegex (input : string) : Regex =
    if System.IO.File.Exists input then
        let inStream = File.OpenText input
        let txt = inStream.ReadToEnd()
        inStream.Close() 

        if txt <> "" then
            parse txt
        else failwith "invalid input"
    else
        parse input

[<EntryPoint>]
let main (args : string[]) : int =
    match args.Length with
    | 1 ->
        let regex = parseRegex args.[0]
        //printfn "Regex Syntax tree:\n%A\n" regex
        //printfn "Regex:\n%s\n" (ppRegex regex)
        let nfa = regexToNFA regex
        let (start, map, alphabet) = nfa
        //printfn "NFA:\n%A\n" nfa
        //printfn "NFA:\n%s\n" (ppNFA nfa)
        
        let dfa = nfaToDFA nfa
        //printfn "DFA:\n%A\n" dfa
        //printfn "DFA:\n%s\n" (ppDFA dfa)

        let minimisedDFA = minimiseDFA dfa
        printfn "Minimised DFA:\n%s\n" (ppDFA minimisedDFA)
        0
    | _ -> printfn "Usage: dotnet run <filename or regex>"; 1