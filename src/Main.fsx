open System.Text
open FSharp.Text.Lexing
open System.IO

open AbSyn
open PrettyPrinter
open RegexToNFA
open NFAToDFA
open MinimiseDFA
open XFAToRegex
open RunDFA

let usage' =
    [  
        "Usage:\n";
        "dotnet run <option> <filename or regular language>\n\n";
        "Possible options are:\n";
        "   -regex\n";
        "   -mindfa\n";
        "   -dfa\n";
        "   -nfa\n\n";
        "Or\n";
        "dotnet run -run <filename or regular language> <input string>\n\n";
        "If the regular language contains complement of an expression '!', complement of a class '^' or all characters '.', then an alphabet must be provided using -alphabet <alphabet>\n\n";
        "e.g. dotnet run -regex /[^a-z]/ -alphabet a-z0-9";
    ]

let usage = List.fold (+) "" usage'

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
            if err.Message = "parse error" then 
                printfn "Parse error %s" Parser.ErrorContextDescriptor
            else 
                printfn "%s" err.Message
            System.Environment.Exit 1
            ([], Epsilon)
    else
        failwith "Invalid input"

let rec containsComplement (grammar : Grammar) (regex: ExtendedRegex) (visited : string list) =
    match regex with
    | Union (r1, r2) -> containsComplement grammar r1 visited || containsComplement grammar r2 visited
    | Seq (r1, r2) -> containsComplement grammar r1 visited || containsComplement grammar r2 visited
    | Class c ->
        match c with
        | ClassContent content -> false
        | Complement content -> true
    | ZeroOrMore r -> containsComplement grammar r visited
    | Nonterminal s ->
        if List.contains s visited then
            false
        else
            let productions = List.map (fun (nt, re) -> re) <| List.filter (fun (nt, re) -> nt = s) grammar
            let visited' = s :: visited
            List.exists (fun re -> containsComplement grammar re visited') productions
    | REComplement r -> true
    | Intersection (r1, r2) -> containsComplement grammar r1 visited || containsComplement grammar r2 visited
    | Epsilon -> false

let rec convertAndPrint (option : string) (grammar : Grammar) (regex: ExtendedRegex) (alphabet : Alphabet option) : unit =
    match containsComplement grammar regex [], alphabet with
    | true, None -> printfn "The regular language contains complement of an expression '!', complement of a class '^' or all characters '.', so an alphabet must be provided using -alphabet <alphabet>"
    | false, Some a -> convertAndPrint option grammar regex None
    | _, _ ->
        match option with
        | "-regex" -> printfn "%s" (ppRegex << xfaToRegex << minimiseDFA << nfaToDFA <| regexToNFA grammar regex alphabet)
        | "-mindfa" -> printfn "%s" (ppDFA << minimiseDFA << nfaToDFA <| regexToNFA grammar regex alphabet)
        | "-dfa" -> printfn "%s" (ppDFA << nfaToDFA <| regexToNFA grammar regex alphabet)
        | "-nfa" -> printfn "%s" (ppNFA <| regexToNFA grammar regex alphabet)
        | _ -> printfn "%s" usage

let rec runAndPrint (grammar : Grammar) (regex: ExtendedRegex) (input : string) (alphabet : Alphabet option) : unit =
    match containsComplement grammar regex [], alphabet with
    | true, None -> printfn "The regular language contains complement so an alphabet must be provided using -alphabet <alphabet>"
    | false, Some a -> runAndPrint grammar regex input None
    | _, _ ->
        let dfa = minimiseDFA << nfaToDFA <| regexToNFA grammar regex alphabet
        if runDFA input dfa then
            printfn "The input string \"%s\" was accepted by the regular language represented by %s" input (ppRegex <| xfaToRegex dfa)
        else
            printfn "The input string \"%s\" was rejected by the regular language represented by %s" input (ppRegex <| xfaToRegex dfa)

[<EntryPoint>]
let main (args : string[]) : int =
    try
        match args with
        | [|option; regLang|] ->
            let (grammar, regex) = parseRegLang regLang
            convertAndPrint option grammar regex None
        | [|option; regLang; "-alphabet"; alphabet|] ->
            let (grammar, regex) = parseRegLang regLang
            let alphabet' = 
                Parser.ClassContent Lexer.Token
                <| LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes alphabet)
            convertAndPrint option grammar regex (Some alphabet')
        | [|"-run"; regLang; input|] ->
            let (grammar, regex) = parseRegLang regLang
            runAndPrint grammar regex input None
        | [|"-run"; regLang; input; "-alphabet"; alphabet|] ->
            let (grammar, regex) = parseRegLang regLang
            let alphabet' = 
                Parser.ClassContent Lexer.Token
                <| LexBuffer<_>.FromBytes (Encoding.UTF8.GetBytes alphabet)
            runAndPrint grammar regex input (Some alphabet')
        | _ ->
            printfn "%s" usage
        0
    with
    | MyError err -> 
        printfn "%s" err
        System.Environment.Exit 1
        1
    | err ->
        if err.Message = "The given key was not present in the dictionary." then
            printfn "%s" "Unexpected error during conversion"
        else
            printfn "%s" err.Message
        System.Environment.Exit 1
        1