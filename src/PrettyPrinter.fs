module PrettyPrinter

open AbSyn

let doesNotNeedParantheses (s : string) : bool =
    let last = s.[String.length s - 1]
    if String.length s = 1 || last = ')' || last = ']' then
        true
    else
        false

let ppChar (c : char) : string =
    match c with 
    | c when c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' -> string c
    | '/' | '|' | '*' | '+' | '?' | '{' | '}' | '(' | ')' | '\\' | '[' | ']' | '-' | '.' -> "\\" + string c
    | _ -> string c

let ppRange start current previous charsFromStart =
    if charsFromStart >= 3 then
        ppChar start + "-" + ppChar previous
    else
        ppChar start + ppChar previous + ppChar current

let rec compactContent (chars : char list) (start : char) (previous : char) (charsFromStart : int): string =
    match chars with
    | [] -> ppRange start previous previous charsFromStart
    | c :: cs ->
        if int c - charsFromStart = int start then
            compactContent cs start c (charsFromStart + 1)
        else
            let range = ppRange start c previous charsFromStart
            match cs with
            | [] -> range + ppChar c
            | [x] -> range + ppChar c + ppChar x
            | x :: xs -> range + compactContent xs c x 2

let ppClassContent (content : Set<char>) : string =
    match Set.toList content with
    | [] -> ""
    | [c] -> ppChar c
    | [c1; c2] -> ppChar c1 + ppChar c2
    | c :: cs -> compactContent cs c c 1

let ppClass (c : Class) : string =
    match c with
    | ClassContent content ->
        match Set.count content with
        | 0 -> "[]"
        | 1 -> ppClassContent content
        | _ -> "[" + ppClassContent content + "]"
    | Complement content -> 
        match Set.count content with
        | 0 -> "."
        | _ -> "[^" + ppClassContent content + "]"

let rec ppUnion (r1 : Regex) (r2 : Regex) : string =
    match r2 with
        | Concat([]) ->
            let s = ppRegex r1
            if doesNotNeedParantheses s then
                s + "?"
            else
                "(" + s + ")?"
        | _ -> "(" + ppRegex r1 + "|" + ppRegex r2 + ")"

and ppConcat (lst : Regex list) : string =
    match lst with
    | [] -> ""
    | ([r1; ZeroOrMore(r2)]) when r1 = r2 ->
        let s = ppRegex r1
        if doesNotNeedParantheses s then
            s + "+"
        else
            "(" + s + ")+"
    | r :: rs -> ppRegex r + ppConcat rs

and ppRegex (regex : Regex) : string =
    match regex with
    | Union (r1, r2) -> ppUnion r1 r2
    | Concat lst -> ppConcat lst
    | Class c -> ppClass c
    | ZeroOrMore r -> ppZeroOrMore r

and ppZeroOrMore (regex : Regex) : string =
    let string = ppRegex regex
    if doesNotNeedParantheses string then
        string + "*"
    else
        "(" + string + ")*"

let transitionToString (s1: State) (t : Transition) =
    match t with
    | (Some c, s2) -> string s1 + " -> " + string c + " " + string s2 + "\n"
    | (None, s2) -> string s1 + " -> epsilon " + string s2 + "\n"

let transitionsToString (s1 : State) (ts : Set<Transition> * bool) =
    let res = Set.fold (fun acc t -> acc + transitionToString s1 t) "" (fst ts)
    if snd ts = true then
        res + "State " + string s1 + " is accepting\n\n"
    else
        res
    
let rec ppNFA (nfa : NFA) : string =
    let (start, map, alphabet) = nfa
    ("Starting state: " + string start + "\n"
    + Map.fold (fun acc key value -> acc + transitionsToString key value) "" map
    + "\nAlphabet: " + ppClassContent alphabet
    )

let dfaTransitionsToString (s1 : State) (ts : Map<char, State> * bool) =
    let res = Map.fold (fun acc symbol s2 -> acc + string s1 + " -> " + string symbol + " " + string s2 + "\n") "" (fst ts)
    if snd ts = true then
        res + "State " + string s1 + " is accepting\n\n"
    else
        res

let rec ppDFA (dfa : DFA) : string =
    let (start, map, alphabet) = dfa
    ("Starting state: " + string start + "\n"
    + Map.fold (fun acc key value -> acc + dfaTransitionsToString key value) "" map
    + "Alphabet: " + ppClassContent alphabet
    )