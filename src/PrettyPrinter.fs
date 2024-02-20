module PrettyPrinter

open AbSyn

let ppChar (c : char) : string =
    match c with 
    | c when c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' -> string c
    | '/' | '|' | '*' | '+' | '?' | '{' | '}' | '(' | ')' | '\\' | '[' | ']' | '-' | '.' -> "\\" + string c
    | _ -> string c

let rec ppClassContent (content : Set<char>) : string =
    Set.fold (fun acc c -> acc + string c) "" content

let ppClass (c : Class) : string =
    match c with
    | ClassContent content -> "[" + ppClassContent content + "]"
    | Complement content -> 
        if Set.isEmpty content then 
            "." 
        else 
            "[^" + ppClassContent content + "]"

let rec ppConcat (lst : Regex list) : string =
    match lst with
    | [] -> ""
    | r :: rs -> ppRegex r + ppConcat rs

and ppRegex (regex : Regex) : string =
    match regex with
    | Union (r1, r2) -> 
        match r2 with
        | Concat([]) -> "(" + ppRegex r1 + ")?"
        | _ -> "(" + ppRegex r1 + ")" + "|" + "(" + ppRegex r2 + ")"
    | Concat lst -> ppConcat lst
    | Class c -> ppClass c
    | Char c -> ppChar c
    | ZeroOrMore r -> "(" + ppRegex r + ")*"

let transitionToString (t : Transition) =
    match t with
    | (s1, Some c, s2) -> string s1 + " -> " + string c + " " + string s2 + "\n"
    | (s1, None, s2) -> string s1 + " -> epsilon " + string s2 + "\n"

let transitionsToString (state : State) (ts : Set<Transition> * bool) =
    let res = Set.fold (fun acc t -> acc + transitionToString t) "" (fst ts)
    if snd ts = true then
        res + "State " + string state + " is accepting\n\n"
    else
        res
    
let rec ppNFA (nfa : NFA) : string =
    let (start, map, alphabet) = nfa
    ("Starting state: " + string start + "\n"
    + Map.fold (fun acc key value -> acc + transitionsToString key value) "" map
    + "\nAlphabet: " + Set.fold (fun acc c -> acc + string c) "" alphabet
    )


(* need to combine nfa and dfa printing, but no time for now *)
let dfaTransitionToString (t : DFATransition) =
    let (s1, c, s2) = t
    string s1 + " -> " + string c + " " + string s2 + "\n"

let dfaTransitionsToString (state : State) (ts : Set<DFATransition> * bool) =
    let res = Set.fold (fun acc t -> acc + dfaTransitionToString t) "" (fst ts)
    if snd ts = true then
        res + "State " + string state + " is accepting\n\n"
    else
        res

let rec ppDFA (dfa : DFA) : string =
    let (start, map, alphabet) = dfa
    ("Starting state: " + string start + "\n"
    + Map.fold (fun acc key value -> acc + dfaTransitionsToString key value) "" map
    + "Alphabet: " + Set.fold (fun acc c -> acc + string c) "" alphabet
    )