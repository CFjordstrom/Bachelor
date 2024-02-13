module PrettyPrinter

open AbSyn

(*let ClassRangeToString (range : ClassRange) : string =
    match range with
    | RangeChar (c) -> string c
    | Range (c1, c2) -> string c1 + "-" + string c2

let rec ClassContentToString (content : ClassRange list) : string =
    match content with
    | [] -> ""
    | content :: contents -> ClassRangeToString content + ClassContentToString contents

let ClassToString (classVal : Class) : string =
    match classVal with
    | Dot -> "."
    | ClassContent (content) -> "[" + ClassContentToString content + "]"
    | Complement (content) -> "[^" + ClassContentToString content + "]" 

let CharToString (c : Char) : string =
    match c with
    | CharLit (c) -> string c
    | EscChar (c) -> "\\" + string c

let rec RegexToString (regex : Regex) : string =
    match regex with
    | Concat (lst)  -> ConcatToString lst
    | Union (regex1, regex2) -> RegexToString regex1 + "|" + RegexToString regex2

and ConcatToString (lst : Rep list) : string =
    match lst with
    | [] -> ""
    | rep :: reps -> RepToString rep + ConcatToString reps

and RepToString (rep : Rep) : string =
    match rep with
    | CharAtom (atom) -> AtomToString atom
    | ZeroOrMore (atom) -> AtomToString atom + "*"
    | OneOrMore (atom) -> AtomToString atom + "+"
    | ZeroOrOne (atom) -> AtomToString atom + "?"
    //| NumReps (atom, numreps) -> AtomToString atom + "{" + string numreps + "}"

and AtomToString (atom : Atom) : string =
    match atom with
    | Char (c) -> CharToString c
    | GroupRegex (regex) -> "(" + RegexToString regex + ")"
    | Class (classVal) -> ClassToString classVal*)

let ppClassRange (range : char * char) : string =
    if fst range = snd range then
        string range
    else
        string (fst range) + "-" + string (snd range)

let rec ppClassContent (content : ClassRange list) : string =
    match content with
    | [] -> ""
    | c :: cs -> ppClassRange c + ppClassContent cs

let ppClass (c : Class) : string =
    match c with
    | ClassContent content -> "[" + ppClassContent content + "]"
    | Complement content -> 
        if List.isEmpty content then 
            "." 
        else 
            "[^" + ppClassContent content + "]"

let ppChar (c : char) : string =
    match c with 
    | c when c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' -> string c
    | '/' | '|' | '*' | '+' | '?' | '{' | '}' | '(' | ')' | '\\' | '[' | ']' | '-' | '.' -> "\\" + string c
    | _ -> string c

let rec ppConcat (lst : Regex list) : string =
    match lst with
    | [] -> ""
    | r :: rs -> ppRegex r + ppConcat rs

and ppRegex (regex : Regex) : string =
    match regex with
    | Epsilon -> ""
    | Union (r1, r2) -> 
        match r2 with
        | Epsilon -> "(" + ppRegex r1 + ")?"
        | _ -> ppRegex r1 + "|" + ppRegex r2
    | Concat lst -> ppConcat lst
    | Class c -> ppClass c
    | Char c -> ppChar c
    | ZeroOrMore r -> "(" + ppRegex r + ")*"

let acceptingBoolToString a =
    match a with
        | true -> ", accepting"
        | false -> ""

let ppTransitionWithID id1 s id2 a =
    let accepting = acceptingBoolToString a
    string id1 + " -> " + s + " " + string id2 + accepting + "\n"

(*
let rec concat lst =
    match lst with
    | [] -> ""
    | (s :: ss) -> s + concat ss

let ppTransition (originID : int) (t : Transition) : string =
    match t with
    | (Some c, s) -> 
        match s with
        | Node(id, _, a) -> ppTransitionWithID originID (string c) id a
        | End(id, a) -> ppTransitionWithID originID (string c) id a
    | (None, s) -> 
        match s with
        | Node(id, _, a) -> ppTransitionWithID originID "epsilon" id a
        | End(id, a) -> ppTransitionWithID originID "epsilon" id a

let ppState (s : State) : string =
    match s with
    | Node(id, ts, a) ->
        match ts with
        | [] -> ""
        | ts -> ts |> List.map (ppTransition id) |> concat
    | End(id, a) -> string id + (acceptingBoolToString a) + "\n"

let rec ppNFA (nfa : NFA) : string =
    match nfa with
    | [] -> ""
    | (s :: ss) -> ppState s + ppNFA ss
*)

let rec ppNFA (nfa : NFA) : string =
    Set.fold (fun acc (origin, s, dest) -> 
        let symbol =
            match s with
            | Some c -> string c
            | None -> "epsilon"
        let res = acc + string origin + " -> " + symbol + " " + string dest
        if Set.contains dest nfa.accepting then
            res + ", accepting\n"
        else
            res + "\n"
    ) "" nfa.transitions

