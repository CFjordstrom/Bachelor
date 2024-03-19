module PrettyPrinter

open AbSyn

let ppChar (c : char) : string =
    match c with
    | '/' | '|' | '*' | '+' | '?' | '{' | '}' | '(' | ')' | '\\' | '[' | ']' | '-' | '.' -> "\\" + string c
    | _ -> string c

let rec splitIntoConsecutiveChars (acc : (char * char) list) (current : char list) (remaining : char list) : (char * char) list =
    match remaining with
    | [] -> if current <> [] then (List.head current, List.last current) :: acc else acc
    | c :: cs ->
        if List.length current > 0 && int c - int (List.last current) = 1 then
            splitIntoConsecutiveChars acc (current @ [c]) cs
        else
            splitIntoConsecutiveChars (if current <> [] then (List.head current, List.last current) :: acc else acc) [c] cs

let ppChars (content : ClassContent) : string =
    let chars = Set.toList content
    let split = splitIntoConsecutiveChars [] [] chars
    List.fold (fun acc (c1, c2) ->
        match int c2 - int c1 with
        | 0 -> acc + ppChar c1
        | 1 -> acc + ppChar c1 + ppChar c2
        | _ -> acc + ppChar c1 + "-" + ppChar c2
    ) "" split

let ppClass (c : Class) : string =
    match c with
    | ClassContent content ->
        match Set.count content with
        | 0 -> "[]"
        | 1 -> ppChars content
        | _ -> "[" + ppChars content + "]"
    | Complement content ->
        match Set.count content with
        | 0 -> "."
        | _ -> "[^" + ppChars content + "]"

let rec ppRegex (regex : Regex) : string =
    match regex with
    | Union (r, seq) -> ppRegex r + "|" + ppSeq seq
    | s -> ppSeq s

and ppSeq (seq : Regex) : string =
    match seq with
    | Seq(rep, Epsilon) -> ppRep rep
    | Seq(rep, seq) -> ppRep rep + ppSeq seq
    | _ -> "\u03B5" //epsilon

and ppRep (rep : Regex) : string =
    match rep with
    | ZeroOrMore atom -> ppAtom atom + "*"
    | Seq(atom1, Seq(ZeroOrMore(atom2), seq)) when atom1 = atom2 -> ppAtom atom1 + "+" + ppSeq seq
    | Seq(Union(atom, Epsilon), seq) -> ppAtom atom + "?" + ppSeq seq
    | atom -> ppAtom atom

and ppAtom (atom : Regex) : string  =
    match atom with
    | Class c -> ppClass c
    | r -> "(" + ppRegex r + ")"

let transitionToString (s1: State) (t : Transition) =
    match t with
    | (Some c, s2) -> string s1 + " -> " + string c + " " + string s2 + "\n"
    | (None, s2) -> string s1 + " -> epsilon " + string s2 + "\n"

let transitionsToString (s1 : State) (ts : Set<Transition> * bool) =
    let res = Set.fold (fun acc t -> acc + transitionToString s1 t) "" (fst ts)
    if snd ts then
        res + "State " + string s1 + " is accepting\n\n"
    else
        res

let rec ppNFA (nfa : NFA) : string =
    let (start, map, alphabet) = nfa
    ("Starting state: " + string start + "\n"
    + Map.fold (fun acc key value -> acc + transitionsToString key value) "" map
    + "\nAlphabet: " + ppChars alphabet
    )

let dfaTransitionsToString (s1 : State) (ts : Map<char, State> * bool) : string =
    let res = Map.fold (fun acc symbol s2 -> acc + string s1 + " -> " + string symbol + " " + string s2 + "\n") "" (fst ts)
    if snd ts then
        res + "State " + string s1 + " is accepting\n\n"
    else
        res

let rec ppDFA (dfa : DFA) : string =
    let (start, map, alphabet) = dfa
    ("Starting state: " + string start + "\n"
    + Map.fold (fun acc key value -> acc + dfaTransitionsToString key value) "" map
    + "Alphabet: " + ppChars alphabet
    )