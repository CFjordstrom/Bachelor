module PrettyPrinter

open AbSyn

let ppChar (c : char) (isAlphabet : bool) : string =
    match c with
    | '/' | '|' | '*' | '+' | '?' | '{' | '}' | '(' | ')' | '\\' | '[' | ']' | '-' | '.' -> 
        if isAlphabet = true then
            string c
        else
            "\\" + string c
    | _ -> string c

let rec splitIntoConsecutiveChars (acc : string list) (current : string) (remaining : string) : string list =
    match remaining with
    | "" -> if current <> "" then acc @ [current] else acc
    | _ ->
        let head = remaining.[0]
        let tail = remaining.[1..]
        if String.length current > 0 && int head - int current.[(String.length current) - 1] = 1 then
            splitIntoConsecutiveChars acc (current + string head) tail
        else
            splitIntoConsecutiveChars (if current <> "" then acc @ [current] else acc) (string head) tail

let ppChars (content : ClassContent) (isAlphabet : bool): string =
    let chars = Set.fold (fun acc c -> acc + string c) "" content
    let split = splitIntoConsecutiveChars [] "" chars
    List.fold (fun acc s -> 
        if String.length s < 3 then
            acc + String.collect (fun c -> ppChar c isAlphabet) s
        else
            acc + ppChar s.[0] isAlphabet + "-" + ppChar s.[String.length s - 1] isAlphabet
    ) "" split

let ppClass (c : Class) : string =
    match c with
    | ClassContent content ->
        match Set.count content with
        | 0 -> "[]"
        | 1 -> ppChars content false
        | _ -> "[" + ppChars content false + "]"
    | Complement content ->
        match Set.count content with
        | 0 -> "."
        | _ -> "[^" + ppChars content false + "]"

let rec ppRegex (regex : Regex) : string =
    match regex with
    | Union (r, Concat []) -> ppRegex r + "?"
    | Union (r, Concat concat) -> ppRegex r + "|" + ppConcat concat
    | Concat lst -> ppConcat lst
    | r -> ppConcat [r]

and ppConcat (lst : Concat) : string =
    match lst with
    | [] -> ""
    | atom1 :: (ZeroOrMore atom2) :: concat when atom1 = atom2 -> ppRep atom1 + "+" + ppConcat concat
    | (ZeroOrMore atom) :: concat -> ppRep (ZeroOrMore atom) + ppConcat concat
    | rep :: concat -> ppRep rep + ppConcat concat

and ppRep (rep : Regex) : string =
    match rep with
    | ZeroOrMore atom -> ppAtom atom + "*"
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
    if snd ts = true then
        res + "State " + string s1 + " is accepting\n\n"
    else
        res

let rec ppNFA (nfa : NFA) : string =
    let (start, map, alphabet) = nfa
    ("Starting state: " + string start + "\n"
    + Map.fold (fun acc key value -> acc + transitionsToString key value) "" map
    + "\nAlphabet: " + ppChars alphabet true
    )

let dfaTransitionsToString (s1 : State) (ts : Map<char, State> * bool) : string =
    let res = Map.fold (fun acc symbol s2 -> acc + string s1 + " -> " + string symbol + " " + string s2 + "\n") "" (fst ts)
    if snd ts = true then
        res + "State " + string s1 + " is accepting\n\n"
    else
        res

let rec ppDFA (dfa : DFA) : string =
    let (start, map, alphabet) = dfa
    ("Starting state: " + string start + "\n"
    + Map.fold (fun acc key value -> acc + dfaTransitionsToString key value) "" map
    + "Alphabet: " + ppChars alphabet true
    )