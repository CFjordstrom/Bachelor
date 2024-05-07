module PrettyPrinter

open AbSyn

let ppChar (c : char) : string =
    match c with
    | '/' | '|' | '*' | '+' | '?' | '{' | '}' | '(' | ')' | '\\' | '[' | ']' | '-' | '.' | ' ' -> "\\" + string c
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

let rec ppRegex' (regex : ExtendedRegex) : string =
    match regex with
    | Union(r, seq) -> ppRegex' r + "|" + ppSeq seq
    | Intersection(r, seq) -> ppRegex' r + "&" + ppSeq seq
    | s -> ppSeq s

and ppSeq (seq : ExtendedRegex) : string =
    match seq with
    | Epsilon -> ""
    | Seq(rep, seq) -> ppRep rep + ppSeq seq
    | _ -> ppRep seq

and ppRep (rep : ExtendedRegex) : string =
    match rep with
    | ZeroOrMore atom -> ppAtom atom + "*"
    | Seq(atom1, Seq(ZeroOrMore(atom2), Epsilon)) when atom1 = atom2 -> ppAtom atom1 + "+"
    | Union(atom, Epsilon) -> ppAtom atom + "?"
    | REComplement atom -> "!" + ppAtom atom
    | atom -> ppAtom atom

and ppAtom (atom : ExtendedRegex) : string  =
    match atom with
    | Class c -> ppClass c
    | Nonterminal s -> "#" + s
    | r -> "(" + ppRegex' r + ")"

let ppRegex (regex : ExtendedRegex) : string =
    "/" + ppRegex' regex + "/"

(*
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
*)

(*
let dfaTransitionsToString (s1 : State) (ts : Map<char, State> * bool) : string =
    let res = Map.fold (fun acc symbol s2 -> acc + string s1 + " -> " + string symbol + " " + string s2 + "\n") "" (fst ts)
    if snd ts then
        res + "State " + string s1 + " is accepting\n\n"
    else
        res

let ppDFA (dfa : DFA<State>) : string =
    let (start, map, alphabet) = dfa
    ("Starting state: " + string start + "\n"
    + Map.fold (fun acc key value -> acc + dfaTransitionsToString key value) "" map
    + "Alphabet: " + ppChars alphabet
    )
*)

let ppNFA (nfa : NFA) : string =
    let (start, map, alphabet) = nfa
    "{" +
    Map.fold (fun acc fromState (ts, isAccepting) ->
        let transitions =
            Set.fold (fun acc' (symbol, toState) ->
                let symbol' = 
                    match symbol with
                    | Some s -> string s
                    | None -> ""
                acc' + "#" + string fromState + " -> " + symbol' + " #" + string toState + ";\n"
            ) "" ts
        if isAccepting then
            acc + transitions + "#" + string fromState + " -> ;\n"
        else
            acc + transitions
    ) "" map
    + "}\n"
    + "/" + "#" + string start + "/"

let ppDFA (dfa : DFA<State>) : string =
    let (start, map, alphabet) = dfa
    "{" + 
    Map.fold (fun acc fromState (charMap, isAccepting) ->
        let transitions =
            Map.fold (fun acc' symbol toState ->
                acc' + "#" + string fromState + " -> " + string symbol + " #" + string toState + ";\n"
            ) "" charMap
        if isAccepting then
            acc + transitions + "#" + string fromState + " -> ;\n"
        else
            acc + transitions
    ) "" map
    + "}\n"
    + "/" + "#" + string start + "/"