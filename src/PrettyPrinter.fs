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
    match regex with
    | Epsilon ->
        "[]"
    | _ -> ppRegex' regex

let ppNFA (nfa : NFA) : string =
    let (start, map, alphabet) = nfa
    "{\n" +
    (* for all states *)
    Map.fold (fun acc fromState (ts, isAccepting) ->
        (* combine all symbols to the same state *)
        let toStateToSymbolsSet =
            Set.fold (fun acc' (symbol, toState) ->
                match Map.tryFind toState acc' with
                | Some symbols ->
                    match symbol with
                    | Some c -> Map.add toState (Set.add c symbols) acc'
                    | None -> acc'
                | None ->
                    match symbol with
                    | Some c -> Map.add toState (Set.singleton c) acc'
                    | None -> Map.add toState Set.empty acc'
            ) Map.empty ts

        (* convert set of chars to string *)
        let toStateToSymbolsString = 
            Map.map (fun toState symbols ->
                match ppClass (ClassContent symbols) with
                | "[]" -> ""
                | str -> str
            ) toStateToSymbolsSet

        (* put all transitions into a list *)
        let transitionList =
            Map.fold (fun acc' toState symbols ->
                match symbols with
                | "" -> ("#" + string toState) :: acc'
                | _ -> (symbols + " #" + string toState) :: acc'
            ) [] toStateToSymbolsString

        (* concat with | as separator *)
        let transition = String.concat " | " transitionList

        match isAccepting, String.length transition with
        | true, 0 -> acc + "#" + string fromState + " -> ();\n"
        | true, _ -> acc + "#" + string fromState + " -> " + transition + " | ();\n"
        | false, 0 -> acc + "#" + string fromState + " -> [];\n"
        | false, _ -> acc + "#" + string fromState + " -> " + transition + ";\n"
    ) "" map
    + "}\n" + "/#" + string start + "/"

let ppDFA (dfa : DFA<State>) : string =
    let (start, map, alphabet) = dfa
    "{\n" + 
    (* for all states *)
    Map.fold (fun acc fromState (charMap, isAccepting) ->
        (* combine all symbols to the same state *)
        let toStateToSymbolsSet = 
            Map.fold (fun acc' symbol toState -> 
                match Map.tryFind toState acc' with
                | Some symbols -> Map.add toState (Set.add symbol symbols) acc'
                | None -> Map.add toState (Set.singleton symbol) acc'
            ) Map.empty charMap

        (* convert set of chars to string *)
        let toStateToSymbolsString = 
            Map.map (fun toState symbols ->
                match ppClass (ClassContent symbols) with
                | "[]" -> ""
                | str -> str
            ) toStateToSymbolsSet

        (* put all transitions into a list *)
        let transitionList =
            Map.fold (fun acc' toState symbols ->
                match symbols with
                | "" -> ("#" + string toState) :: acc'
                | _ -> (symbols + " #" + string toState) :: acc'
            ) [] toStateToSymbolsString

        (* concat with | as separator *)
        let transition = String.concat " | " transitionList

        match isAccepting, String.length transition with
        | true, 0 -> acc + "#" + string fromState + " -> ();\n"
        | true, _ -> acc + "#" + string fromState + " -> " + transition + " | ();\n"
        | false, 0 -> acc + "#" + string fromState + " -> [];\n"
        | false, _ -> acc + "#" + string fromState + " -> " + transition + ";\n"
    ) "" map
    + "}\n" + "/#" + string start + "/"