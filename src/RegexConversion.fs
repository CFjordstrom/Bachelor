module RegexConversion

open AbSyn

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

and AtomToString (atom : Atom) : string =
    match atom with
    | CharLit c -> string c
    | GroupRegex (regex) -> "(" + RegexToString regex + ")"