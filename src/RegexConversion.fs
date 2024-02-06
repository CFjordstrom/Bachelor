module RegexConversion

open AbSyn

let ClassRangeToString (range : ClassRange) : string =
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
    | NumReps (atom, numreps) -> AtomToString atom + "{" + string numreps + "}"

and AtomToString (atom : Atom) : string =
    match atom with
    | Char (c) -> CharToString c
    | GroupRegex (regex) -> "(" + RegexToString regex + ")"
    | Class (classVal) -> ClassToString classVal