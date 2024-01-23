module RegexConversion

open AbSyn

let rec AbSynToString (regex : Regex) : string =
    match regex with
    | CharLit (c) -> string c
    | 