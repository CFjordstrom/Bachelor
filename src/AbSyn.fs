module AbSyn
(*

Start -> / Regex /

Regex  -> Concat
        | Regex '|' Regex

Concat -> Rep
        | Rep Concat

Rep    -> Atom
        | Atom *
        | Atom +
        | Atom ?
        | Atom { num }

Atom   -> Char
        | ( Regex )
        | Class

Char   -> char not in {/|*+?{}()\[]-.}
        | \ char in {/|*+?{}()\[]-.}

Class  -> .
        | [ ClassContent ]
        | [ ^ ClassContent ]

ClassContent -> ClassRange
              | ClassRange ClassContent

ClassRange  -> any char
            | any char - any char
*)

type ClassRange =
    RangeChar of char
  | Range of char * char

type ClassContent = ClassRange list

type Class =
    Dot
  | ClassContent of ClassContent
  | Complement of ClassContent

type Char = 
    CharLit of char
  | EscChar of char

type Atom = 
    Char of Char
  | GroupRegex of Regex
  | Class of Class

and Rep =
    CharAtom of Atom
  | ZeroOrMore of Atom
  | OneOrMore of Atom
  | ZeroOrOne of Atom
  | NumReps of Atom * int 

and Concat = Rep list

and Regex =
    Concat of Concat
  | Union of Regex * Regex