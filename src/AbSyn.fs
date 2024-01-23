module AbSyn
(*
s
st
s|t
s*
s+
s?
[a-z]
[abc]
.

[^abc]
\
s{3}

S -> / Regex /
Regex      -> Seq
            | Regex '|' Regex

Seq         -> Quant
            | Seq Seq

Quant       -> Chars
            | Chars { Number }
            | Chars *
            | Chars +
            | Chars ?

Chars       -> Char
            | Class
            | \
            | ( Regex )

Number      -> any integer

Char        -> any character except |*+?()[]{}.\
            | EscChar

EscChar     -> \ any of |*+?()[]{}.

Class       -> [ ClassItems ]
            | [ ^ ClassItems ]
            | .

ClassItems  -> epsilon
            | ClassItem ClassItems

ClassItem   -> RangeChar
            | RangeChar - RangeChar

RangeChar   -> any character except -, ]

todo: escape, complement

*)

(*
type Position = int * int

type ClassItem =
      SingleChar of char                        // a
    | CharRange of char * char                  // 0-9

type Class =
      ClassItems of ClassItem list              // [a-zA-Z0-9]
    | AnyChar                                   // .

type CharSet =
      CharVal of char                           // s
    | CharClass of Class                        //
    | GroupRegex of Regex                       // ( s )

type Quantifier =
      CharSet of CharSet                        
    | ZeroOrMore of CharSet                     // s*
    | OneOrMore of CharSet                      // s+
    | ZeroOrOne of CharSet                      // s?

type Sequence =
      SingleQuant of Quantifier                 // 
    | Concatenation of Sequence * Sequence      // st

type Regex =
      Seq of Sequence                           //
    | Union of Regex * Regex                    // s|t
*)

(*
Regex  -> Concat
        | Regex '|' Regex

Concat -> Rep
        | Concat Concat

Rep    -> Atom
        | Atom *

Atom   -> any char
        | ( Regex )
*)

type Atom = 
      CharLit of char
    | GroupRegex of Regex

type Rep =
      CharAtom of Atom
    | ZeroOrMore of Atom

type Concat =
    Chars of Rep list

type Regex =
      Concatenation of Concat
    | Union of Regex * Regex