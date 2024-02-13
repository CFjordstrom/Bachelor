module AbSyn
(*

Start -> / Regex /

Regex  -> Concat
        | Regex '|' Regex

Concat -> epsilon
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

ClassContent -> epsilon
              | ClassRange ClassContent

ClassRange  -> any char
            | any char - any char
*)

type ClassRange = char * char

type ClassContent = ClassRange list

type Class =
    ClassContent of ClassContent
  | Complement of ClassContent

type Char = char
  //  CharLit of char
  //| EscChar of char

//type Atom = 
//    Char of Char
  //| GroupRegex of Regex
//  | Class of Class

//type Rep =
    //CharAtom of Atom
  //| ZeroOrMore of Atom
  //| OneOrMore of Atom
  //| ZeroOrOne of Atom
  //| NumReps of Atom * int 

type Concat = Regex list

and Regex =
    Epsilon
  | Union of Regex * Regex
  | Concat of Concat
  | Class of Class
  | Char of Char
  | ZeroOrMore of Regex

(*
// id, accepting
type State = 
    Node of int * Transition list * bool
  | End of int * bool

// symbol(none for epsilon), dest
and Transition = char option * State

// states(first is starting)
type NFA = State list
*)

type State = int

// origin, symbol(None for epsilon), destination
type Transition = State * char option * State

// states, transitions, start, accepting states, alphabet
type NFA = {
    states: State Set;
    transitions: Transition Set;
    start: State;
    accepting: State Set;
    alphabet: char Set;
}