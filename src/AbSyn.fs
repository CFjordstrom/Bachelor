module AbSyn
(*

Start -> / Regex /

Regex  -> Seq
        | Regex '|' Regex

Seq    -> Epsilon
        | Rep Seq

Rep    -> Atom
        | Atom *
        | Atom +
        | Atom ?

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


pp:
Regex -> Seq | Regex '|' Seq
Seq -> epsilon | Rep Seq
Rep -> Atom | Atom * | Atom + | Atom ?
Atom -> Char | ( Regex ) | Class
Char ->
Class -> . | [ ClassContent ] | [ ^ ClassContent ]
ClassRange ->

*)

type ClassContent = Set<char>

type Class =
    ClassContent of ClassContent
  | Complement of ClassContent

//type Concat = Regex list

type Regex =
    Union of Regex * Regex
  | Seq of Regex * Regex
  | Class of Class
  | ZeroOrMore of Regex
  | Epsilon

type State = int
type Transition = char option * State
type NFA = State * Map<State, (Set<Transition> * bool)> * Set<char>

//type DFATransition = State * char * State
type DFA = State * Map<State, (Map<char, State> * bool)> * Set<char>

(* dfa state maps to a set of nfa states, a bool indicating if the dfa state is marked or not, and the transitions that belong to that dfa state *)
type WorkList = Map<State, (Set<State> * bool)>

//type DFARegexTransitions = State * Map<State, (Map<Regex, State> * bool)> * Set<char>
type GNFA = Regex option[,]