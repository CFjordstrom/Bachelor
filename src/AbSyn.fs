module AbSyn
(*

Start    -> '/' RegLang '/'

RegLang  -> Seq
        | Regex '|' Regex

Seq    -> Epsilon
        | Rep Seq

Rep    -> Atom
        | Atom '*'
        | Atom '+'
        | Atom '?'

Atom   -> Char
        | '(' Regex ')'
        | Class
        | '{' NFA '}'

Char   -> char not in {/|*+?{}()\[]-.}
        | '\' char in {/|*+?{}()\[]-.}

Class  -> '.'
        | '[' ClassContent ']'
        | '[' '^' ClassContent ']'

ClassContent -> epsilon
              | ClassRange ClassContent

ClassRange  -> any char
            | any char '-' any char

NFA    -> epsilon
        | '#' State '->' Regex '#' State ';' NFA
        | '#' State '->' Regex ';' NFA
        | '#' State '->' ';' NFA

State   -> *any alphanumerical chars*

*)

type ClassContent = Set<char>

type Class =
    ClassContent of ClassContent
  | Complement of ClassContent

//type Concat = Regex list

(*
type Regex =
    Union of Regex * Regex
  | Seq of Regex * Regex
  | Class of Class
  | ZeroOrMore of Regex
  | Epsilon
*)
type State = int
type Transition = char option * State
type NFAMap = Map<State, (Set<Transition> * bool)>
type Alphabet = Set<char>
type NFA = State * NFAMap * Alphabet

type DFA = State * Map<State, (Map<char, State> * bool)> * Set<char>

(* dfa state maps to a set of nfa states, a bool indicating if the dfa state is marked or not, and the transitions that belong to that dfa state *)
type WorkList = Map<State, (Set<State> * bool)>

//type DFARegexTransitions = State * Map<State, (Map<Regex, State> * bool)> * Set<char>

type Transitions = (string * ExtendedRegex * string option) list

and ExtendedRegex =
    Union of ExtendedRegex * ExtendedRegex
  | Seq of ExtendedRegex * ExtendedRegex
  | Class of Class
  | ZeroOrMore of ExtendedRegex
  | Transitions of Transitions
  | Epsilon

type GNFA = ExtendedRegex option[,]

type StateMap = Map<string, State>