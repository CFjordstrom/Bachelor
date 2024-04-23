module AbSyn
(*

Start    -> '/' NFA Regex '/'

Regex  -> Seq
        | Regex '|' Regex
        | Regex '&' Regex

Seq    -> Epsilon
        | Rep Seq

Rep    -> Atom
        | Atom '*'
        | Atom '+'
        | Atom '?'
        | '!' Atom

Atom   -> Char
        | '(' Regex ')'
        | Class
        | Nonterminal

Char   -> char not in {/|*+?{}()\[]-.#;}
        | '\' *any non-alphanumerical character*

Class  -> '.'
        | '[' ClassContent ']'
        | '[' '^' ClassContent ']'

ClassContent -> epsilon
              | ClassRange ClassContent

ClassRange  -> any char
            | any char '-' any char

NFA    -> epsilon
       -> { Transitions }

Transitions    -> Nonterminal '->' Regex ';' NFA
                | epsilon

Nonterminal   -> '#' *any alphanumerical character*

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

type DFA<'State when 'State : comparison> = 'State * Map<'State, (Map<char, 'State> * bool)> * Set<char>

(* dfa state maps to a set of nfa states, a bool indicating if the dfa state is marked or not, and the transitions that belong to that dfa state *)
type WorkList = Map<State, (Set<State> * bool)>

//type DFARegexTransitions = State * Map<State, (Map<Regex, State> * bool)> * Set<char>

type Transitions = (string * ExtendedRegex) list

and ExtendedRegex =
    Union of ExtendedRegex * ExtendedRegex
  | Seq of ExtendedRegex * ExtendedRegex
  | Class of Class
  | ZeroOrMore of ExtendedRegex
  | Nonterminal of string
  | REComplement of ExtendedRegex
  | Intersection of ExtendedRegex * ExtendedRegex
  | Epsilon

type GNFA = ExtendedRegex option[,]

type NTab = Map<string, NFA>

type RegLang = Transitions * ExtendedRegex