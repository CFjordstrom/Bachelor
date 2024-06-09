module AbSyn
(*

Start    -> Automata Regex

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

ClassContent -> Epsilon
              | ClassRange ClassContent

ClassRange  -> any char
            | any char '-' any char

Automata    -> Epsilon
             | { Grammar }

Grannar    -> Nonterminal '->' Regex ';' Grammar
            | Epsilon

Nonterminal   -> '#' *any alphanumerical character*

*)

exception MyError of string

type ClassContent = Set<char>

type Class =
    ClassContent of ClassContent
  | Complement of ClassContent

type State = int
type Transition = char option * State
type NFAMap = Map<State, (Set<Transition> * bool)>
type Alphabet = Set<char>
type NFA = State * NFAMap * Alphabet

type DFA<'State when 'State : comparison> = 'State * Map<'State, (Map<char, 'State> * bool)> * Set<char>

(* dfa state maps to a set of nfa states, a bool indicating if the dfa state is marked or not, and the transitions that belong to that dfa state *)
type WorkList = Map<State, (Set<State> * bool)>

and Regex =
    Union of Regex * Regex
  | Seq of Regex * Regex
  | Class of Class
  | ZeroOrMore of Regex
  | Nonterminal of string
  | REComplement of Regex
  | Intersection of Regex * Regex
  | Epsilon

type GNFA = Regex option[,]

type Layers = (string list) list

(* map from NT to starting state and the layer's map *)
type NTStart = Map<string, State>
type Templates = Map<NFAMap, string list>

type Grammar = (string * Regex) list

type NTInfo = Grammar * NTStart * Templates * Layers

(* given NT and a map from NT to start state and map from NFA to lists of NT's, return starting state and NFA map *)
let findNTAssociations (nt : string) (ntStart : NTStart) (templates : Templates) : (State * NFAMap) option = 
    match Map.tryFind nt ntStart, Map.tryFindKey (fun map nts -> List.contains nt nts) templates with
    | Some start, Some nfaMap -> Some (start, nfaMap)
    | _ -> None

type RegLang = Grammar * Regex