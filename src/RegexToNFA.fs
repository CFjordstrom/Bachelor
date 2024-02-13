module RegexToNFA

open AbSyn

let mutable counter = 0
let nextID () =
    counter <- counter + 1
    (counter - 1)

let emptyNFA =
    {
        states = Set.empty;
        transitions = Set.empty;
        start = 0;
        accepting = Set.empty;
        alphabet = Set.empty;
    }

let rec getLast lst =
    match lst with
    | [] -> None
    | [x] -> Some x
    | x :: xs -> getLast xs

// takes a list of NFA's and returns a set of epsilon transitions between the accepting states of NFA to the starting state of the following NFA
let rec createTransitionsFromAcceptingStatesToStart (nfas : NFA list) : Transition Set =
    match nfas with
    | [] -> Set.empty // if list is empty return empty set of transitions
    | [nfa] -> nfa.transitions // one NFA -> don't need to add transitions
    | (nfa1 :: nfa2 :: rest) ->
        // for each accepting state create an epsilon transition to the starting state of the following state
        let epsilonTransitions = Set.map (fun s -> (s, None, nfa2.start)) nfa1.accepting
        // return union of epsilon transitions from the current 2 states and the epsilon transitions from the next states
        Set.union epsilonTransitions (createTransitionsFromAcceptingStatesToStart (nfa2 :: rest))

let rec classContentToCharList content =
    match content with
    | [] -> []
    | ((c1, c2) :: cs) -> [c1..c2] @ (classContentToCharList cs)

let rec regexToNFA(regex : Regex) : NFA =
    match regex with
    | Epsilon -> emptyNFA
    | Union (r1, r2) ->
        let s = regexToNFA r1
        let t = regexToNFA r2
        let startingState = nextID()
        let acceptingState = nextID()
        {   // states = states of s and t, new starting state and new accepting state
            states = Set.union s.states t.states |> Set.add startingState |> Set.add acceptingState;
            // transitions = transitions in s and t, new starting state to starting state of s and t, all accepting states of s and t to new accepting state
            transitions = Set.union s.transitions t.transitions
                |> Set.add (startingState, None, s.start)
                |> Set.add (startingState, None, t.start)
                |> Set.union (Set.map (fun state -> (state, None, acceptingState)) s.accepting)
                |> Set.union (Set.map (fun state -> (state, None, acceptingState)) t.accepting);
            start = startingState;
            accepting = Set.singleton acceptingState;
            alphabet = Set.union s.alphabet t.alphabet;
        }

    | Concat regexList ->
        match regexList with
        | [] -> emptyNFA
        | rs ->
            let NFAs = List.map regexToNFA rs // list of all the individual NFAs that are to be concatenated
            {   // states = all states in the list of NFAs
                states = List.fold (fun acc nfa -> Set.union acc nfa.states) Set.empty NFAs;
                // transitions = all transitions in the list of NFAs + epsilon transitions between all the accepting states of an NFA to the starting state of the following NFA
                transitions = List.fold (fun acc nfa -> Set.union acc nfa.transitions) Set.empty NFAs
                              |> Set.union (createTransitionsFromAcceptingStatesToStart NFAs)
                // the starting state is the starting state of the first NFA in the list
                start = NFAs.[0].start;
                // the accepting states are the accepting states of the last NFA in the list
                accepting = 
                    match getLast NFAs with
                    | Some nfa -> nfa.accepting
                    | None -> Set.empty;
                // alphabet = union of all the alphabets in the list of NFAs
                alphabet = List.fold (fun acc nfa -> Set.union acc nfa.alphabet) Set.empty NFAs;
            }
    
    | Class c ->
        match c with
        | ClassContent content ->
            let startingState = nextID()
            let acceptingState = nextID()
            let chars = classContentToCharList content
            {
                states = Set.empty |> Set.add startingState |> Set.add acceptingState;
                transitions = List.map (fun c -> Transition(startingState, Some c, acceptingState)) chars |> Set.ofList;
                start = startingState;
                accepting = Set.singleton acceptingState;
                alphabet = Set.ofList chars;
            }
        //| Complement content -> // not sure how to implement

    | ZeroOrMore r ->
        let s = regexToNFA r
        let startingAcceptingstate = nextID()
        {
            states = s.states |> Set.add startingAcceptingstate;
            transitions = s.transitions
                          |> Set.add (startingAcceptingstate, None, s.start)
                          |> Set.union (Set.map (fun state -> (state, None, startingAcceptingstate)) s.accepting);
            start = startingAcceptingstate;
            accepting = Set.singleton startingAcceptingstate;
            alphabet = s.alphabet;
        }

    | Char c ->
        let startingState = nextID()
        let acceptingState = nextID()
        {
            states = Set.empty |> Set.add startingState |> Set.add acceptingState;
            transitions = Set.singleton (startingState, Some c, acceptingState);
            start = startingState;
            accepting = Set.singleton acceptingState;
            alphabet = Set.singleton c;
        }