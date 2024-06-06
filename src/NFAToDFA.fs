module NFAToDFA

open AbSyn

let mutable counter = 1
let nextID () =
    counter <- counter + 1
    (counter - 1)

(* find the set of states in the given nfa that are reachable through epsilon transitions starting at the given state *)
let epsilonClosure (state : State) (nfa : NFA) : Set<State> =
    let (nfaStart, nfaMap, alphabet) = nfa
    let rec ecHelper s visited =
        (* get the transitions for the given state *)
        let transitions = fst <| Map.find s nfaMap
        (* filter to get the epsilon transitions that are not to itself *)
        let epsilonTransitions = Set.filter (fun (symbol, dest) -> symbol = None && Set.contains dest visited = false) transitions
        (* map to get the destination of the epsilon transitions and calculate the epsilon closure of them*)
        let destinationsOfEpsilonTransitions = Set.map (fun (_, dest) -> ecHelper dest (Set.add s visited)) epsilonTransitions
        (* combine all the states that can be reached into one set of states *)
        let combinedDestinations = Set.fold (fun acc set -> Set.union acc set) Set.empty destinationsOfEpsilonTransitions
        (* add the state itself *)
        Set.add s combinedDestinations
    ecHelper state Set.empty

(* takes a set of states and returns a set containing the epsilon closures of those states *)
let epsilonClosures (states : Set<State>) (nfa : NFA) : Set<State> =
    Set.fold (fun acc state -> Set.union acc (epsilonClosure state nfa)) Set.empty states

(* takes a set of states and an nfa and returns whether one of the states is an accepting state in the NFA *)
let isAccepting (states : Set<State>) (nfa : NFA) : bool =
    let (nfaStart, nfaMap, alphabet) = nfa
    Set.exists (fun s -> snd (Map.find s nfaMap)) states

(* given a set of states, a symbol and an nfa, find the set of states reachable from the set of input states on the input symbol *)
let findReachableStatesFromTransitionOnSymbol (states : Set<State>) (symbol : char) (nfa : NFA) : Set<State> =
    let (nfaStart, nfaMap, alphabet) = nfa
    
    Set.fold (fun acc state ->
        let transitions = fst <| Map.find state nfaMap
        let transitionsOnSymbol = 
            Set.filter (fun (sym, _) ->
                match sym with
                | Some c -> c = symbol
                | None -> false) transitions
        let destinationsOnSymbol = Set.map (fun (_, s) -> s) transitionsOnSymbol
        Set.union acc destinationsOnSymbol) Set.empty states

(* takes a set of states, a symbol and an nfa and returns the epsilon closures of
   the set of states reachable from transitions on that symbol *)
let move (states : Set<State>) (symbol : char) (nfa : NFA) : Set<State> =
    let statesReachableFromTransitionOnSymbol = findReachableStatesFromTransitionOnSymbol states symbol nfa 
    epsilonClosures statesReachableFromTransitionOnSymbol nfa

let rec constructSubset (workList : WorkList) (nfa : NFA) (transitionMap : Map<State, Map<char, State>>) : (WorkList * Map<State, Map<char, State>>) =
    (*
    1. while there are unmarked states in the worklist
    2. pick an unmarked element from the worklist and mark it
    3. for each symbol in the alphabet:
        3.1 find the epsilon closure of the states reachable from the current symbol
        3.2 add the epsilon closure to the work list if it is not already there
        3.3 add a transition in the dfa from the current state to the epsilon closure on the current symbol
    *)
    (* 1. stop if no more unmarked states *)
    if Map.forall (fun _ (_, mark) -> mark) workList then
        (workList, transitionMap)
    else
        let (nfaStart, nfaMap, alphabet) = nfa
        (* 2. pick an unmarked element from the worklist *)
        let (currentDFAState, (currentNFAStates, currentMark)) = 
            Map.minKeyValue <| Map.filter (fun _ (_, mark) -> mark = false) workList
        
        (* 2. mark it *)
        let markedWorkList = 
            Map.change currentDFAState (fun x -> 
                match x with
                | Some (s, m) -> Some (s, true)
                | None -> None) workList

        (* 3.1 for each symbol, find the epsilon closures of the states reachable by transitions on each symbol *)
        let moveAllSymbols : Set<(Set<State> * char)> = 
            Set.fold (fun acc symbol ->
                let res = move currentNFAStates symbol nfa
                if Set.isEmpty res then
                    acc
                else
                    Set.add (res, symbol) acc) Set.empty alphabet
        
        (* 3.2 add to worklist if not already there *)
        let updatedWorkList = Set.fold (fun wl moves ->
            let (nfaStates, c) = moves
            match Map.exists (fun _ (nfaSs, mark) -> nfaStates = nfaSs) wl with
            | true -> wl
            | false -> Map.add (nextID()) (nfaStates, false) wl) markedWorkList moveAllSymbols

        (* 3.3 Add transitions from current state *)
        let newTransitions = Set.fold (fun acc moves ->
            let (nfaStates, symbol) = moves
            let dfaState = Map.findKey (fun _ (nfaSs, _) -> nfaStates = nfaSs) updatedWorkList
            Map.add symbol dfaState acc) Map.empty moveAllSymbols

        (* create new map from the current dfa state and its transitions *)
        let newMap = Map.add currentDFAState newTransitions transitionMap

        constructSubset updatedWorkList nfa newMap

let addAcceptingOrRejecting (transitionMap : Map<State, (Map<char, State>)>) (workList : WorkList) (nfa : NFA) : Map<State, (Map<char, State> * bool)> =
    Map.map (fun dfaState transitions ->
        let nfaStates = fst <| Map.find dfaState workList
        let accepting = isAccepting nfaStates nfa
        (transitions, accepting)) transitionMap

let nfaToDFA (nfa : NFA) : DFA<State> =
    let (start, map, alphabet) = nfa

    (* find the epsilon closure of the starting state *)
    let s0 = epsilonClosure start nfa
    let startingState = nextID()
    
    (* initialize the worklist to contain the starting state *)
    let workList = Map.ofList [(startingState, (s0, false))]
    (* use the subset construction algorithm to find which DFA states correspond to which NFA states
       and to find the DFA transitions *)
    let (fullWorkList, dfaTransitionMap) = constructSubset workList nfa Map.empty

    (startingState, 
    addAcceptingOrRejecting dfaTransitionMap fullWorkList nfa, 
    alphabet)

let renumberAndConvertNFAToDFA (nfa : NFA) : DFA<State> =
    let (start, map, alphabet) = nfa

    (* set counter to 1 for renumbering - don't like doing this, but don't know what else to do *)
    counter <- 1
    (* find the epsilon closure of the starting state *)
    let s0 = epsilonClosure start nfa
    let startingState = nextID()
    
    (* initialize the worklist to contain the starting state *)
    let workList = Map.ofList [(startingState, (s0, false))]
    (* use the subset construction algorithm to find which DFA states correspond to which NFA states
       and to find the DFA transitions *)
    let (fullWorkList, dfaTransitionMap) = constructSubset workList nfa Map.empty

    (startingState, 
    addAcceptingOrRejecting dfaTransitionMap fullWorkList nfa, 
    alphabet)