module NFAToDFA

open AbSyn

let mutable counter = 0
let nextID () =
    counter <- counter + 1
    (counter - 1)

(* find the set of states in the given nfa that are reachable through epsilon transitions starting at the given state *)
let rec epsilonClosure (state : State) (nfa : NFA) : Set<State> =
    let (start, map, alphabet) = nfa

    (* get the transitions for the given state *)
    match Map.tryFind state map with
    | Some (ts, _) ->
        (* filter to get the epsilon transitions *)
        ts |> Set.filter (fun (_, symbol, _) -> symbol = None)
        (* map to get the destination of the epsilon transitions *)
        |> Set.map (fun (_, _, dest) -> dest)
        (* find the epsilon closures of states that can be reached from epsilon transitions *)
        |> Set.map (fun s -> epsilonClosure s nfa)
        (* combine all the states that can be reach into one set of states *)
        |> Set.fold (fun acc set -> Set.union acc set) Set.empty
        (* add the state itself *)
        |> Set.add state

    | None -> Set.empty (* maybe raise an error? *)

(* takes a set of states and an nfa and returns whether one of the states is an accepting state in the NFA *)
let isAccepting (states : Set<State>) (nfa : NFA) : bool =
    let (start, map, alphabet) = nfa
    states |> Set.exists (fun s -> match Map.tryFind s map with
                                   | Some (_, accepting) -> accepting
                                   | None -> false) (* should maybe be an error *)

(* given a set of states, a symbol and an nfa, find the set of states reachable from the set of input states on the input symbol *)
let findReachableStates (states : Set<State>) (symbol : char) (nfa : NFA) : Set<State> =
    let (start, map, alphabet) = nfa
    (* for every input state *)
    Set.fold (fun acc state ->
        match Map.tryFind state map with
        | Some (ts, _) ->
            (* find transitions on the given symbol *)
            ts |> Set.filter (fun (_, sym, _) ->
                match sym with
                | Some c -> c = symbol
                | None -> false)
            (* extract destinations of transitions *)
            |> Set.map (fun (_, _, s) -> s)
            (* combine with other reachable states *)
            |> Set.union acc
        | None -> Set.empty (* maybe error? *) 
    ) Set.empty states

let generateDFAState (states : Set<State>) (nfa : NFA) : Set<State> =
    Set.fold (fun acc state -> epsilonClosure state nfa |> Set.union acc) Set.empty states

let move (states : Set<State>) (symbol : char) (nfa : NFA) : Set<State> =
    let reachable = findReachableStates states symbol nfa 
    generateDFAState reachable nfa

let rec constructSubset (workList : WorkList) (nfa : NFA) (transitionMap : Map<State, Set<DFATransition>>): (WorkList * Map<State, Set<DFATransition>>) =
(*
    1. while there are unmarked states in the worklist
    2. pick an unmarked element from the worklist and mark it
    3. for each symbol in the alphabet:
        3.1 find the epsilon closure of the states reachable from the current symbol
        3.2 add the epsilon closure to the work list if it is not already there
        3.3 add a transition in the dfa from the current state to the epsilon closure on the current symbol
*)
    (* 1. stop if no more unmarked states *)
    if Map.forall (fun _ (_, mark) -> mark = true) workList then
        (workList, transitionMap)
    else
        let (nfaStart, nfaMap, alphabet) = nfa
        (* 2. pick an unmarked element from the worklist *)
        let (currentDFAState, (currentNFAStates, currentMark)) = 
            workList |> Map.filter (fun _ (_, mark) -> mark = false) |> Map.minKeyValue
        (* 2. mark it *)
        let markedWorkList = 
            Map.change currentDFAState (fun x -> 
                match x with
                | Some (s, m) -> Some (s, true)
                | None -> None) workList

        (* 3.1 for each symbol, find the epsilon closures of the states reachable by transitions on each symbol *)
        (* current implementation of step 3.1 is wrong *)
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
            match Map.tryFindKey (fun _ (nfaSs, mark) -> nfaStates = nfaSs) updatedWorkList with
            | Some dfaS -> Set.union acc (Set.ofList [(currentDFAState, symbol, dfaS)])
            | None -> acc (* error? *)) Set.empty moveAllSymbols
        
        (* create new map from the current dfa state and its transitions *)
        let newMap = Map.add currentDFAState newTransitions transitionMap

        constructSubset updatedWorkList nfa newMap

let generateDFAMapFromTransitionMap (transitionMap : Map<State, Set<DFATransition>>) (workList : WorkList) (nfa : NFA) : Map<State, (Set<DFATransition> * bool)> =
    (* check if the state is accepting *)
    Map.map (fun dfaState transitions ->
        let accepting =
            match Map.tryFind dfaState workList with
            | Some (nfaStates, _) -> isAccepting nfaStates nfa
            | None -> false (* maybe error? *)
        (transitions, accepting)
    ) transitionMap

let nfaToDFA (nfa : NFA) : DFA =
    let (start, map, alphabet) = nfa
    (* find the epsilon closure of the starting state *)
    let s0 = epsilonClosure start nfa
    let startingState = nextID()
    
    (* initialize the worklist to contain the starting state *)
    let workList = Map.ofList [(startingState, (s0, false))]
    (* use the subset construction algorithm to find  *)
    let (fullWorkList, dfaTransitionMap) = constructSubset workList nfa Map.empty

    (startingState, 
    generateDFAMapFromTransitionMap dfaTransitionMap fullWorkList nfa, 
    alphabet)