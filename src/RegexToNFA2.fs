module RegexToNFA

open AbSyn

let mutable counter = 0
let nextID () =
    counter <- counter + 1
    (counter - 1)

(*
let getAcceptingStates (nfa : NFA) : State list =
  List.filter (fun state -> snd state) nfa
*)
let getStartingState (nfa : NFA) : State=
    match nfa with
    | ([]) -> failwith "Cannot get starting state of NFA because it has no states"
    | (s :: ss) -> s

let updateStateWithTransition (t : Transition) (s : State) : State =
    match s with
    | Node(id, transitions, _) -> Node(id, t :: transitions, false)
    | End(id, _) -> Node(id, [t], false)

let updateNFAAcceptingStatesWithTransition (nfa : NFA) (t : Transition) : NFA =
    // partially apply updateStateWithTransition on t to get a function which updates a state with the transition t
    let updateStateWithTransitionToAccepting = updateStateWithTransition t
    // map to get a new nfa where all the old accepting states get a new transition to t
    List.map (fun state -> ( match state with
                                | Node(_, _, true) -> updateStateWithTransitionToAccepting state
                                | End(_, true) -> updateStateWithTransitionToAccepting state
                                | _ -> state)) nfa

let rec regexToNFA (regex : Regex) : NFA =
    match regex with
    | Epsilon -> []
    | Union (r1, r2) ->
        // recursively find NFA for the subexpressions
        let s = regexToNFA r1
        let t = regexToNFA r2

        // define epsilon transitions from new starting state to s and t
        let startToS = (None, getStartingState s)
        let startToT = (None, getStartingState t)

        // create starting state which contains new transitions
        let startingState = Node(nextID(), [startToS; startToT], false)

        // create accepting state and a transition going to it
        let acceptingState = End(nextID(), true)
        let epsilonToAccepting = Transition(None, acceptingState)

        // update s and t such that their accepting states have epsilon transitions to the new accepting states
        let s' = updateNFAAcceptingStatesWithTransition s epsilonToAccepting
        let t' = updateNFAAcceptingStatesWithTransition t epsilonToAccepting

        // return NFA
        [startingState] @ s' @ t' @ [acceptingState]

    | Concat lst ->
        match lst with
        | [] -> [] // if there is nothing to convert then return empty nfa
        | r :: rs ->
            let s = regexToNFA r // convert s to nfa
            let t = regexToNFA (Concat(rs)) // convert t to nfa
            match t with
            | [] -> s // if t is empty then don't have to make a transition from s to t              
            | state :: states ->
                let s' = updateNFAAcceptingStatesWithTransition s (Transition(None, state))
                s' @ t

    | Char c ->
        let startingID = nextID()
        let acceptingState = End(nextID(), true)
        let startToAccept = (Some c, acceptingState)
        let startingState = Node(startingID, [startToAccept], false)
        [startingState; acceptingState]
        
    | _ -> []