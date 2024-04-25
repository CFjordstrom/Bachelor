module TransitionsToNFA

open AbSyn
open RegexToNFA
open PrettyPrinter

(* takes two NFAMaps and adds the transitions from the shorter one to the longer one *)
let nfaUnion (map1 : NFAMap) (map2 : NFAMap) : NFAMap =
    let (longer, shorter) =
        if Map.count map1 >= Map.count map2 then (map1, map2) else (map2, map1)
    
    Map.fold (fun acc key (ts, isAccepting) ->
        Map.change key (fun v ->
            match v with
            | Some (ts', isAccepting') -> Some (Set.union ts ts', isAccepting || isAccepting')
            | None -> Some (ts, isAccepting)
        ) acc
    ) longer shorter

(* bind a state represented as a string to a new State*)
let bind (stateStr : string) (symTab : StateMap) : StateMap =
    Map.change stateStr (fun s ->
        match s with
        | Some state -> Some state
        | None -> Some (nextID())
    ) symTab

(* bind a state represented as a string to an existing State *)
let bindInt (stateStr : string) (state : State) (symTab : StateMap) : StateMap =
    Map.change stateStr (fun s ->
        match s with
        | Some existing -> Some existing
        | None -> Some state
    ) symTab

let rec transitionsToNFA' (transitions : Transitions) (symTab : StateMap) (nfaMap : NFAMap) (alphabet : Alphabet) (endState : State): NFAMap * Alphabet * StateMap =
    match transitions with
    (* if no more transitions then stop *)
    | [] -> (nfaMap, alphabet, symTab)
    | (fromStateString, symbol, None) :: ts ->
        (* check if states are already bound, if not bind them *)
        let symTab' = bind fromStateString symTab

        (* create NFA representing the transition regex *)
        let fromInt = Map.find fromStateString symTab'
        let (start, map, alphabet') = regexToNFARec symbol symTab' endState
        let nfaMap' =
            Map.change fromInt (fun x ->
                match x with
                | Some (ts, isAccepting) -> Some (Set.add (None, start) ts, isAccepting)
                | None -> Some (Set.ofList [None, start], false))
            <| nfaUnion nfaMap map

        transitionsToNFA' ts symTab' nfaMap' (Set.union alphabet alphabet') endState

    | (fromStateString, symbol, Some toStateString) :: ts ->
        (* check if states are already bound, if not bind them *)
        let symTabFrom = bind fromStateString symTab
        let symTabTo = bind toStateString symTab
        let symTab' = mapDisjointUnion symTabFrom symTabTo

        (* create NFA representing the transition regex *)
        let fromInt = Map.find fromStateString symTab'
        let toInt = Map.find toStateString symTab'
        let (start, map, alphabet') = regexToNFARec symbol symTab' toInt
        let nfaMap' =
            Map.change fromInt (fun x ->
                match x with
                | Some (ts, isAccepting) -> Some (Set.add (None, start) ts, isAccepting)
                | None -> Some (Set.ofList [None, start], false))
            <| nfaUnion nfaMap map

        transitionsToNFA' ts symTab' nfaMap' (Set.union alphabet alphabet') endState

let transitionsToNFA (ts : Transitions) : NFA =
    let endState = nextID()
    let symTab = Map.empty
    match ts with
    | [] -> 
        (endState, 
        Map.add endState (Set.empty, true) Map.empty, 
        Set.empty)
    | (startString, symbol, endString) :: ts ->
        let distinctTransitions = List.distinct ((startString, symbol, endString) :: ts)
        let symTab' = bind startString symTab
        let (nfaMap, alphabet, symTab'') = transitionsToNFA' distinctTransitions symTab' Map.empty Set.empty endState
        let start = Map.find startString symTab''
        (start, 
        Map.add endState (Set.empty, true) nfaMap, 
        alphabet)