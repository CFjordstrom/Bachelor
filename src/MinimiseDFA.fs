module MinimiseDFA

open AbSyn

let mutable counter = 0
let nextID () =
    counter <- counter + 1
    (counter - 1)

let makeMoveTotal (dfa : DFA) : DFA =
    let (dfaStart, dfaMap, alphabet) = dfa
    let deadState = -1
    let deadStateTransitions = Set.fold (fun acc symbol -> Map.add symbol deadState acc) Map.empty alphabet
    
    (* for each state in the DFA *)
    let totalWithoutDeadState = 
        Map.map (fun state (symbolToStateMap, isAccepting) ->
        (* for each symbol in the alphabet *)
            let transitions = 
                Set.fold (fun acc symbol ->
            (* if a transition on the symbol exists, keep it, otherwise add a transition to the dead state *)
                    match Map.tryFind symbol symbolToStateMap with
                    | Some s -> Map.add symbol s acc
                    | None -> Map.add symbol deadState acc
                ) Map.empty alphabet
            (transitions, isAccepting)
        ) dfaMap

    let totalWithDeadState = Map.add deadState (deadStateTransitions, false) totalWithoutDeadState
    (dfaStart, 
    totalWithDeadState, 
    alphabet)

(* takes a group, the worklist and the dfa and returns  
let generateTransitionTable (group : State) (workList : WorkList) (dfa : DFA) : (Map<State, Map<char, State>>) =
    let 
    true
*)
(* takes a worklist consisting of two groups: the accepting and rejecting states, the dfa and an empty map
   and returns the transitions of the minimised dfa *)
let rec constructMinimalDFA (workList : WorkList) (dfa : DFA) : DFA =
    (*
    1. stop if all groups are singleton or marked
    2. pick unmarked and non-singleton group
    3. check if consistent
        3.1 if consistent, go to step 1
        3.2 if not consistent -> split into maximal consistent subgroups
        3.3 replace group with new groups
    *)
    let (dfaStart, dfaMap, alphabet) = dfa
    (* 1. if all groups are marked or singleton then stop *)
    if Map.forall (fun state (transitions, mark) -> Set.count transitions = 1 || mark = true) workList then
        // generate new states and transitions
        let deadStateKey = Map.findKey (fun start (states, mark) -> Set.contains -1 states) workList
        (* remove the dead state *)
        let workListDeadStateRemoved = Map.remove deadStateKey workList

        let minimisedMap = 
            (* for each group *)
            Map.fold (fun acc state (states, mark) ->
                let (transitions, isAccepting) = Map.find (Set.minElement states) dfaMap
                (* remove transitions to the dead state and *)
                let transitionsNoDead = Map.filter (fun symbol dest -> dest <> -1) transitions
                (* fix transitions such that they map from char to group instead of char to dfa state *)
                let transitionsToGroup = 
                    Map.map (fun symbol dest ->
                        Map.findKey (fun s (ss, mark) -> Set.contains dest ss) workListDeadStateRemoved
                    ) transitionsNoDead
                Map.add state (transitionsToGroup, isAccepting) acc
            ) Map.empty workListDeadStateRemoved
        
        (Map.findKey (fun s (ss, mark) -> Set.contains dfaStart ss) workListDeadStateRemoved,
        minimisedMap,
        alphabet)
    else
        (* 2. pick unmarked and non-singleton group *)
        let (currentGroup, (currentDFAStates, currentMark)) =
            Map.minKeyValue <| Map.filter (fun state (transitions, mark) ->
                mark = false && Set.count transitions > 1
            ) workList

        (* 3. generate transition table *)
        (* filter out states that are not in the current group and remove accepting/rejecting flag*)
        let transitionTableToDFAState = 
            Map.map (fun start (transitions, isAccepting) -> transitions) 
            <| Map.filter (fun start (transitions, isAccepting) -> Set.contains start currentDFAStates) dfaMap
        
        (* modify table such that the transitions are to groups instead of dfa states *)
        let transitionTableToGroup =
            (* for each state *)
            Map.map (fun start transitions ->
                (* for each transition from that state *)
                Map.map (fun symbol dest ->
                    let group = Map.findKey (fun _ (states, _) -> Set.contains dest states) workList
                    group  
                ) transitions
            ) transitionTableToDFAState
        
        (* 3. check if the transition table is consistent *)
        (* create a set with the unique transitions *)
        let uniqueTransitions = Map.fold (fun acc start transitions -> Set.add transitions acc) Set.empty transitionTableToGroup
        (* since sets don't have duplicate elements, if there is one element, then all transitions on the same symbol lead to the same group, making the group consistent *)
        (* 3.1 if the group is consistent, go back to step 1 *)
        if Set.count uniqueTransitions = 1 then
            (* mark the group *)
            let markedWorkList =
                Map.change currentGroup (fun x ->
                    match x with
                    | Some(s, m) -> Some (s, true)
                    | None -> None
                ) workList
            constructMinimalDFA markedWorkList dfa
        else
            (* 3.2 not consistent, so split into maximal consistent subgroups *)
            let maximalConsistentSubgroups =
                (* for each unique transition *)
                Set.map (fun transitions1 ->
                    (* for each state in the current group *)
                    Map.fold ( fun acc state transitions2 ->
                        if transitions1 = transitions2 then
                            Set.add state acc
                        else
                            acc
                    ) Set.empty transitionTableToGroup
                ) uniqueTransitions

            (* 3.3 remove old group and add new groups to worklist *)
            let workListWithGroupRemoved = Map.remove currentGroup workList
            let newWorkList = 
                Set.fold (fun acc subgroup ->
                    Map.add (nextID()) (subgroup, false) acc
                ) workListWithGroupRemoved maximalConsistentSubgroups

            constructMinimalDFA newWorkList dfa

let minimiseDFA (dfa : DFA) : DFA =
    let (dfaStart, dfaMap, alphabet) = makeMoveTotal dfa

    (* split dfa into accepting and rejecting states *)
    let (accepting, rejecting) = 
        Map.fold (fun (accepting, rejecting) state (symbolToStateMap, isAccepting) ->
            if isAccepting then
                (Set.add state accepting, rejecting)
            else
                (accepting, Set.add state rejecting)
        ) (Set.empty, Set.empty) dfaMap
    
    let workList = Map.ofList ([nextID(), (accepting, false); nextID(), (rejecting, false)])
    constructMinimalDFA workList (dfaStart, dfaMap, alphabet)