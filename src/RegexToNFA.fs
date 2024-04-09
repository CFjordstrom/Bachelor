module RegexToNFA

open AbSyn

let mutable counter = 1
let nextID () =
    counter <- counter + 1
    (counter - 1)

(* takes two maps and adds the shorter one to the longer one - the function assumes that the maps are disjoint *)
let mapDisjointUnion map1 map2 =
    let (longer, shorter) =
        if Map.count map1 >= Map.count map2 then (map1, map2)
        else (map2, map1)
    Map.fold (fun acc key value -> Map.add key value acc) longer shorter

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

(* set of all printable ascii chars *)
let ascii = Set.ofList <| List.map char [0x20u..0x7Eu]

let rec transitionsToNFA (transitions : Transitions) (symTab : StateMap) (nfaMap : NFAMap) (alphabet : Alphabet) (endState : State): NFAMap * Alphabet * StateMap =
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
            Map.add fromInt (Set.ofList [None, start], false)
            <| mapDisjointUnion nfaMap map

        transitionsToNFA ts symTab' nfaMap' (Set.union alphabet alphabet') endState

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
        

        transitionsToNFA ts symTab' nfaMap' (Set.union alphabet alphabet') endState

and regexToNFARec (regex : ExtendedRegex) (symTab : StateMap) (endState : State) : NFA =
    match regex with
    | Union (r1, r2) ->
        let startingState = nextID()
        let (sStart, sMap, sAlphabet) = regexToNFARec r1 symTab endState
        let (tStart, tMap, tAlphabet) = regexToNFARec r2 symTab endState

        (* combine the two maps into one *)
        let combinedMap = mapDisjointUnion sMap tMap
        (startingState, 
        (* add transitions from the starting state to s and t *)
        Map.add startingState ((Set.ofList [(None, sStart); (None, tStart)]), false) combinedMap,
        Set.union sAlphabet tAlphabet)

    | Seq (r1, r2) ->
        let (tStart, tMap, tAlphabet) = regexToNFARec r2 symTab endState
        let (sStart, sMap, sAlphabet) = regexToNFARec r1 symTab tStart
        (sStart,
        mapDisjointUnion sMap tMap,
        Set.union sAlphabet tAlphabet)

    | Class c ->
        match c with
        | ClassContent content ->
            let startingState = nextID()
            (* create a set of transitions from the start to end on all the given symbols *)
            let transitions = Set.map (fun symbol -> (Some symbol, endState)) content
            (* create a map containing these transitions *)
            let map = Map.ofList [(startingState, (transitions, false))]
            (startingState, map, content)
        | Complement content -> regexToNFARec (Class(ClassContent(Set.difference ascii content))) symTab endState

    | ZeroOrMore r ->
        let state = nextID()
        let (start, map, alphabet) = regexToNFARec r symTab state
        (state,
        Map.add state ((Set.ofList [(None, endState); (None, start)]), false) map,
        alphabet)
    
    | Transitions [] -> (endState, Map.empty, Set.empty)
    
    | Transitions ((startString, symbol, endString) :: ts) ->
        let distinctTransitions = List.distinct ((startString, symbol, endString) :: ts)
        let symTab' = bind startString symTab
        let (nfaMap, alphabet, symTab'') = transitionsToNFA distinctTransitions symTab' Map.empty Set.empty endState
        let start = Map.find startString symTab''
        (start, nfaMap, alphabet)

    | Epsilon -> (endState, Map.empty, Set.empty)

let regexToNFA regex = 
    let endState = nextID()
    let symTab = Map.empty
    let (start, map, alphabet) = regexToNFARec regex symTab endState
    (start,
    Map.add endState (Set.empty, true) map,
    alphabet)
