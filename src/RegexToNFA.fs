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

(* set of all printable ascii chars *)
let ascii = Set.ofList <| List.map char [0x20u..0x7Eu]

(* given a regex and a nonterminal, the function returns true if there is a nonterminal in the tail position that is identical to the given nonterminal *)
let rec isRecursive (nt : string) (regex : ExtendedRegex) : bool =
    match regex with
    | Seq(Nonterminal s, Epsilon) when s = nt -> true
    | Seq(r1, r2) -> isRecursive nt r2
    | Union(r1, r2) -> isRecursive nt r1 || isRecursive nt r2
    | _ -> false

(* removes tail nonterminal from a regex *)
let rec removeTailNonterminal (regex : ExtendedRegex) : ExtendedRegex =
    match regex with
    | Seq(Nonterminal s, Epsilon) -> Epsilon
    | Seq(r1, r2) -> Seq(r1, removeTailNonterminal r2)
    | Union(r1, r2) -> Union(removeTailNonterminal r1, removeTailNonterminal r2)
    | _ -> regex

let rec regexToNFARec (regex : ExtendedRegex) (ts : Transitions) (endState : State) : NFA =
    match regex with
    | Union (r1, r2) ->
        let startingState = nextID()
        let (sStart, sMap, sAlphabet) = regexToNFARec r1 ts endState
        let (tStart, tMap, tAlphabet) = regexToNFARec r2 ts endState

        (* combine the two maps into one *)
        let combinedMap = mapDisjointUnion sMap tMap
        (startingState, 
        (* add transitions from the starting state to s and t *)
        Map.add startingState ((Set.ofList [(None, sStart); (None, tStart)]), false) combinedMap,
        Set.union sAlphabet tAlphabet)

    | Seq (r1, r2) ->
        let (tStart, tMap, tAlphabet) = regexToNFARec r2 ts endState
        let (sStart, sMap, sAlphabet) = regexToNFARec r1 ts tStart
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
        | Complement content -> regexToNFARec (Class(ClassContent(Set.difference ascii content))) ts endState

    | ZeroOrMore r ->
        let state = nextID()
        let (start, map, alphabet) = regexToNFARec r ts state
        (state,
        Map.add state ((Set.ofList [(None, endState); (None, start)]), false) map,
        alphabet)

    | Nonterminal s ->
        let productions = List.map (fun (nt, re) -> re) <| List.filter (fun (nt, re) -> nt = s) ts
        let isRecursive' = isRecursive s
        let startingState = nextID()

        (* if the nonterminal has a recursive production *)
        if List.exists isRecursive' productions then
            let nfaList = 
                List.map (fun p ->
                    if isRecursive' p then
                        let re = ZeroOrMore (removeTailNonterminal p)
                        regexToNFARec re ts startingState
                    else
                        regexToNFARec p ts endState
                ) productions

            let (combinedMap, combinedAlphabet) = 
                List.fold (fun (accMap, accAlphabet) (start, map, alphabet) ->
                    (nfaUnion accMap map, Set.union accAlphabet alphabet)
                ) (Map.empty, Set.empty) nfaList

            let transitionsToNFAs = 
                List.fold (fun acc (start, map, alphabet) ->
                    Set.union acc (Set.ofList [None, start])
                ) Set.empty nfaList

            let mapToNFAs = Map.ofList ([(startingState, (transitionsToNFAs, false))])

            (startingState,
            nfaUnion combinedMap mapToNFAs,
            combinedAlphabet)
        else
            let unionProductions = List.fold (fun acc p -> Union(acc, p)) (List.head productions) (List.tail productions)
            regexToNFARec unionProductions ts endState

    | Epsilon -> (endState, Map.empty, Set.empty)

let regexToNFA ts regex = 
    let endState = nextID()
    let (start, map, alphabet) = regexToNFARec regex ts endState
    (start,
    Map.add endState (Set.empty, true) map,
    alphabet)
