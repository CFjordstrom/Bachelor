module RegexToNFA

open AbSyn
open NFAToDFA
open MinimiseDFA
open DFAToNFA
open PrettyPrinter

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
    | Nonterminal s when s = nt -> true
    | _ -> false

(* removes tail nonterminal from a regex *)
let rec removeTailNonterminal (regex : ExtendedRegex) : ExtendedRegex =
    match regex with
    | Seq(Nonterminal s, Epsilon) -> Epsilon
    | Seq(r1, r2) -> Seq(r1, removeTailNonterminal r2)
    | Union(r1, r2) -> Union(removeTailNonterminal r1, removeTailNonterminal r2)
    | Nonterminal s -> Epsilon
    | _ -> regex

(* constructs the product of two DFAs *)
(* currently sets accepting flags for intersection, maybe add so it also can set for union? *)
let rec constructProduct (dfa1 : DFA<State>) (dfa2 : DFA<State>) : DFA<State * State> =
    let (start1, map1, alphabet) = makeMoveTotal <| dfa1
    let (start2, map2, alphabet) = makeMoveTotal <| dfa2

    let keys1 = Map.keys map1
    let keys2 = Map.keys map2
    let states = Seq.collect (fun x -> x) <| Seq.map (fun x -> Seq.map (fun y -> (x, y)) keys2) keys1
    
    let productMap =
        (* for every pair for states *)
        Seq.fold (fun acc (s1, s2) ->
            (* get transition map and accepting flag of each state *)
            let (t1, isAccepting1) = Map.find s1 map1
            let (t2, isAccepting2) = Map.find s2 map2

            let transitionsFromPair =
                (* for each symbol in the alphabet *)
                Set.fold (fun acc' symbol ->
                    (* find destinations of transitions on current symbol *)
                    let s1To = Map.find symbol t1
                    let s2To = Map.find symbol t2

                    (* add transition on this symbol to the pair *)
                    Map.add symbol (s1To, s2To) acc'
                ) Map.empty alphabet

            Map.add (s1, s2) (transitionsFromPair, isAccepting1 && isAccepting2) acc
        ) Map.empty states

    ((start1, start2), productMap, alphabet)

let addAccepting (endState : State) (nfa : NFAMap) : NFAMap =
    Map.change endState (fun x ->
        match x with
        | Some (ts, isAccepting) -> Some (ts, true)
        | None -> Some (Set.empty, true)
    ) nfa

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

    | REComplement r ->
        let (nfaStart, nfaMap, alphabet) = regexToNFARec r ts endState
        let mapWithAccepting = addAccepting endState nfaMap
        let dfa = nfaToDFA (nfaStart, mapWithAccepting, alphabet)
        let (totalStart, totalMap, alphabet) = makeMoveTotal dfa

        (* flip accepting flag *)
        let mapAcceptingFlipped = 
            Map.map (fun state (charMap, isAccepting) ->
                (charMap, not isAccepting)
            ) totalMap

        dfaToNFA (totalStart, mapAcceptingFlipped, alphabet)

    | Intersection (r1, r2) ->
        let (nfaStart1, nfaMap1, alphabet1) = regexToNFARec r1 ts endState
        let (nfaStart2, nfaMap2, alphabet2) = regexToNFARec r2 ts endState

        (* if thw two expressions have the same alphabet then continue *)
        if alphabet1 = alphabet2 then
            (* construct DFA for r1 *)
            let nfaMap1' = addAccepting endState nfaMap1
            let dfa1 = nfaToDFA (nfaStart1, nfaMap1', alphabet1)

            (* construct DFA for r2 *)
            let nfaMap2' = addAccepting endState nfaMap2
            let dfa2 = nfaToDFA (nfaStart2, nfaMap2', alphabet2)

            (* if thw two expressions have the same alphabet *)
            let (productStart, productMap, alphabet) = constructProduct dfa1 dfa2
            
            (* map each pair of product states to a new NFA state *)
            let productStateToNFAStateMap =
                Seq.fold (fun acc s ->
                    Map.add s (nextID()) acc
                ) Map.empty (Map.keys productMap)

            (* convert product to NFA *)
            let nfaMap =
                (* for every state *)
                Map.fold (fun acc statePair (charMap, isAccepting) ->
                    (* get the nfa state for the pair *)
                    let state = Map.find statePair productStateToNFAStateMap
                    (* convert map to set of transitions *)
                    let transitions =
                        Map.fold (fun acc' symbol pairTo ->
                            let nfaTo = Map.find pairTo productStateToNFAStateMap
                            Set.add (Some symbol, nfaTo) acc'
                        ) Set.empty charMap
                    
                    Map.add state (transitions, isAccepting) acc
                ) Map.empty productMap

            (Map.find productStart productStateToNFAStateMap, nfaMap, alphabet)
        else
            failwith "When finding the intersection, the alphabets must be identical"
    
    | Epsilon -> (endState, Map.empty, Set.empty)

let regexToNFA ts regex = 
    let endState = nextID()
    let (start, map, alphabet) = regexToNFARec regex ts endState
    (start,
    Map.change endState (fun x ->
        match x with
        | Some (ts, isAccepting) -> Some (ts, isAccepting)
        | None -> Some (Set.empty, true)
    ) map,
    alphabet)