module RegexToNFA

open AbSyn
open NFAToDFA
//open MinimiseDFA
open DFAToNFA
open CheckGrammar

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

(* given a regex and a nonterminal, the function returns true if there is a nonterminal in the tail position that is identical to the given nonterminal *)
let rec isRecursive (nt : string) (regex : ExtendedRegex) : bool =
    match regex with
    | Seq(Nonterminal s, Epsilon) when s = nt -> true
    | Seq(r1, r2) -> isRecursive nt r2
    | Union(r1, r2) -> isRecursive nt r1 || isRecursive nt r2
    | Nonterminal s when s = nt -> true
    | _ -> false

(* removes tail nonterminal from a regex *)
let rec removeRecursiveTailNonterminal (regex : ExtendedRegex) (nt : string): ExtendedRegex =
    match regex with
    | Seq(Nonterminal s, Epsilon) when s = nt -> Epsilon
    | Seq(r1, r2) -> Seq(r1, removeRecursiveTailNonterminal r2 nt)
    | Union(r1, r2) -> Union(removeRecursiveTailNonterminal r1 nt, removeRecursiveTailNonterminal r2 nt)
    | Nonterminal s when s = nt -> Epsilon
    | _ -> regex

let makeMoveTotal (dfa : DFA<State>) : DFA<State> =
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

let rec computeAlphabet (regex : ExtendedRegex) (g: Grammar) (visited : string list) : Set<char> =
    match regex with
    | Union (r1, r2) -> Set.union (computeAlphabet r1 g visited) (computeAlphabet r2 g visited)
    | Seq (r1, r2) -> Set.union (computeAlphabet r1 g visited) (computeAlphabet r2 g visited)
    | Class c ->
        match c with
        | ClassContent content -> content
        | Complement content -> Set.empty
    | ZeroOrMore r -> computeAlphabet r g visited
    | Nonterminal s ->
        if List.contains s visited then
            Set.empty
        else
            let productions = List.map (fun (nt, re) -> re) <| List.filter (fun (nt, re) -> nt = s) g
            let visited' = s :: visited
            List.fold (fun acc p -> Set.union acc (computeAlphabet p g (visited'))) Set.empty productions
    | REComplement r -> computeAlphabet r g visited
    | Intersection (r1, r2) -> Set.union (computeAlphabet r1 g visited) (computeAlphabet r2 g visited)
    | Epsilon -> Set.empty

let rec regexToNFARec (regex : ExtendedRegex) (grammar : Grammar) (alphabet : Alphabet) (endState : State) : (State * NFAMap) =
    match regex with
    | Union (r1, r2) ->
        let startingState = nextID()
        let (sStart, sMap) = regexToNFARec r1 grammar alphabet endState
        let (tStart, tMap) = regexToNFARec r2 grammar alphabet endState
        (* combine the two maps into one *)
        let combinedMap = mapDisjointUnion sMap tMap
        (startingState, 
        (* add transitions from the starting state to s and t *)
        Map.add startingState ((Set.ofList [(None, sStart); (None, tStart)]), false) combinedMap)

    | Seq (r1, r2) ->
        let (tStart, tMap) = regexToNFARec r2 grammar alphabet endState
        let (sStart, sMap) = regexToNFARec r1 grammar alphabet tStart
        (sStart,
        mapDisjointUnion sMap tMap)

    | Class c ->
        match c with
        | ClassContent content ->
            let startingState = nextID()
            (* create a set of transitions from the start to end on all the given symbols *)
            let transitions = Set.map (fun symbol -> (Some symbol, endState)) content
            (* create a map containing these transitions *)
            let map = Map.ofList [(startingState, (transitions, false))]
            (startingState, map)
        | Complement content -> regexToNFARec (Class(ClassContent(Set.difference alphabet content)))grammar alphabet endState

    | ZeroOrMore r ->
        let state = nextID()
        let (start, map) = regexToNFARec r grammar alphabet state
        (state,
        Map.add state ((Set.ofList [(None, endState); (None, start)]), false) map)

    | Nonterminal s ->
        (*match transitions with
        | NTab ntab ->
            let (start, map, alphabet) = Map.find s ntab
            let map' =
                Map.map (fun _ (ts, isAccepting) ->
                    let ts' =
                        Set.map (fun (symbol, state) ->
                            if state = 0 then
                                (symbol, endState)
                            else
                                (symbol, state)
                        ) ts
                    (ts', isAccepting)
                ) map

            (start, map', alphabet)
        *)
        //| Grammar grammar ->

        let productions = List.map (fun (nt, re) -> re) <| List.filter (fun (nt, re) -> nt = s) grammar
        if List.isEmpty productions then
            raise (MyError ("There are no productions for nonterminal #" + s))
        else
            let isRecursive' = isRecursive s

            (* if the nonterminal has a recursive production *)
            if List.exists isRecursive' productions then
                let startingState = nextID()

                let nfaList = 
                    List.map (fun p ->
                        if isRecursive' p then
                            let re = ZeroOrMore (removeRecursiveTailNonterminal p s)
                            regexToNFARec re grammar alphabet startingState
                        else
                            regexToNFARec p grammar alphabet endState
                    ) productions

                let combinedMap = 
                    List.fold (fun accMap (start, map) ->
                        (nfaUnion accMap map)
                    ) Map.empty nfaList

                let transitionsToNFAs = 
                    List.fold (fun acc (start, map) ->
                        Set.union acc (Set.ofList [None, start])
                    ) Set.empty nfaList

                let mapToNFAs = Map.ofList ([(startingState, (transitionsToNFAs, false))])

                (startingState,
                nfaUnion combinedMap mapToNFAs)
            else
                let unionProductions = List.fold (fun acc p -> Union(acc, p)) (List.head productions) (List.tail productions)
                regexToNFARec unionProductions grammar alphabet endState

    | REComplement r ->
        let (nfaStart, nfaMap) = regexToNFARec r grammar alphabet endState
        let mapWithAccepting = addAccepting endState nfaMap
        let dfa = nfaToDFA (nfaStart, mapWithAccepting, alphabet)
        let (totalStart, totalMap, _) = makeMoveTotal dfa

        (* flip accepting flag *)
        let mapAcceptingFlipped = 
            Map.map (fun state (charMap, isAccepting) ->
                (charMap, not isAccepting)
            ) totalMap

        let (start, map, _) = dfaToNFA (totalStart, mapAcceptingFlipped, alphabet)

        let mapAcceptingToEnd =
            Map.map (fun state (ts, isAccepting) ->
                if isAccepting then
                    (Set.add (None, endState) ts, false)
                else
                    (ts, isAccepting)
            ) map

        (start, mapAcceptingToEnd)

    | Intersection (r1, r2) ->
        let (nfaStart1, nfaMap1) = regexToNFARec r1 grammar alphabet endState
        let (nfaStart2, nfaMap2) = regexToNFARec r2 grammar alphabet endState

        (* construct DFA for r1 *)
        let nfaMap1' = addAccepting endState nfaMap1
        let dfa1 = nfaToDFA (nfaStart1, nfaMap1', alphabet)

        (* construct DFA for r2 *)
        let nfaMap2' = addAccepting endState nfaMap2
        let dfa2 = nfaToDFA (nfaStart2, nfaMap2', alphabet)

        (* if thw two expressions have the same alphabet *)
        let (productStart, productMap, _) = constructProduct dfa1 dfa2
        
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

        let mapAcceptingToEnd =
            Map.map (fun state (ts, isAccepting) ->
                if isAccepting then
                    (Set.add (None, endState) ts, false)
                else
                    (ts, isAccepting)
            ) nfaMap

        (Map.find productStart productStateToNFAStateMap, mapAcceptingToEnd)
    
    | Epsilon -> (endState, Map.empty)

let rec attachToEnd (regex : ExtendedRegex) (attachee : ExtendedRegex) : ExtendedRegex =
    match regex with
    | Seq(r1, Epsilon) -> Seq(r1, attachee)
    | Seq(r1, r2) -> Seq(r1, attachToEnd r2 attachee)
    | Epsilon -> attachee
    | _ -> Seq(regex, attachee)

(* removes all Unions that can contain recursive nonterminals from an extended regular expression and returns a list of all the individual expressions *)
let rec removeUnion (regex : ExtendedRegex) : ExtendedRegex list =
    match regex with
    | Union(r1, r2) ->
        let r1list = removeUnion r1
        let r2list = removeUnion r2
        r1list @ r2list
    | Seq(r1, r2) ->
        let r1list = removeUnion r1
        let r2list = removeUnion r2
        List.concat
        <| List.map (fun re1 ->
            List.map (fun re2 ->
                attachToEnd re1 re2
            ) r2list
        ) r1list
    (* not needed to remove unions since they either cannot contain other expressions (Class, Nonterminal, Epsilon), 
    or are not allowed to contain recursive unions (ZeroOrMore, REComplement, Intersection) *)
    | _ -> [regex]

let regexToNFA (grammar : Grammar) (regex : ExtendedRegex) (alphabet : Alphabet option) : NFA =
    let layers = checkGrammar grammar
    
    let grammarUnionRemoved = 
        List.fold (fun acc (nt, re) ->
            let re' = removeUnion re
            let g = List.map (fun x -> (nt, x)) re'
            acc @ g
        ) [] grammar
    
    (*let ntab =
        List.fold (fun acc nt ->
            let nfa = regexToNFARec (Nonterminal nt) (Grammar grammar) 0
            Map.add nt nfa acc
        ) Map.empty (List.concat layers)
    *)
    let alphabet' =
        match alphabet with
        | Some a -> a
        | None -> computeAlphabet regex grammar []
    let endState = nextID()
    let (start, map) = regexToNFARec regex grammarUnionRemoved alphabet' endState

    (start,
    Map.change endState (fun x ->
        match x with
        | Some (transitions, isAccepting) -> Some (transitions, isAccepting)
        | None -> Some (Set.empty, true)
    ) map,
    alphabet')