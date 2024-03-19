module RegexToNFA

open AbSyn

let mutable counter = 1
let nextID () =
    counter <- counter + 1
    (counter - 1)

(* set of all printable ascii chars *)
let ascii = Set.ofList <| List.map char [0x20u..0x7Eu]

(* takes two maps and adds the shorter one to the longer one - the function assumes that the maps are disjoint *)
let mapDisjointUnion map1 map2 =
    let (longer, shorter) =
        if Map.count map1 >= Map.count map2 then (map1, map2)
        else (map2, map1)
    Map.fold (fun acc key value -> Map.add key value acc) longer shorter

let rec regexToNFARec (regex : Regex) (acc : State) : NFA =
    match regex with
    | Union (r1, r2) ->
        let startingState = nextID()
        let (sStart, sMap, sAlphabet) = regexToNFARec r1 acc
        let (tStart, tMap, tAlphabet) = regexToNFARec r2 acc

        (* combine the two maps into one *)
        let combinedMap = mapDisjointUnion sMap tMap
        (startingState, 
        (* add transitions from the starting state to s and t *)
        Map.add startingState ((Set.ofList [(None, sStart); (None, tStart)]), false) combinedMap,
        Set.union sAlphabet tAlphabet)

    //| Concat regexList -> nfaFromReverseRegexList (List.rev regexList) (acc, Map.empty, Set.empty)
    | Seq (r1, r2) ->
        let (tStart, tMap, tAlphabet) = regexToNFARec r2 acc
        let (sStart, sMap, sAlphabet) = regexToNFARec r1 tStart
        (sStart,
        mapDisjointUnion sMap tMap,
        Set.union sAlphabet tAlphabet)

    | Class c ->
        match c with
        | ClassContent content ->
            let startingState = nextID()
            (* create a set of transitions from the start to end on all the given symbols *)
            let transitions = Set.map (fun symbol -> (Some symbol, acc)) content
            (* create a map containing these transitions *)
            let map = Map.ofList [(startingState, (transitions, false))]
            (startingState, map, content)
        | Complement content -> regexToNFARec (Class(ClassContent(Set.difference ascii content))) acc

    | ZeroOrMore r ->
        let state = nextID()
        let (start, map, alphabet) = regexToNFARec r state
        (state,
        Map.add state ((Set.ofList [(None, acc); (None, start)]), false) map,
        alphabet)
    
    | Epsilon -> (acc, Map.empty, Set.empty)

let regexToNFA regex = 
    let endState = nextID()
    let (start, map, alphabet) = regexToNFARec regex endState
    (start,
    Map.add endState (Set.empty, true) map,
    alphabet)
