module RegexToNFA

open AbSyn

let mutable counter = 0
let nextID () =
    counter <- counter + 1
    (counter - 1)

(* set of all printable ascii chars *)
let ascii = Set.ofList <| List.map char [0x20u..0x7Eu]

(* takes two maps and merges them *)
let mapDisjointUnion map1 map2 =
    let (longer, shorter) =
        if Map.count map1 >= Map.count map2 then (map1, map2)
        else (map2, map1)
    Map.fold (fun acc key value -> Map.add key value acc) longer shorter

(* takes a list of regexes and an nfa containing the desired ending state as its starting 
state and computes the NFA that results from combining them all sequentially *)
let rec nfaFromReverseRegexList (rgxlst : Regex list) (acc : NFA): NFA =
    match rgxlst with
    | [] -> acc
    | (r :: rs) ->
        let (accStart, accMap, accAlphabet) = acc
        let (regexStart, regexMap, regexAlphabet) = regexToNFARec r accStart
        let nfa = (regexStart, mapDisjointUnion accMap regexMap, Set.union accAlphabet regexAlphabet)
        nfaFromReverseRegexList rs nfa

and regexToNFARec (regex : Regex) (acc : State) : NFA =
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

    | Concat regexList -> nfaFromReverseRegexList (List.rev regexList) (acc, Map.empty, Set.empty)

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

let regexToNFA regex = 
    let endState = nextID()
    let (start, map, alphabet) = regexToNFARec regex endState
    (start,
    Map.add endState (Set.empty, true) map,
    alphabet)