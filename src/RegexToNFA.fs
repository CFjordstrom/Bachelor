module RegexToNFA

open AbSyn

let mutable counter = 0
let nextID () =
    counter <- counter + 1
    (counter - 1)

(* set of all printable ascii chars *)
let ascii = List.map char [0x21uy..0x7Euy] |> Set.ofList

(* takes two maps and merges them *)
let mergeMaps map1 map2 =
    Map.fold (fun acc key value -> Map.add key value acc) map1 map2

(* takes a map and sets all its states to be rejecting *)
let setRejecting (map : Map<State, (Set<Transition> * bool)>) : Map<State, (Set<Transition> * bool)> =
    map |> Map.map (fun state (ts, _) -> (ts, false))

(* takes a list of regexes and an nfa containing the desired ending state
   and computes the NFA that results from combining them all sequentially
   could maybe just have been implemented with foldback instead *)
let rec nfaFromReverseRegexList (rgxlst : Regex list) (acc : NFA): NFA =
    match rgxlst with
    | [] -> acc
    | (r :: rs) ->
        let (accStart, accMap, accAlphabet) = acc
        let (regexStart, regexMap, regexAlphabet) = regexToNFARec r accStart
        let nfa = (regexStart, mergeMaps accMap regexMap, Set.union accAlphabet regexAlphabet)
        nfaFromReverseRegexList rs nfa

and regexToNFARec (regex : Regex) (acc : State) : NFA =
    match regex with
    | Union (r1, r2) ->
        let startingState = nextID()
        let endState = nextID()
        let (sStart, sMap, sAlphabet) = regexToNFARec r1 endState
        let (tStart, tMap, tAlphabet) = regexToNFARec r2 endState

        (startingState, 
        (* merge the mappings of the two NFAs *)
        mergeMaps sMap tMap
        (* set all their states to be rejecting *)
        |> setRejecting
        (* add transitions from the starting state to s and t *)
        |> Map.add startingState ((Set.ofList [(startingState, None, sStart); (startingState, None, tStart)]), false)
        (* add a transition from the end state to the given end state(maybe just use the given end state as end state?) *)
        |> Map.add endState ((Set.ofList [(endState, None, acc)]), false)
        , Set.union sAlphabet tAlphabet)

    | Concat regexList -> nfaFromReverseRegexList (List.rev regexList) (acc, Map.empty, Set.empty)

    | Class c ->
        match c with
        | ClassContent content ->
            let startingState = nextID()
            let endState = nextID()

            (* create a set of transitions from the start to end on all the given symbols *)
            let transitions = Set.map (fun c -> (startingState, Some c, endState)) content
            (* create a map containing these transitions *)
            let map = Map.add startingState (transitions, false) Map.empty
            (* add a transition from the end state to the given end state(maybe just use the given end state as end state?) *)
            let map2 = Map.add endState ((Set.ofList [(endState, None, acc)]), false) map
            (startingState, map2, content)
        | Complement content -> regexToNFARec (Class(ClassContent(Set.difference ascii content))) acc

    | Char c ->
        let startingState = nextID()
        (startingState,
        Map.empty |> Map.add startingState ((Set.ofList [(startingState, Some c, acc)]), false),
        Set.singleton c)

    | ZeroOrMore r ->
        let state = nextID()
        let (start, map, alphabet) = regexToNFARec r state
        (state,
        Map.add state ((Set.ofList [(state, None, acc); (state, None, start)]), false) map,
        alphabet)

let regexToNFA regex = 
    let endState = nextID()
    let (start, map, alphabet) = regexToNFARec regex endState
    (start,
    map |> Map.add endState (Set.empty, true),
    alphabet)