module DFAToNFA

open AbSyn

let dfaToNFA (dfa : DFA<State>) : NFA =
    let (start, dfaMap, alphabet) = dfa
    let nfaMap =
        Map.map (fun start (symbolToStateMap, accepting) ->
            let transitions =
                Map.fold (fun acc symbol state ->
                    Set.add (Some symbol, state) acc
                ) Set.empty symbolToStateMap
            (transitions, accepting)
        ) dfaMap
    (start, nfaMap, alphabet)