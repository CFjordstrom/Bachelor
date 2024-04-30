module RunDFA

open AbSyn

let rec runDFA' (input : string) (dfa : DFA<State>) (state : State) : bool =
    let (start, map, alphabet) = dfa
    let len = String.length input
    match len with
    | 0 -> snd <| Map.find state map
    | _ ->
        let charMap = fst <| Map.find state map
        match Map.tryFind input.[0] charMap with
        | Some s -> runDFA' input.[1..] dfa s
        | None -> false

let runDFA (input : string) (dfa : DFA<State>) : bool =
    let (start, map, alphabet) = dfa
    runDFA' input dfa start