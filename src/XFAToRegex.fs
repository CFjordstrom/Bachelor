module XFAToRegex

open AbSyn
open NFAToDFA

let kleenesAlgorithm (dfa : DFARegexTransitions) : Regex =
    let (start, map, alphabet) = dfa
    (* stop when the starting state and accepting states are left *)
    if Map.forall (fun state (transitions, isAccepting) -> state = start || isAccepting = true) map then
        Map.fold 
    else
        (*
        1. select state that is not starting or accepting
        *)

let dfaToDFARegexTransitions (dfa : DFA) : DFARegexTransitions =
    let (start, dfaMap, alphabet) = dfa
    let mapRegexTransitions =
        (* for each state *)
        Map.map (fun state (map, isAccepting) ->
            (* convert transitions to regexes *)
            let regexMap =
                Map.fold (fun acc symbol dest ->
                    let regex = Class(ClassContent(Set.ofList [symbol]))
                    Map.add regex dest acc
                ) Map.empty map
            (regexMap, isAccepting)
        ) dfaMap
    printfn "%A" mapRegexTransitions
    (start, mapRegexTransitions, alphabet)
        
let xfaToRegex (automaton : obj) : Regex =
    match automaton with
    | :? NFA as nfa -> kleenesAlgorithm << dfaToDFARegexTransitions <| nfaToDFA nfa
    | :? DFA as dfa -> kleenesAlgorithm <| dfaToDFARegexTransitions dfa
    | _ -> failwith "invalid argument type to xfaToRegex"