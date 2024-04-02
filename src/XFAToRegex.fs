module XFAToRegex

open AbSyn
open DFAToNFA


let nfaToGNFAMap (nfa : NFA) : GNFA =
    let (nfaStart, nfaMap, alphabet) = nfa
    let keys = Map.keys nfaMap
    let start = 0
    let accept = Seq.max keys + 1

    (* create transitions from old accepting states to the new accepting state *)
    let mapTransitionsToNewAccepting = 
        Map.map (fun state (transitions, isAccepting) -> 
            if isAccepting then
                (Set.add (None, accept) transitions, false)
            else
                (transitions, isAccepting)
        ) nfaMap

    (* add the new states to the map *)
    let newMap = 
        Map.add accept (Set.empty, true)
        <| Map.add start (Set.singleton (None, nfaStart), false) mapTransitionsToNewAccepting
    
    let gnfa = Array2D.create (Seq.length keys + 2) (Seq.length keys + 2) None

    (* convert nfa to gnfa *)
    (* for each mapping *)
    Map.iter (fun state (transitions, isAccepting) ->
            (* for each transition *)
            Set.iter (fun (symbol, dest) ->
                (* convert it to a regular expression *)
                let transition =
                    match symbol with
                    | Some c -> Seq(Class ( ClassContent ( Set.singleton c)), Epsilon)
                    | None -> Epsilon

                (* update the corresponding field with a new regular expression *)
                match gnfa[state, dest] with
                | Some r -> 
                    gnfa[state, dest] <- Some <| Union(r, transition)
                | None -> 
                    gnfa[state, dest] <- Some transition
            ) transitions
    ) newMap

    gnfa

(* Assumes that regex1 and regex2 are right associated Seqs *)
let rec makeSeq (regex1 : Regex) (regex2 : Regex) : Regex =
    match regex1, regex2 with
    | Epsilon, r -> r
    | r, Epsilon -> r
    | Seq(r1, Epsilon), r2 -> Seq(r1, r2)
    | Seq(r1, r2), r3 -> Seq(r1, makeSeq r2 r3)
    | r1, r2 -> Seq(r1, r2)

(*
let rec makeUnion (regex1 : Regex) (regex2 : Regex ) : Regex =
    match regex1, regex2 with
    | 
*)
let kleenesAlgorithm (gnfa : GNFA) : Regex =
    let len = Array2D.length1 gnfa
    (* for every state that needs to be removed *)
    Array2D.iteri (fun i j t -> 
        if i = j && i <> 0 && i <> len - 1 then
            let incoming = gnfa[*, i]
            let outgoing = gnfa[i, *]
            (* for every incoming transition *)
            Array.iteri (fun k v1 ->
                if k <> i then
                    match v1 with
                    | None -> ()
                    | Some rIn ->
                    (* for every outgoing transition *)
                        Array.iteri (fun l v2 ->
                            if l <> i then
                                match v2 with
                                | None -> ()
                                | Some rOut ->
                                    let r1 =
                                        match t with
                                        | Some (Seq(rSelf, Epsilon)) ->
                                            let rInRSelf = makeSeq rIn (Seq(ZeroOrMore(rSelf), Epsilon))
                                            makeSeq rInRSelf rOut
                                        | Some rSelf ->
                                            let rInRSelf = makeSeq rIn (ZeroOrMore(rSelf))
                                            makeSeq rInRSelf rOut
                                        | None -> makeSeq rIn rOut

                                    match gnfa[k, l] with
                                    | Some r2 -> gnfa[k, l] <- Some (Union(r1, r2))
                                    | None -> gnfa[k, l] <- Some r1
                        ) outgoing
            ) incoming
    ) gnfa

    match gnfa[0, len - 1] with
    | Some r -> r
    | None -> Epsilon

let xfaToRegex (automaton : obj) : Regex =
    let input = 
        match automaton with
        | :? NFA as nfa -> nfa
        | :? DFA as dfa -> dfaToNFA dfa
        | _ -> failwith "invalid argument type to xfaToRegex"
    
    let gnfa = nfaToGNFAMap input
    kleenesAlgorithm gnfa