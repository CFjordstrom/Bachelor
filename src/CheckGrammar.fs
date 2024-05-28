module CheckGrammar

open AbSyn
open PrettyPrinter

let format (set : Set<string>) : string =
    let formatted = Set.map (fun s -> "#" + s) set
    String.concat ", " formatted

let rec getDependencies' (nts : Set<string>) (re : ExtendedRegex) : Set<string> * Set<string> =
    match re with
    | Union(r1, r2) ->
        let (init1, tail1) = getDependencies' nts r1
        let (init2, tail2) = getDependencies' nts r2
        let init = Set.union init1 init2
        let tail = Set.union tail1 tail2
        if Set.exists (fun nt -> Set.contains nt init) nts then
            raise (MyError ("Recursive nonterminal found in non-tail position in expression " + ppRegex re + " in the production(s) of #" + format nts))
        else
            (init, tail)

    | Seq(Nonterminal s, Epsilon) -> (Set.empty, Set.singleton s)
    | Seq(r, Epsilon) ->
        let (init, tail) = getDependencies' nts r
        if Set.exists (fun nt -> Set.contains nt init) nts then
            raise (MyError ("Recursive nonterminal found in non-tail position in expression " + ppRegex re + " in the production(s) of " + format nts))
        else
            (init, tail)

    | Seq(r1, r2) ->
        let (init1, tail1) = getDependencies' nts r1
        let (init2, tail2) = getDependencies' nts r2
        let init = Set.union init1 <| Set.union tail1 init2
        if Set.exists (fun nt -> Set.contains nt init) nts then
            raise (MyError ("Recursive nonterminal found in non-tail position in expression " + ppRegex re + " in the production(s) of " + format nts))
        else
            (init, tail2)
    
    | Class c -> (Set.empty, Set.empty)

    | ZeroOrMore r ->
        let (init, tail) = getDependencies' nts r
        let all = Set.union init tail
        if Set.exists (fun nt -> Set.contains nt all) nts then
            raise (MyError ("Recursive nonterminal found in \"zero or more\" expression " + ppRegex re + " in the production(s) of " + format nts))
        else
            (init, tail)

    | Nonterminal s -> (Set.empty, Set.singleton s)

    | REComplement r ->
        let (init, tail) = getDependencies' nts r
        let all = Set.union init tail
        if Set.exists (fun nt -> Set.contains nt all) nts then
            raise (MyError ("Recursive nonterminal found in complement expression " + ppRegex re + " in the production(s) of " + format nts))
        else
            (init, tail)

    | Intersection(r1, r2) ->
        let (init1, tail1) = getDependencies' nts r1
        let (init2, tail2) = getDependencies' nts r2
        let init = Set.union init1 init2
        let tail = Set.union tail1 tail2
        let all = Set.union init tail
        if Set.exists (fun nt -> Set.contains nt all) nts then
            raise (MyError ("Recursive nonterminal found in intersection expression " + ppRegex (Intersection(r1, r2)) + " in the production(s) of " + format nts))
        else
            (init, tail)
    
    | Epsilon -> (Set.empty, Set.empty)

let rec getDependencies (nts : Set<string>) (g : Grammar) : Set<string> =
    let deps = 
        Set.fold (fun acc nt ->
            match List.tryFind (fun (nt', re) -> nt = nt') g with
            | Some (_, re) ->
                let (head, tail) = getDependencies' (Set.singleton nt) re
                Set.union acc <| Set.union head tail
            (* if nt not in grammar, then it is in a previous layer and its dependencies do not matter *)
            | None -> acc
        ) nts nts
    
    if deps = nts then
        deps
    else
        getDependencies deps g

let rec buildLayers (g : Grammar) (layers : Layers) : Layers =
    match g with
    | [] -> layers
    | ps ->
        let newLayer =
            List.fold (fun acc (nt, re) ->
                let dep = Set.toList <| getDependencies (Set.singleton nt) g
                if List.isEmpty dep then
                    nt :: acc
                else
                    let dependenciesDefined =
                        List.forall (fun nt' ->
                            match List.tryFind (fun (nt'', _) -> nt'' = nt') g with
                            | Some (ntDep, reDep) ->
                                List.exists (fun layer -> List.contains nt' layer) layers 
                                || Set.contains nt <| getDependencies (Set.singleton ntDep) g
                            (* NT not in grammar means that it is in a previous layer *)
                            | None -> true
                        ) dep
                    if dependenciesDefined then
                        nt :: acc
                    else
                        acc
            ) [] g
        
        (* if there are still productions left in the grammar, but no new layer can be built, then the grammar is not stratifiable *)
        if List.isEmpty newLayer then
            raise (MyError ("There is an unresolvable dependency issue in the production(s) of " + (String.concat ", " (List.map (fun s -> "#" + s) <| List.map fst g))))
        else
            let g' = List.filter (fun (nt, _) -> List.contains nt newLayer = false) g
            let layers' = layers @ [newLayer]
            buildLayers g' layers'

let combineProductions (g : Grammar) : Grammar =
    let nts = List.distinct <| List.map fst g
    List.fold (fun acc nt ->
        let ps = List.map (fun (_, re) -> re) <| List.filter (fun (nt', _) -> nt = nt') g
        let union = List.fold (fun acc' p -> Union(acc', p)) (List.head ps) (List.tail ps)
        (nt, union) :: acc
    ) [] nts

(* build layers and then check that mutual recursion is only in tail position *)

let checkGrammar (g : Grammar) : Layers =
    let combined = combineProductions g
    (* build layers *)
    let layers = buildLayers combined []
    (* check that mutual recursion only happens in tail position *)
    List.iter (fun (nt, re) ->
        let layer = List.find (fun layer -> List.contains nt layer) layers
        ignore <| getDependencies' (Set.ofList layer) re
    ) g
    layers