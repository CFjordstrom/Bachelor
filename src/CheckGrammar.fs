module CheckGrammar

open AbSyn
open PrettyPrinter

type Layers = (string list) list

let rec getDependencies' (nt : string) (re : ExtendedRegex) : Set<string> * Set<string> =
    match re with
    | Union(r1, r2) ->
        let (init1, tail1) = getDependencies' nt r1
        let (init2, tail2) = getDependencies' nt r2
        let init = Set.union init1 init2
        let tail = Set.union tail1 tail2
        if Set.contains nt init then
            raise (MyError ("Recursive nonterminal found in non-tail position in expression " + ppRegex re + " in the production of #" + nt))
        else
            (init, tail)

    | Seq(Nonterminal s, Epsilon) -> (Set.empty, Set.singleton s)
    | Seq(r, Epsilon) ->
        let (init, tail) = getDependencies' nt r
        if Set.contains nt init then
            raise (MyError ("Recursive nonterminal found in non-tail position in expression " + ppRegex re + " in the production of #" + nt))
        else
            (init, tail)

    | Seq(r1, r2) ->
        let (init1, tail1) = getDependencies' nt r1
        let (init2, tail2) = getDependencies' nt r2
        let init = Set.union init1 <| Set.union tail1 init2
        if Set.contains nt init then
            raise (MyError ("Recursive nonterminal found in non-tail position in expression " + ppRegex re + " in the production of #" + nt))
        else
            (init, tail2)
    
    | Class c -> (Set.empty, Set.empty)

    | ZeroOrMore r ->
        let (init, tail) = getDependencies' nt r
        if Set.contains nt (Set.union init tail) then
            raise (MyError ("Recursive nonterminal found in \"zero or more\" expression " + ppRegex re + " in the production of #" + nt))
        else
            (init, tail)

    | Nonterminal s -> (Set.empty, Set.singleton s)

    | REComplement r ->
        let (init, tail) = getDependencies' nt r
        if Set.contains nt (Set.union init tail) then
            raise (MyError ("Recursive nonterminal found in complement expression " + ppRegex re + " in the production of #" + nt))
        else
            (init, tail)

    | Intersection(r1, r2) ->
        let (init1, tail1) = getDependencies' nt r1
        let (init2, tail2) = getDependencies' nt r2
        let init = Set.union init1 init2
        let tail = Set.union tail1 tail2
        let all = Set.union init tail
        if Set.contains nt all then
            raise (MyError ("Recursive nonterminal found in intersection expression " + ppRegex (Intersection(r1, r2)) + " in the production of #" + nt))
        else
            (init, tail)
    
    | Epsilon -> (Set.empty, Set.empty)

let getDependencies (nt : string) (re : ExtendedRegex) : string list =
    let (head, tail) = getDependencies' nt re
    Set.toList <| Set.union head tail

let rec buildDependencyGraph (g : Grammar) (layers : Layers) : Layers =
    match g with
    | [] -> layers
    | ps ->
        let newLayer =
            List.fold (fun acc (nt, re) ->
                let dep = getDependencies nt re
                if List.isEmpty dep then
                    nt :: acc
                else
                    let dependenciesDefined =
                        List.forall (fun nt' ->
                            List.exists (fun layer -> List.contains nt' layer) layers 
                            || nt = nt'
                        ) dep
                    if dependenciesDefined then
                        nt :: acc
                    else
                        acc
            ) [] g
            
        if List.isEmpty newLayer then
            raise (MyError ("There is an unresolvable dependency issue in the production(s) of " + (String.concat ", " (List.map (fun s -> "#" + s) <| List.map fst g))))
        else
            let g' = List.filter (fun (nt, _) -> List.contains nt newLayer = false) g
            let layers' = layers @ [newLayer]
            buildDependencyGraph g' layers'

let combineProductions (g : Grammar) : Grammar =
    let nts = List.distinct <| List.map fst g
    List.fold (fun acc nt ->
        let ps = List.map (fun (_, re) -> re) <| List.filter (fun (nt', _) -> nt = nt') g
        let union = List.fold (fun acc' p -> Union(acc', p)) (List.head ps) (List.tail ps)
        (nt, union) :: acc
    ) [] nts

let checkGrammar (g : Grammar) : Layers =
    let combined = combineProductions g
    buildDependencyGraph combined []