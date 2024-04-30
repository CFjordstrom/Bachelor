module CheckGrammar

open AbSyn

type Layers = (string list) list

let rec init lst =
    match lst with
    | [] -> []
    | [x] -> []
    | x :: xs -> x :: (init xs)

let rec getDependencies (nt : string) (re : ExtendedRegex) : string list =
    match re with
    | Union (r1, r2) -> 
        let dep1 = getDependencies nt r1
        let dep2 = getDependencies nt r2
        if List.contains nt ((init dep1) @ (init dep2)) then
            failwith ("Recursive nonterminal found in non-tail position in the production of #" + nt + " at line/col")
        else
            dep1 @ dep2

    | Seq (Nonterminal s, Epsilon) -> [s]

    | Seq (r, Epsilon) ->
        let dep = getDependencies nt r
        if List.contains nt (init dep) then
            failwith ("Recursive nonterminal found in non-tail position in the production of #" + nt + " at line/col")
        else
            dep

    | Seq (r1, r2) -> 
        let dep1 = getDependencies nt r1
        let dep2 = getDependencies nt r2
        if List.contains nt (dep1 @ (init dep2)) then
            failwith ("Recursive nonterminal found in non-tail position in the production of #" + nt + " at line/col")
        else
            dep1 @ dep2

    | Class c -> []

    | ZeroOrMore r -> 
        let dep = getDependencies nt r
        if List.contains nt (init dep) then
            failwith ("Recursive nonterminal found in non-tail position in the production of #" + nt + " at line/col")
        else
            dep

    | Nonterminal s -> [s]

    | REComplement r -> 
        let dep = getDependencies nt r
        if List.contains nt dep then
            failwith ("Recursive nonterminal found in a complement expression in the production of #" + nt + " at line/col")
        else
            dep

    | Intersection (r1, r2) -> 
        let dep1 = getDependencies nt r1
        let dep2 = getDependencies nt r2
        if List.contains nt (dep1 @ dep2) then
            failwith ("Recursive nonterminal found in an intersection expression in the production of #" + nt + " at line/col")
        else
            dep1 @ dep2

    | Epsilon -> []
        
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
                            List.exists (fun layer ->
                                List.contains nt' layer || nt = nt'
                            ) layers
                        ) dep

                    if dependenciesDefined then
                        nt :: acc
                    else
                        acc
            ) [] g
            
        if List.isEmpty newLayer then
            failwith ("There is an unresolvable dependency issue in the production(s) of " + (String.concat ", " (List.map (fun s -> "#" + s) <| List.map fst g)))
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