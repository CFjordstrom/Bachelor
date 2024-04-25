module CheckTransitions

open AbSyn

(* check if a given regex is well-formed i.e. all non-tail nonterminals must already have been defined *)
(* if there is a nonterminal in the tail position, then it must either be defined previously or later *)
(* returns a bool indicating if the regex is well-formed or not, and a list of nonterminals whose productions need to be defined later *)
let rec checkExtendedRegex (re : ExtendedRegex) (visited : string list) : bool * string list=
    match re with
    | Union (r1, r2) -> 
        let (isWellFormed1, missing1) = checkExtendedRegex r1 visited
        let (isWellFormed2, missing2) = checkExtendedRegex r2 visited
        (isWellFormed1 && isWellFormed2, missing1 @ missing2)

    (* if nonterminal in tail position *)
    | Seq (Nonterminal s, Epsilon) ->
        let (isWellFormed, missing) = checkExtendedRegex (Nonterminal s) visited
        if isWellFormed then
            (true, [])
        else
            (true, s :: missing)

    | Seq (r1, r2) ->
        let (isWellFormed1, missing1) = checkExtendedRegex r1 visited
        let (isWellFormed2, missing2) = checkExtendedRegex r2 visited

        (isWellFormed1 && isWellFormed2, missing1 @ missing2)

    | Class c -> (true, [])
    | ZeroOrMore r -> checkExtendedRegex r visited
    | Nonterminal s -> (List.contains s visited, [])
    | REComplement r -> failwith "not yet implemented"
    | Intersection (r1, r2) -> failwith "not yet implemented"
    | Epsilon -> (true, [])

(* check if all transitions are well-formed i.e. all non-tail nonterminals must already have been defined *)
let rec checkTransitions' (transitions : Transitions) (visited : string list) (missing : string list) : bool =
    match transitions with
    | [] ->
        if List.isEmpty missing then
            true
        else 
            false

    | (nonterminal, r) :: ts ->
        let (isWellFormed, missing') = checkExtendedRegex r visited
        if isWellFormed then
            let missingCurrentRemoved = List.filter (fun x -> x <> nonterminal) missing
            checkTransitions' ts (nonterminal :: visited) (missing' @ missingCurrentRemoved)
        else
            false

let checkTransitions ts = checkTransitions' ts [] []