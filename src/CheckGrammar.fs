module CheckGrammar

open AbSyn

type Layers = (string list) list



let checkGrammar (ts : Grammar) (layers : Layers): Layers =
    match ts with
    | [] -> layers
    | t :: ts ->
