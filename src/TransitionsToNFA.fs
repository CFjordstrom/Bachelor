module TransitionsToNFA

open AbSyn

let mutable counter = 1
let nextID () =
    counter <- counter + 1
    (counter - 1)

