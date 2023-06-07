type t = Tezos_tree_encoding.wrapped_tree option

exception Durable_empty

let of_tree tree = Some tree

let to_tree_exn = function Some tree -> tree | None -> raise Durable_empty

let to_tree t = t

let empty = None
