type error = ..

type nonrec 'a result = ('a, error list) result

exception StaTz_error of error list

let fail e = raise (StaTz_error e)

let ( >>= ) (m : 'a result) (f : 'a -> 'b result) : 'b result =
  match m with Error e -> Error e | Ok x -> f x
  [@@inline]
