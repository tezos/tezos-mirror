(* Bijections between finite sets and {0; ...; n-1}. *)

module type S = sig
  type t

  type elt

  val of_list : elt list -> t

  val nth_exn : t -> int -> elt

  val nth_opt : t -> int -> elt option

  val idx_exn : t -> elt -> int

  val idx_opt : t -> elt -> int option

  val support : t -> int

  val fold : (elt -> int -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make (Elt : sig
  type t

  val compare : t -> t -> int
end) : S with type elt = Elt.t = struct
  module M = Map.Make (Elt)

  type t = { forward : int M.t; backward : Elt.t array }

  type elt = Elt.t

  let of_list elements =
    let backward = Array.of_list elements in
    let forward =
      M.of_seq Array.(to_seq @@ mapi (fun i elt -> (elt, i)) backward)
    in
    { forward; backward }

  let nth_exn { backward; _ } i = backward.(i)

  let nth_opt { backward; _ } i =
    if i >= Array.length backward then None
    else Some (Array.unsafe_get backward i)

  let idx_exn { forward; _ } elt = M.find elt forward

  let idx_opt { forward; _ } elt = M.find_opt elt forward

  let support { backward; _ } = Array.length backward

  let fold f { backward; _ } acc =
    snd
    @@ Array.fold_left
         (fun (i, acc) elt -> (i + 1, f elt i acc))
         (0, acc)
         backward
end
