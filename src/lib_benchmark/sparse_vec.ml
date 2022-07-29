(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Map = Stdlib.Map

(* Our favorite "ring" *)
module R = Float

module type S = sig
  type t

  type basis

  val is_empty : t -> bool

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val zero : t

  val add : t -> t -> t

  val smul : R.t -> t -> t

  val neg : t -> t

  val fold : (basis -> R.t -> 'b -> 'b) -> t -> 'b -> 'b

  val iter : (basis -> R.t -> unit) -> t -> unit

  val find_map : (basis -> R.t -> 'res option) -> t -> 'res option

  val set : t -> basis -> R.t -> t

  val get_exn : t -> basis -> R.t

  val get_opt : t -> basis -> R.t option

  val get : t -> basis -> float

  val swap : t -> basis -> basis -> t

  val eval : t -> basis -> float

  val of_list : (basis * R.t) list -> t

  val to_list : t -> (basis * R.t) list

  val pp :
    pp_basis:(Format.formatter -> basis -> unit) ->
    pp_element:(Format.formatter -> R.t -> unit) ->
    Format.formatter ->
    t ->
    unit

  module Op : sig
    val ( .%[] ) : t -> basis -> float

    val ( .%[]<- ) : t -> basis -> R.t -> t

    val ( + ) : t -> t -> t

    val ( * ) : R.t -> t -> t
  end
end

module Make (M : Tezos_error_monad.TzLwtreslib.Map.S) :
  S with type t = R.t M.t and type basis = M.key = struct
  type t = R.t M.t

  type basis = M.key

  let is_empty = M.is_empty

  let compare : t -> t -> int = M.compare R.compare

  let equal : t -> t -> bool = M.equal R.equal

  let zero = M.empty

  let add (vec1 : t) (vec2 : t) : t =
    M.union
      (fun _elt i1 i2 ->
        let res = R.add i1 i2 in
        if R.compare res R.zero = 0 then None else Some res)
      vec1
      vec2

  let smul coeff vec =
    if R.compare coeff R.zero = 0 then zero
    else M.map (fun x -> R.mul coeff x) vec

  let neg vec = M.map R.neg vec

  let fold = M.fold

  let iter = M.iter

  let find_map : (basis -> R.t -> 'res option) -> t -> 'res option =
    fun (type res) f vec ->
     let exception Return of res in
     try
       M.iter
         (fun basis elt ->
           match f basis elt with None -> () | Some res -> raise (Return res))
         vec ;
       None
     with Return res -> Some res

  let set vec i e =
    if R.compare e R.zero = 0 then M.remove i vec else M.add i e vec

  let get_opt vec i = M.find i vec

  let get_exn vec i = WithExceptions.Option.get ~loc:__LOC__ @@ M.find i vec

  let get vec i = Option.value ~default:R.zero @@ M.find i vec

  let swap vec i j =
    match (M.find_opt i vec, M.find_opt j vec) with
    | None, None -> vec
    | Some elt, None ->
        let vec = set vec i R.zero in
        set vec j elt
    | None, Some elt ->
        let vec = set vec j R.zero in
        set vec i elt
    | Some e1, Some e2 ->
        let vec = set vec i e2 in
        set vec j e1

  let eval v u = match M.find_opt u v with None -> R.zero | Some c -> c

  let of_list : (basis * R.t) list -> t =
   fun l -> List.fold_left (fun vec (i, elt) -> set vec i elt) zero l

  let to_list : t -> (basis * R.t) list = fun m -> List.of_seq (M.to_seq m)

  let pp :
      pp_basis:(Format.formatter -> basis -> unit) ->
      pp_element:(Format.formatter -> R.t -> unit) ->
      Format.formatter ->
      t ->
      unit =
   fun ~pp_basis ~pp_element fmtr vec ->
    if M.is_empty vec then Format.fprintf fmtr "∅"
    else
      let bindings = M.bindings vec in
      Format.pp_print_list
        ~pp_sep:(fun fmtr () -> Format.fprintf fmtr ";@,")
        (fun fmtr (k, v) ->
          Format.fprintf fmtr "%a ↦ %a" pp_basis k pp_element v)
        fmtr
        bindings

  module Op = struct
    let ( .%[] ) vec x = get vec x

    let ( .%[]<- ) vec x v = set vec x v

    let ( + ) = add

    let ( * ) = smul
  end
end

module Make_compare (X : Map.OrderedType) =
  Make (Tezos_error_monad.TzLwtreslib.Map.Make (X))
module String = Make (String.Map)
