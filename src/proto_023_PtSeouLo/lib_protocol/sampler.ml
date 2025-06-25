(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(*

   This module implements the alias method for sampling from a given
   distribution. The distribution need not be normalized.

*)

module type SMass = sig
  type t

  val encoding : t Data_encoding.t

  val zero : t

  val of_int : int -> t

  val mul : t -> t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val ( = ) : t -> t -> bool

  val ( <= ) : t -> t -> bool

  val ( < ) : t -> t -> bool
end

module type S = sig
  type mass

  type 'a t

  val create : ('a * mass) list -> 'a t

  val sample : 'a t -> (int_bound:int -> mass_bound:mass -> int * mass) -> 'a

  val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t

  val map : ('a -> 'b) -> 'a t -> 'b t
end

module Make (Mass : SMass) : S with type mass = Mass.t = struct
  type mass = Mass.t

  type 'a t = {
    total : Mass.t;
    support : 'a FallbackArray.t;
    p : Mass.t FallbackArray.t;
    alias : int FallbackArray.t;
  }

  let rec init_loop total p alias small large =
    match (small, large) with
    | [], _ -> List.iter (fun (_, i) -> FallbackArray.set p i total) large
    | _, [] ->
        (* This can only happen because of numerical inaccuracies e.g. when using
           [Mass.t = float] *)
        List.iter (fun (_, i) -> FallbackArray.set p i total) small
    | (qi, i) :: small', (qj, j) :: large' ->
        FallbackArray.set p i qi ;
        FallbackArray.set alias i j ;
        let qj' = Mass.sub (Mass.add qi qj) total in
        if Mass.(qj' < total) then
          init_loop total p alias ((qj', j) :: small') large'
        else init_loop total p alias small' ((qj', j) :: large')

  let support : fallback:'a -> ('a * Mass.t) list -> 'a FallbackArray.t =
   fun ~fallback measure -> FallbackArray.of_list ~fallback ~proj:fst measure

  let check_and_cleanup measure =
    let total, measure =
      List.fold_left
        (fun ((total, m) as acc) ((_, p) as point) ->
          if Mass.(zero < p) then (Mass.add total p, point :: m)
          else if Mass.(p < zero) then invalid_arg "create"
          else (* p = zero: drop point *)
            acc)
        (Mass.zero, [])
        measure
    in
    match measure with
    | [] -> invalid_arg "create"
    | (fallback, _) :: _ -> (fallback, total, measure)

  (* NB: duplicate elements in the support are not merged;
     the algorithm should still function correctly. *)
  let create (measure : ('a * Mass.t) list) =
    let fallback, total, measure = check_and_cleanup measure in
    let length = List.length measure in
    let n = Mass.of_int length in
    let small, large =
      List.fold_left_i
        (fun i (small, large) (_, p) ->
          let q = Mass.mul p n in
          if Mass.(q < total) then ((q, i) :: small, large)
          else (small, (q, i) :: large))
        ([], [])
        measure
    in
    let support = support ~fallback measure in
    let p = FallbackArray.make length Mass.zero in
    let alias = FallbackArray.make length (-1) in
    init_loop total p alias small large ;
    {total; support; p; alias}

  let sample {total; support; p; alias} draw_i_elt =
    let n = FallbackArray.length support in
    let i, elt = draw_i_elt ~int_bound:n ~mass_bound:total in
    let p = FallbackArray.get p i in
    if Mass.(elt < p) then FallbackArray.get support i
    else
      let j = FallbackArray.get alias i in
      assert (Compare.Int.(j >= 0)) ;
      FallbackArray.get support j

  (* Note: this could go in the environment maybe? *)
  let array_encoding : 'a Data_encoding.t -> 'a FallbackArray.t Data_encoding.t
      =
   fun venc ->
    let open Data_encoding in
    conv
      (fun array ->
        let length = FallbackArray.length array in
        let fallback = FallbackArray.fallback array in
        let elements =
          List.rev (FallbackArray.fold (fun acc elt -> elt :: acc) array [])
        in
        (length, fallback, elements))
      (fun (length, fallback, elements) ->
        let array = FallbackArray.make length fallback in
        List.iteri (fun i elt -> FallbackArray.set array i elt) elements ;
        array)
      (obj3
         (req "length" int31)
         (req "fallback" venc)
         (req "elements" (list venc)))

  let mass_array_encoding = array_encoding Mass.encoding

  let int_array_encoding = array_encoding Data_encoding.int31

  let encoding enc =
    let open Data_encoding in
    conv
      (fun {total; support; p; alias} -> (total, support, p, alias))
      (fun (total, support, p, alias) -> {total; support; p; alias})
      (obj4
         (req "total" Mass.encoding)
         (req "support" (array_encoding enc))
         (req "p" mass_array_encoding)
         (req "alias" int_array_encoding))

  let map f ({support; _} as sampler) =
    {sampler with support = FallbackArray.map f support}
end

module Internal_for_tests = struct
  module Make = Make

  module type SMass = SMass
end

module Mass : SMass with type t = int64 = struct
  type t = int64

  let encoding = Data_encoding.int64

  let zero = 0L

  let of_int = Int64.of_int

  let mul = Int64.mul

  let add = Int64.add

  let sub = Int64.sub

  let ( = ) = Compare.Int64.( = )

  let ( <= ) = Compare.Int64.( <= )

  let ( < ) = Compare.Int64.( < )
end

(* This is currently safe to do that since since at this point the values for
   [total] is 8 * 10^8 * 10^6 and the delegates [n] = 400.

   Therefore [let q = Mass.mul p n ...] in [create] does not overflow since p <
   total.

   Assuming the total active stake does not increase too much, which is the case
   at the current 5% inflation rate, this implementation can thus support around
   10000 delegates without overflows.

   If/when this happens, the implementation should be revisited.
*)
include Make (Mass)
