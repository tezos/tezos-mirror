(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* From Lwtreslib *)

module type S = sig
  type key

  type +!'a t

  val empty : 'a t

  val is_empty : 'a t -> bool

  val mem : key -> 'a t -> bool

  val add : key -> 'a -> 'a t -> 'a t

  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t

  val singleton : key -> 'a -> 'a t

  val remove : key -> 'a t -> 'a t

  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

  val iter : (key -> 'a -> unit) -> 'a t -> unit

  (** [iter_e f m] applies [f] to the bindings of [m] one by one in an
      unspecified order. If all the applications result in [Ok ()], then the
      result of the iteration is [Ok ()]. If any of the applications results in
      [Error e] then the iteration stops and the result of the iteration is
      [Error e]. *)
  val iter_e :
    (key -> 'a -> (unit, 'trace) result) -> 'a t -> (unit, 'trace) result

  val iter_s : (key -> 'a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  val iter_p : (key -> 'a -> unit Lwt.t) -> 'a t -> unit Lwt.t

  (** [iter_es f m] applies [f] to the bindings of [m] in an unspecified order,
      one after the other as the promises resolve. If all the applications
      result in [Ok ()], then the result of the iteration is [Ok ()]. If any of
      the applications results in [Error e] then the iteration stops and the
      result of the iteration is [Error e]. *)
  val iter_es :
    (key -> 'a -> (unit, 'trace) result Lwt.t) ->
    'a t ->
    (unit, 'trace) result Lwt.t

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** [fold_e f m init] is
      [f k1 d1 init >>? fun acc -> f k2 d2 acc >>? fun acc -> …] where [kN] is
      the key bound to [dN] in [m]. *)
  val fold_e :
    (key -> 'a -> 'b -> ('b, 'trace) result) ->
    'a t ->
    'b ->
    ('b, 'trace) result

  val fold_s : (key -> 'a -> 'b -> 'b Lwt.t) -> 'a t -> 'b -> 'b Lwt.t

  (** [fold_es f m init] is
      [f k1 d1 init >>=? fun acc -> f k2 d2 acc >>=? fun acc -> …] where [kN] is
      the key bound to [dN] in [m]. *)
  val fold_es :
    (key -> 'a -> 'b -> ('b, 'trace) result Lwt.t) ->
    'a t ->
    'b ->
    ('b, 'trace) result Lwt.t

  val for_all : (key -> 'a -> bool) -> 'a t -> bool

  val exists : (key -> 'a -> bool) -> 'a t -> bool

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t

  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t

  val cardinal : 'a t -> int

  val bindings : 'a t -> (key * 'a) list

  val min_binding : 'a t -> (key * 'a) option

  val min_binding_opt : 'a t -> (key * 'a) option

  val max_binding : 'a t -> (key * 'a) option

  val max_binding_opt : 'a t -> (key * 'a) option

  val choose : 'a t -> (key * 'a) option

  val choose_opt : 'a t -> (key * 'a) option

  val split : key -> 'a t -> 'a t * 'a option * 'a t

  val find : key -> 'a t -> 'a option

  val find_opt : key -> 'a t -> 'a option

  val find_first : (key -> bool) -> 'a t -> (key * 'a) option

  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option

  val find_last : (key -> bool) -> 'a t -> (key * 'a) option

  val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option

  val map : ('a -> 'b) -> 'a t -> 'b t

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t

  val to_seq : 'a t -> (key * 'a) Seq.t

  val to_rev_seq : 'a t -> (key * 'a) Seq.t

  val to_seq_from : key -> 'a t -> (key * 'a) Seq.t

  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t

  val of_seq : (key * 'a) Seq.t -> 'a t

  val iter_ep :
    (key -> 'a -> (unit, 'error Error_monad.trace) result Lwt.t) ->
    'a t ->
    (unit, 'error Error_monad.trace) result Lwt.t

end

module Make (Ord : Compare.COMPARABLE) : S with type key = Ord.t
