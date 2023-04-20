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
  type elt

  type t

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val add : elt -> t -> t

  val singleton : elt -> t

  val remove : elt -> t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val disjoint : t -> t -> bool

  val diff : t -> t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val subset : t -> t -> bool

  val iter : (elt -> unit) -> t -> unit

  val iter_e : (elt -> (unit, 'trace) result) -> t -> (unit, 'trace) result

  val iter_s : (elt -> unit Lwt.t) -> t -> unit Lwt.t

  val iter_p : (elt -> unit Lwt.t) -> t -> unit Lwt.t

  val iter_es :
    (elt -> (unit, 'trace) result Lwt.t) -> t -> (unit, 'trace) result Lwt.t

  val map : (elt -> elt) -> t -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val fold_e :
    (elt -> 'a -> ('a, 'trace) result) -> t -> 'a -> ('a, 'trace) result

  val fold_s : (elt -> 'a -> 'a Lwt.t) -> t -> 'a -> 'a Lwt.t

  val fold_es :
    (elt -> 'a -> ('a, 'trace) result Lwt.t) ->
    t ->
    'a ->
    ('a, 'trace) result Lwt.t

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val filter : (elt -> bool) -> t -> t

  val filter_map : (elt -> elt option) -> t -> t

  val partition : (elt -> bool) -> t -> t * t

  val cardinal : t -> int

  val elements : t -> elt list

  val min_elt : t -> elt option

  val min_elt_opt : t -> elt option

  val max_elt : t -> elt option

  val max_elt_opt : t -> elt option

  val choose : t -> elt option

  val choose_opt : t -> elt option

  val split : elt -> t -> t * bool * t

  val find : elt -> t -> elt option

  val find_opt : elt -> t -> elt option

  val find_first : (elt -> bool) -> t -> elt option

  val find_first_opt : (elt -> bool) -> t -> elt option

  val find_last : (elt -> bool) -> t -> elt option

  val find_last_opt : (elt -> bool) -> t -> elt option

  val of_list : elt list -> t

  val to_seq_from : elt -> t -> elt Seq.t

  val to_seq : t -> elt Seq.t

  val to_rev_seq : t -> elt Seq.t

  val add_seq : elt Seq.t -> t -> t

  val of_seq : elt Seq.t -> t

  val iter_ep :
    (elt -> (unit, 'error Error_monad.trace) result Lwt.t) ->
    t ->
    (unit, 'error Error_monad.trace) result Lwt.t
end

module Make (Ord : Compare.COMPARABLE) : S with type elt = Ord.t
