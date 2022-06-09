(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** We have to duplicate the content of
    [src/lib_protocol_environment/sigs/v3/set.mli] (and a few other signatures
    from the same directory) so we can refer to them in
    [src/lib_protocol_environment/environment_V3.ml]. Indeed, it is impossible
    to refer to components of [Tezos_protocol_environment_sigs.V3.T] (such as
    the [Set] component) because [T] is a module type (not a module).

    It might be possible to change the way that the signatures are exported and
    thus allow access to some of the sub-components, at which point this whole
    file can be removed. Note, however, that some components make reference to
    others (e.g., here we need to introduce the type [error_monad_trace] which
    we then substitute for [Error_monad.trace]) so simply exporting the
    components will not suffice because there will be unbound types.

    Ultimately, it might be easier to provide each struct as a partial or full
    set of legacy libraries which used to be present at the time the environment
    was made. For the signatures below, we would include a full implementation
    of [Lwtreslib] as it appeared when the environment v3 was cut. *)

module Compare = struct
  module type COMPARABLE = sig
    type t

    val compare : t -> t -> int
  end
end

module Set = struct
  module type S = sig
    type 'a error_monad_trace (* to be substituted *)

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

    val partition : (elt -> bool) -> t -> t * t

    val cardinal : t -> int

    val elements : t -> elt list

    val min_elt : t -> elt option

    val max_elt : t -> elt option

    val choose : t -> elt option

    val split : elt -> t -> t * bool * t

    val find : elt -> t -> elt option

    val find_first : (elt -> bool) -> t -> elt option

    val find_last : (elt -> bool) -> t -> elt option

    val of_list : elt list -> t

    val to_seq_from : elt -> t -> elt Stdlib.Seq.t

    val to_seq : t -> elt Stdlib.Seq.t

    val add_seq : elt Stdlib.Seq.t -> t -> t

    val of_seq : elt Stdlib.Seq.t -> t

    val iter_ep :
      (elt -> (unit, 'error error_monad_trace) result Lwt.t) ->
      t ->
      (unit, 'error error_monad_trace) result Lwt.t
  end
end

module Map = struct
  module type S = sig
    type 'a error_monad_trace (* for substitution *)

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

    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t

    val cardinal : 'a t -> int

    val bindings : 'a t -> (key * 'a) list

    val min_binding : 'a t -> (key * 'a) option

    val max_binding : 'a t -> (key * 'a) option

    val choose : 'a t -> (key * 'a) option

    val split : key -> 'a t -> 'a t * 'a option * 'a t

    val find : key -> 'a t -> 'a option

    val find_first : (key -> bool) -> 'a t -> (key * 'a) option

    val find_last : (key -> bool) -> 'a t -> (key * 'a) option

    val map : ('a -> 'b) -> 'a t -> 'b t

    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t

    val to_seq : 'a t -> (key * 'a) Seq.t

    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t

    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t

    val of_seq : (key * 'a) Seq.t -> 'a t

    val iter_ep :
      (key -> 'a -> (unit, 'error error_monad_trace) result Lwt.t) ->
      'a t ->
      (unit, 'error error_monad_trace) result Lwt.t
  end
end
