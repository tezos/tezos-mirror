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

(** Hashtbls with the signature [S] are safe (e.g., [find] uses [option] rather
    than raising [Not_found]) extensions of [Hashtbl.S] with some Lwt- and
    Error-aware traversal functions. *)
module type S = sig
  include Bare_sigs.Hashtbl.S

  type 'error trace

  val iter_ep :
    (key -> 'a -> (unit, 'error trace) result Lwt.t) ->
    'a t ->
    (unit, 'error trace) result Lwt.t
end

module type SeededS = sig
  include Bare_sigs.Hashtbl.SeededS

  type 'error trace

  val iter_ep :
    (key -> 'a -> (unit, 'error trace) result Lwt.t) ->
    'a t ->
    (unit, 'error trace) result Lwt.t
end

module type S_LWT = sig
  include Bare_sigs.Hashtbl.S_LWT

  type 'error trace

  (** [iter_with_waiting_ep f tbl] iterates [f] over the bindings in [tbl].

      Specifically, for each binding [(k, p)] it waits for [p] to be fulfilled
      with [Ok v] and calls [f k v]. If [p] fulfills with [Error _] or is
      rejected, then no call is made for this binding.

      Note however that if one (or more) of the promises returned by [f] ends in
      [Error]/rejection, the final result of this promise is an
      [Error]/rejection. Even so, it only resolves once all the promises have.

      It processes all bindings concurrently: it concurrently waits for all the
      bound promises to resolve and calls [f] as they resolve. *)
  val iter_with_waiting_ep :
    (key -> 'a -> (unit, 'error trace) result Lwt.t) ->
    ('a, 'error trace) t ->
    (unit, 'error trace) result Lwt.t
end

module type LWTRESLIB_TRACED_HASHTBL_S = sig
  type 'error trace

  val hash : 'a -> int

  val seeded_hash : int -> 'a -> int

  val hash_param : meaningful:int -> total:int -> 'a -> int

  val seeded_hash_param : meaningful:int -> total:int -> int -> 'a -> int

  module type S = S with type 'error trace := 'error trace

  module Make (H : Stdlib.Hashtbl.HashedType) : S with type key = H.t

  module type SeededS = SeededS with type 'error trace := 'error trace

  module MakeSeeded (H : Stdlib.Hashtbl.SeededHashedType) :
    SeededS with type key = H.t

  module type S_LWT = S_LWT with type 'error trace := 'error trace

  module Make_Lwt (H : Stdlib.Hashtbl.HashedType) : S_LWT with type key = H.t
end
