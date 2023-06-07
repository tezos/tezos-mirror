(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module type S = Traced_sigs.Trace.S

module type EXTENDED = sig
  include S

  val pp :
    (Format.formatter -> 'err -> unit) -> Format.formatter -> 'err trace -> unit

  val pp_top :
    (Format.formatter -> 'err -> unit) -> Format.formatter -> 'err trace -> unit

  val fold : ('a -> 'error -> 'a) -> 'a -> 'error trace -> 'a

  val salvage :
    ('error -> 'a option) -> 'error trace -> ('a, 'error trace) result

  val salvage_s :
    ('error -> 'a Lwt.t option) ->
    'error trace ->
    ('a, 'error trace) result Lwt.t

  val salvage_e :
    ('error -> ('a, 'error trace) result option) ->
    'error trace ->
    ('a, 'error trace) result

  val salvage_es :
    ('error -> ('a, 'error trace) result Lwt.t option) ->
    'error trace ->
    ('a, 'error trace) result Lwt.t

  val recover :
    ('error -> 'a option) -> ('error trace -> 'a) -> 'error trace -> 'a

  val recover_s :
    ('error -> 'a Lwt.t option) ->
    ('error trace -> 'a Lwt.t) ->
    'error trace ->
    'a Lwt.t

  val recover_e :
    ('error -> ('a, 'error trace) result option) ->
    ('error trace -> ('a, 'error trace) result) ->
    'error trace ->
    ('a, 'error trace) result

  val recover_es :
    ('error -> ('a, 'error trace) result Lwt.t option) ->
    ('error trace -> ('a, 'error trace) result Lwt.t) ->
    'error trace ->
    ('a, 'error trace) result Lwt.t

  val wrap : ('a -> 'b) -> 'a trace -> 'b trace
end

(** [Singleton] is a trace implementation where a trace carries exactly one
    error. Additional information is discarded. The different variant discard
    additional information in different unspecified way. This is useful for
    testing purpose if you need to check that you do not depend on some
    unspecified behaviour. [SingletonND] is even non-deterministic. *)
module SingletonR : EXTENDED

module SingletonL : EXTENDED

module SingletonND : EXTENDED

(** [Flat] is a trace implementation where a trace carries a flat collection of
    errors. No error is discarded, but the structure (parallel vs sequential)
    is. *)
module Flat : EXTENDED

(** [Full] is a trace implementation where a trace carries a structured
    collection of errors. No error is discarded, nor is the structure. *)
module Full : EXTENDED

(** [Unit] is a trivial implementation where a trace carries no information
    at all. This is useful for testing. *)
module Unit : S with type 'a trace = unit
