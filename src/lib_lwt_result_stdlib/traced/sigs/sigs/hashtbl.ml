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
  include Bare_sigs_sigs.Hashtbl.S

  type 'error trace

  val iter_ep :
    (key -> 'a -> (unit, 'error trace) result Lwt.t) ->
    'a t ->
    (unit, 'error trace) result Lwt.t
end

module type SeededS = sig
  include Bare_sigs_sigs.Hashtbl.SeededS

  type 'error trace

  val iter_ep :
    (key -> 'a -> (unit, 'error trace) result Lwt.t) ->
    'a t ->
    (unit, 'error trace) result Lwt.t
end

module type S_LWT = sig
  include Bare_sigs_sigs.Hashtbl.S_LWT

  type 'error trace

  val iter_with_waiting_ep :
    (key -> 'a -> (unit, 'error trace) result Lwt.t) ->
    ('a, 'error trace) t ->
    (unit, 'error trace) result Lwt.t
end
