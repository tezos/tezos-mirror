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

(** Modules with the [S] signature are used to instantiate the other modules of
    this library. [S] describes a generic Lwt-Result combined monad, the rest of
    this library builds upon. *)
module type S = sig
  include Bare_sigs.Monad.S

  type 'error trace

  val error_trace : 'error -> ('a, 'error trace) result

  val fail_trace : 'error -> ('a, 'error trace) result Lwt.t

  val join_e : (unit, 'error trace) result list -> (unit, 'error trace) result

  val all_e : ('a, 'error trace) result list -> ('a list, 'error trace) result

  val both_e :
    ('a, 'error trace) result ->
    ('b, 'error trace) result ->
    ('a * 'b, 'error trace) result

  val join_ep :
    (unit, 'error trace) result Lwt.t list -> (unit, 'error trace) result Lwt.t

  val all_ep :
    ('a, 'error trace) result Lwt.t list ->
    ('a list, 'error trace) result Lwt.t

  val both_ep :
    ('a, 'error trace) result Lwt.t ->
    ('b, 'error trace) result Lwt.t ->
    ('a * 'b, 'error trace) result Lwt.t
end
