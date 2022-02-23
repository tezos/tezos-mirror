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

(** [S] is the signature for an exception-safe replacements for {!Stdlib.List}
    with Lwt- and result-aware traversal functions.

    See {!Lwtreslib}'s introductory documentation for explanations regarding
    [_e]-, [_s]-, [_es]-, [_p]-, and [_ep]-suffixed functions and exception
    safety. See {!Stdlib.Hashtbl.S} for explanations regarding OCaml's
    hashtables in general.

    Note that this signature is within the Traced part of the library. As a
    result, the [_ep] traversor returns en ['error trace]. *)
module type S = sig
  include Bare_sigs.List.S

  (** ['error trace] is intended to be substituted by a type provided by a
      [Trace] module ([with type 'error trace := 'error Trace.trace]) *)
  type 'error trace

  val init_ep :
    when_negative_length:'error ->
    int ->
    (int -> ('a, 'error trace) result Lwt.t) ->
    ('a list, 'error trace) result Lwt.t

  val filter_ep :
    ('a -> (bool, 'error trace) result Lwt.t) ->
    'a list ->
    ('a list, 'error trace) result Lwt.t

  val partition_ep :
    ('a -> (bool, 'error trace) result Lwt.t) ->
    'a list ->
    ('a list * 'a list, 'error trace) result Lwt.t

  val iter_ep :
    ('a -> (unit, 'error trace) result Lwt.t) ->
    'a list ->
    (unit, 'error trace) result Lwt.t

  val iteri_ep :
    (int -> 'a -> (unit, 'error trace) result Lwt.t) ->
    'a list ->
    (unit, 'error trace) result Lwt.t

  val map_ep :
    ('a -> ('b, 'error trace) result Lwt.t) ->
    'a list ->
    ('b list, 'error trace) result Lwt.t

  val mapi_ep :
    (int -> 'a -> ('b, 'error trace) result Lwt.t) ->
    'a list ->
    ('b list, 'error trace) result Lwt.t

  val rev_map_ep :
    ('a -> ('b, 'error trace) result Lwt.t) ->
    'a list ->
    ('b list, 'error trace) result Lwt.t

  val rev_mapi_ep :
    (int -> 'a -> ('b, 'error trace) result Lwt.t) ->
    'a list ->
    ('b list, 'error trace) result Lwt.t

  val filter_map_ep :
    ('a -> ('b option, 'error trace) result Lwt.t) ->
    'a list ->
    ('b list, 'error trace) result Lwt.t

  val concat_map_ep :
    ('a -> ('b list, 'error trace) result Lwt.t) ->
    'a list ->
    ('b list, 'error trace) result Lwt.t

  val for_all_ep :
    ('a -> (bool, 'error trace) result Lwt.t) ->
    'a list ->
    (bool, 'error trace) result Lwt.t

  val exists_ep :
    ('a -> (bool, 'error trace) result Lwt.t) ->
    'a list ->
    (bool, 'error trace) result Lwt.t
end
