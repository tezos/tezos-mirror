(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

(** A map addressed by (protocol agnostic) DAC reveal hashes. *)
module Reveal_hash_map : Map.S with type key = Dac_plugin.hash

(** [dictionary_encoding keys string_of_key key_of_string
    value_encoding]: a json only encoding of a [(key, 'value)
    list]. JSON is of the form
{v
{ "k1" : v1,
  "k2" : v2,
  "k3" : v3 }
v} *)
val dictionary_encoding :
  keys:'k list ->
  string_of_key:('k -> string) ->
  key_of_string:(string -> 'k) ->
  value_encoding:('k -> 'v Data_encoding.t) ->
  ('k * 'v) list Data_encoding.t

(** {2 Lock files}  *)

(** [lock path] acquires a lock on the file [path] and returns the opened file
    descriptor (for unlocking). If there is already a lock on [path], this
    function call is blocking until the previous lock is released.  *)
val lock : string -> Lwt_unix.file_descr tzresult Lwt.t

(** [unlock fd] releases the lock on the opened file descriptor [fd]. If there
    is no lock or if it is already released, this function does nothing. *)
val unlock : Lwt_unix.file_descr -> unit Lwt.t

(** [with_lockfile path f] executes the function [f] by taking a lock on the
    file [path]. If there is already a lock on [path], the execution of [f] is
    blocking until the previous lock is released. *)
val with_lockfile : string -> (unit -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t
