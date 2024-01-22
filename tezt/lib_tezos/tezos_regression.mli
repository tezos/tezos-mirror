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

(** Tezos-specific extension for the [Regression] module. *)

(** [replace_variables log] returns [log] with all occurrences of variables
    such as contract addresses, hashes etc. that may change between different
    runs replaced by constants. *)
val replace_variables : string -> string

(** Custom Tezos-specific regression hooks that replaces Tezos-specific values
    with constants.

    [scrubbed_global_options] list of global options to scrub. Default: remove
    global options [--base-dir], [-d], [--endpoint], [-E], and [--sources].

    [replace_variables] is applied to the output and to the argument of each
    global option not in [scrubbed_global_options]. Defaults to
    {!replace_variables} that replaces key hashes, public keys, contract hashes,
    and timestamps. *)
val hooks_custom :
  ?scrubbed_global_options:string list ->
  ?replace_variables:(string -> string) ->
  unit ->
  Process.hooks

(** Hooks that replaces Tezos-specific values with constants.

    This is {!hooks_custom} with the default arguments. *)
val hooks : Process.hooks

(** PRC_hooks that replaces Tezos-specific values with constants and calls
    Regression.capture on request and response. *)
val rpc_hooks : RPC_core.rpc_hooks
