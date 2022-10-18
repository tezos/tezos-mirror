(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module provides basic support for reveals.

    The rollup can ask for data being the reveal of some hash. This
    allows transferring data directly to the rollup without going
    through the L1 inbox.

    Data length must be under 4KB to be refutable in a single L1
    operation.

    Data must be made available by off-chain mechanisms: it is the
    responsibility of the rollup kernel to make sure that the reveal
    data is available: otherwise, there is a potential safety issue.

    For the moment, the support is basic and mostly manual as the operator
    needs to explicitly import a file in the rollup node data directoy to
    enable the rollup node to answer reveal requests.

*)

(* FIXME:https://gitlab.com/tezos/tezos/-/issues/3854

   We should probably have a mechanism to let the kernel declare
   sources of reveal data so that the rollup node can automatically
   download data in advance. *)

open Protocol.Alpha_context

(** [get ~data_dir ~pvm_name ~hash] returns [Some data] such that
    [Input_hash.hash_string [data] = hash]. If such [data] is known
    to the rollup node. Otherwise, returns [None]. *)
val get :
  data_dir:string ->
  pvm_name:string ->
  hash:Sc_rollup.Reveal_hash.t ->
  string option

(** [import ~data_dir ~pvm_name ~filename] turns the content of ~filename
    into a chunk of pages of (at most) 4KB, returning the hash of the first
    chunk. *)
val import :
  data_dir:string ->
  pvm_name:string ->
  filename:string ->
  Sc_rollup.Reveal_hash.t
