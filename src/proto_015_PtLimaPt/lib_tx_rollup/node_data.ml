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

open Filename.Infix

type t = string

let store_dir data_dir = data_dir // "store"

let context_dir data_dir = data_dir // "context"

let l2blocks_index data_dir = store_dir data_dir // "l2blocks_index"

let l2blocks_data data_dir = store_dir data_dir // "l2blocks_data"

let tezos_blocks_index data_dir = store_dir data_dir // "tezos_blocks_index"

let commitments_index data_dir = store_dir data_dir // "commitments_index"

let levels_index data_dir = store_dir data_dir // "levels_index"

let head_file data_dir = store_dir data_dir // "head"

let mkdir ?(perm = 0o777) dir =
  let open Lwt_syntax in
  let* b = Lwt_unix.file_exists dir in
  match b with false -> Lwt_unix.mkdir dir perm | true -> Lwt.return_unit

let mk_store_dir ?perm data_dir = mkdir ?perm (store_dir data_dir)

let mk_context_dir ?perm data_dir = mkdir ?perm (context_dir data_dir)
