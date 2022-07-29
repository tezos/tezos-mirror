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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/3207
   use another storage solution that irmin as we don't need backtracking *)

(* Relative path to store directory from base-dir *)
let path = "store"

module StoreMaker = Irmin_pack_unix.KV (Tezos_context_encoding.Context.Conf)
include StoreMaker.Make (Irmin.Contents.String)

let info message =
  let date = Unix.gettimeofday () |> int_of_float |> Int64.of_int in
  Irmin.Info.Default.v ~author:"DAL Node" ~message date

let set ~msg store path v = set_exn store path v ~info:(fun () -> info msg)

let init config =
  let open Lwt_syntax in
  let dir = Configuration.data_dir_path config path in
  let* repo = Repo.v (Irmin_pack.config dir) in
  let* store = main repo in
  let* () = Event.(emit store_is_ready ()) in
  Lwt.return store
