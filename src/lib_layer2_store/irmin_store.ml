(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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
module Make (N : sig
  val name : string
end) =
struct
  module Maker = Irmin_pack_unix.Maker (Tezos_context_encoding.Context.Conf)
  include Maker.Make (Tezos_context_encoding.Context.Schema)
  module Schema = Tezos_context_encoding.Context.Schema

  let make_key_path path key = path @ [key]

  let load data_dir =
    let open Lwt_syntax in
    let* repo = Repo.v (Irmin_pack.config data_dir) in
    main repo

  let flush store = flush (repo store)

  let close store = Repo.close (repo store)

  let info message =
    let date =
      Tezos_base.Time.(
        System.now () |> System.to_protocol |> Protocol.to_seconds)
    in
    Irmin.Info.Default.v ~author:N.name ~message date

  let path_to_string path = String.concat "/" path

  let set_exn store path bytes =
    let full_path = path_to_string path in
    let info () = info full_path in
    set_exn ~info store path bytes
end
