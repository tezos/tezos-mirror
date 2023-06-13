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

open Lwt_result_syntax

type endpoint = {auth : string * string; endpoint : Uri.t}

type ctx = {cohttp_ctx : Cohttp_lwt_unix.Net.ctx; endpoints : endpoint list}

type t = ctx

let name = "remote-archiver"

let extract_auth uri =
  let user = Option.value (Uri.user uri) ~default:"archiver" in
  let pass = Option.value (Uri.password uri) ~default:"secret" in
  {auth = (user, pass); endpoint = Uri.with_uri ~userinfo:None uri}

let send_something ctx path body log_msg_prefix =
  let headers =
    Cohttp.Header.init_with "content-type" "application/json; charset=UTF-8"
  in
  Lwt_list.iter_p
    (fun {auth; endpoint} ->
      let headers = Cohttp.Header.add_authorization headers (`Basic auth) in
      let uri = Uri.with_path endpoint (Uri.path endpoint ^ "/" ^ path) in
      let*! resp, out =
        Cohttp_lwt_unix.Client.post ~ctx:ctx.cohttp_ctx ~body ~headers uri
      in
      let*! out = Cohttp_lwt.Body.to_string out in
      Lwt_io.printlf
        "%s%s: %s"
        log_msg_prefix
        (Cohttp.Code.string_of_status resp.status)
        out)
    ctx.endpoints

let send_rights ctx level rights =
  let body =
    `String
      (Ezjsonm.value_to_string
         (Data_encoding.Json.construct
            Teztale_lib.Consensus_ops.rights_encoding
            rights))
  in
  let path = Int32.to_string level ^ "/rights" in
  let log_msg_prefix = Format.sprintf "level %li: " level in
  send_something ctx path body log_msg_prefix

let send_block ctx level block_data =
  let body =
    `String
      (Ezjsonm.value_to_string
         (Data_encoding.Json.construct Data.block_data_encoding block_data))
  in
  let path = Int32.to_string level ^ "/block" in
  let log_msg_prefix = Format.sprintf "level %li: " level in
  send_something ctx path body log_msg_prefix

let send_mempool ctx level ops =
  let body =
    `String
      (Ezjsonm.value_to_string
         (Data_encoding.Json.construct Consensus_ops.delegate_ops_encoding ops))
  in
  let path = Int32.to_string level ^ "/mempool" in
  let log_msg_prefix = Format.sprintf "level %li: " level in
  send_something ctx path body log_msg_prefix

type chunk =
  | Block of
      Int32.t (* level *)
      * (Data.Block.t
        * (Consensus_ops.block_op list (* endos *)
          * Consensus_ops.block_op list (* preendos *)))
  | Mempool of Int32.t (* level *) * Consensus_ops.delegate_ops
  | Rights of (Int32.t (* level *) * Consensus_ops.rights)

let chunk_stream, chunk_feeder = Lwt_stream.create ()

let launch actx _source =
  Lwt_stream.iter_p
    (function
      | Block (level, block) -> send_block actx level block
      | Mempool (level, ops) -> send_mempool actx level ops
      | Rights (level, rights) -> send_rights actx level rights)
    chunk_stream

let stop () = chunk_feeder None

let add_mempool ?unaccurate:(_ : bool option) ~level items =
  chunk_feeder (Some (Mempool (level, items)))

let add_block ~level block = chunk_feeder (Some (Block (level, block)))

let add_rights ~level rights = chunk_feeder (Some (Rights (level, rights)))
