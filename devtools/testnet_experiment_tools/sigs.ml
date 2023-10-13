(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module type PROTO_TOOL = sig
  val extract_client_context : Client_context.full -> unit tzresult Lwt.t

  val start_injector :
    Client_context.full ->
    op_per_mempool:int ->
    min_manager_queues:int ->
    operations_file_path:string ->
    unit tzresult Lwt.t

  val sync_node :
    Client_context.full ->
    ?round_duration_target:int ->
    unit ->
    unit tzresult Lwt.t

  val patch_block_time :
    Tezos_protocol_environment.Context.t ->
    head_level:int32 ->
    block_time_target:int ->
    Tezos_protocol_environment.Context.t tzresult Lwt.t
end

let all : (module PROTO_TOOL) Protocol_hash.Map.t ref =
  ref Protocol_hash.Map.empty

let register proto t = all := Protocol_hash.Map.add proto t !all
