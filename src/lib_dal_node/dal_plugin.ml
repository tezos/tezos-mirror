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

type slot_header = {
  published_level : int32;
  slot_index : int;
  commitment : Tezos_crypto_dal.Cryptobox.Verifier.commitment;
}

module type T = sig
  module Proto : Registered_protocol.T

  val get_constants :
    Tezos_shell_services.Chain_services.chain ->
    Tezos_shell_services.Block_services.block ->
    Client_context.full ->
    Tezos_crypto_dal.Cryptobox.Verifier.parameters tzresult Lwt.t

  val get_published_slot_headers :
    Tezos_shell_services.Block_services.block ->
    Client_context.full ->
    slot_header list tzresult Lwt.t

  module RPC : sig
    val rpc_services :
      reveal_data_dir:string ->
      #Client_context.wallet ->
      Tezos_crypto.Aggregate_signature.public_key option list ->
      Client_keys.aggregate_sk_uri option list ->
      int ->
      unit Tezos_rpc.Directory.directory
  end
end

let table : (module T) Tezos_crypto.Protocol_hash.Table.t =
  Tezos_crypto.Protocol_hash.Table.create 5

let register (module Plugin : T) =
  assert (not (Tezos_crypto.Protocol_hash.Table.mem table Plugin.Proto.hash)) ;
  Tezos_crypto.Protocol_hash.Table.add table Plugin.Proto.hash (module Plugin)

let get hash = Tezos_crypto.Protocol_hash.Table.find table hash
