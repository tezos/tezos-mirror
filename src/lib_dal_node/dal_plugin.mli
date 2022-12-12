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

type operation_application_result =
  | Succeeded
      (** The associated operation is included in a block and successfully
          applied. *)
  | Failed
      (** The associated operation is included in a block but its application was
          not successful (failed, backtracked or skipped). *)

type slot_index = int

(** Information extracted from DAL slots headers operations included in L1
    blocks. Each slot header is made of an L1 level for which it is published,
    the slot's index and commitment. *)
type slot_header = {
  published_level : int32;
  slot_index : slot_index;
  commitment : Tezos_crypto_dal.Cryptobox.Verifier.commitment;
}

type proto_parameters = {
  feature_enable : bool;
  number_of_slots : int;
  attestation_lag : int;
  availability_threshold : int;
  cryptobox_parameters : Tezos_crypto_dal.Cryptobox.Verifier.parameters;
  blocks_per_epoch : int32;
}

module type T = sig
  module Proto : Registered_protocol.T

  type block_info

  (** [block_info ?chain ?block ~metadata ctxt] returns the information of the
      [block] in [ctxt] for the given [chain]. Block's metadata are included or
      skipped depending on the value of [metadata]. This is a wrapper on top of
      {!Protocol_client_context.Alpha_block_services.info}.  *)
  val block_info :
    ?chain:Tezos_shell_services.Block_services.chain ->
    ?block:Tezos_shell_services.Block_services.block ->
    metadata:[`Always | `Never] ->
    Client_context.full ->
    block_info tzresult Lwt.t

  val get_constants :
    Tezos_shell_services.Chain_services.chain ->
    Tezos_shell_services.Block_services.block ->
    Client_context.full ->
    proto_parameters tzresult Lwt.t

  val get_published_slot_headers :
    block_info ->
    (slot_header * operation_application_result) list tzresult Lwt.t

  (** [slot_headers_attestation hash block_info number_of_slots] reads the
      metadata of the given [block_info] and constructs two lists of attested
      and unattested slots depending on the value of dal_attestation bitset.
      The value of [number_of_slots] indicates the current maximum number of
      slots on DAL per level.

      Fails with [Cannot_read_block_metadata] if [block_info]'s metadata are
      stripped.  *)
  val slot_headers_attestation :
    Tezos_crypto.Block_hash.t ->
    block_info ->
    number_of_slots:int ->
    ([`Attested of slot_index list] * [`Unattested of slot_index list]) tzresult
    Lwt.t

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

val register : (module T) -> unit

val get : Tezos_crypto.Protocol_hash.Table.key -> (module T) option
