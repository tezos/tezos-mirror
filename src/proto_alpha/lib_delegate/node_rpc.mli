(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol
open Alpha_context

(** Inject a block.

    @param force defaults to [false]
    @return block hash of the newly injected block
*)
val inject_block :
  #Protocol_client_context.full ->
  ?force:bool ->
  chain:Shell_services.chain ->
  Block_header.t ->
  Tezos_base.Operation.t list list ->
  Block_hash.t tzresult Lwt.t

(** Inject an operation.

    @return operation hash of the newly injected operation
*)
val inject_operation :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  packed_operation ->
  Operation_hash.t tzresult Lwt.t

(** Preapply a block using the node validation mechanism.*)
val preapply_block :
  #Protocol_client_context.full ->
  chain:Shell_services.chain ->
  head:Block_hash.t ->
  timestamp:Time.Protocol.t ->
  protocol_data:Protocol.block_header_data ->
  packed_operation list list ->
  (Tezos_base.Block_header.shell_header * error Preapply_result.t list) tzresult
  Lwt.t

(** Monitor validated blocks/proposals from the node. *)
val monitor_valid_proposals :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  ?cache:Baking_state.block_info Baking_cache.Block_cache.t ->
  unit ->
  (Baking_state.proposal Lwt_stream.t * (unit -> unit)) tzresult Lwt.t

(** Monitor heads from the node. *)
val monitor_heads :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  ?cache:Baking_state.block_info Baking_cache.Block_cache.t ->
  unit ->
  (Baking_state.proposal Lwt_stream.t * (unit -> unit)) tzresult Lwt.t

(** Await the current protocol to be activated. *)
val await_protocol_activation :
  #Protocol_client_context.rpc_context ->
  chain:Shell_services.chain ->
  unit ->
  unit tzresult Lwt.t

val fetch_dal_config :
  #Protocol_client_context.rpc_context -> Cryptobox.Config.t tzresult Lwt.t

(** [dal_attestable_slots ctxt ~attestation_level delegates_slots] calls the DAL
    node RPC GET /profiles/<pkh>/attested_levels/<level>/attestable_slots/<pkh>
    for each of the delegates in [delegate_slots] and returns the corresponding
    promises. *)
val dal_attestable_slots :
  Tezos_rpc.Context.generic ->
  attestation_level:int32 ->
  Baking_state.delegate_slot list ->
  Baking_state.dal_attestable_slots

(** [get_dal_profiles ctxt delegates] calls the DAL node RPC GET
    /profiles/ to retrieve the DAL node's profiles. *)
val get_dal_profiles :
  Tezos_rpc.Context.generic ->
  Tezos_dal_node_services.Types.profile tzresult Lwt.t

(** [register_dal_profiles ctxt delegates] calls the DAL node RPC PATCH
    /profiles/ to register each profile corresponding to a delegate in
    [delegates]. *)
val register_dal_profiles :
  Tezos_rpc.Context.generic ->
  Signature.Public_key_hash.t list ->
  unit tzresult Lwt.t

(** [get_dal_health ctxt] calls the DAL node RPC 'GET /health' *)
val get_dal_health :
  Tezos_rpc.Context.generic ->
  Tezos_dal_node_services.Types.Health.t tzresult Lwt.t
