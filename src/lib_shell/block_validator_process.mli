(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

(** Block_validator_process is used to validate new blocks. This validation can be
    - internal: the same processus is used to run the node and to validate blocks
    - external: another processus is used to validate blocks
    This module also ensures the liveness of the operations
    (see [Block_validation:check_liveness]). *)

type validator_environment = {
  user_activated_upgrades : User_activated.upgrades;
      (** user activated upgrades *)
  user_activated_protocol_overrides : User_activated.protocol_overrides;
      (** user activated protocol overrides *)
  operation_metadata_size_limit : Shell_limits.operation_metadata_size_limit;
      (** size limit for operation metadata that should be written on disk *)
}

(** For performances reasons, it may be interesting to use another processus
    (from the OS) to validate blocks (External). However, in that case, only
    one processus has a write access to the context. Currently informations
    are exchanged via the file system. *)
type validator_kind =
  | Internal : Store.Chain.chain_store -> validator_kind
  | External : {
      genesis : Genesis.t;
      readonly : bool;
      data_dir : string;
      context_root : string;
      protocol_root : string;
      process_path : string;
      sandbox_parameters : Data_encoding.json option;
      dal_config : Tezos_crypto_dal.Cryptobox.Config.t;
      log_config : External_validation.log_config;
    }
      -> validator_kind

type simple_kind = External_process | Single_process

(** Internal representation of the block validator process *)
type t

val init : validator_environment -> validator_kind -> t tzresult Lwt.t

val kind : t -> simple_kind

(** [close vp] closes the given [vp]. In the case of an [External]
   validator process, we first ask the validator to shutdown. If it is
   still running after 5 seconds, we notice that the block validation
   process is unresponsive and we force its termination (using a
   registered Lwt_exit.clean_up_callback). *)
val close : t -> unit Lwt.t

val reconfigure_event_logging :
  t -> Internal_event_unix.Configuration.t -> unit tzresult Lwt.t

(** [apply_block bvp predecessor header os] checks the liveness of the
    operations and then call [Block_validation.apply]

    [should_precheck] when set, triggers the block prechecking before applying
    it, see [Block_validation.apply].

    If [simulate] is true, the context resulting from the application
    will not be committed to disk. Set to false by default.
*)
val apply_block :
  ?simulate:bool ->
  ?should_precheck:bool ->
  t ->
  Store.chain_store ->
  predecessor:Store.Block.t ->
  Block_header.t ->
  Operation.t list list ->
  Block_validation.result tzresult Lwt.t

val preapply_block :
  t ->
  Store.chain_store ->
  predecessor:Store.Block.t ->
  protocol_data:bytes ->
  timestamp:Time.Protocol.t ->
  Operation.t list list ->
  (Block_header.shell_header * error Preapply_result.t list) tzresult Lwt.t

(** [precheck_block bvp chain_store ~predecessor header hash os] is a
   wrapper for [Block_validation.precheck]. *)
val precheck_block :
  t ->
  Store.chain_store ->
  predecessor:Store.Block.t ->
  Block_header.t ->
  Block_hash.t ->
  Operation.t trace trace ->
  unit tzresult Lwt.t

(** [context_garbage_collection bvp context_index context_hash]
    removes contexts that are below [context_hash] in the context
    tree. *)
val context_garbage_collection :
  t ->
  Context_ops.index ->
  Context_hash.t ->
  gc_lockfile_path:string ->
  unit tzresult Lwt.t

(** [context_split bvp context_index] splits the context storage
    layout into a new chunk. *)
val context_split : t -> Context_ops.index -> unit tzresult Lwt.t

val commit_genesis : t -> chain_id:Chain_id.t -> Context_hash.t tzresult Lwt.t

(** [init_test_chain] must only be called on a forking block. *)
val init_test_chain :
  t -> Chain_id.t -> Store.Block.t -> Block_header.t tzresult Lwt.t
