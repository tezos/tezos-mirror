(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** Storage reconstruction

    The storage reconstruction feature aims to re-compute the contexts
    (ledger state) and the blocks metadata of a full mode storage, and
    thus, migrate a storage from a full history mode to an archive
    one.

    To do so, it is needed to re-validate the whole chain, by applying
    (using the standard validation method:
    {!Tezos_validation.Block_validation.apply}) all the blocks from the
    genesis on empty context. As a storage running a full history mode
    will not store all the ledger state but keeps all the blocks (and
    operations), it is the only mode that can be reconstructed. The
    operation consist of two major steps:
    - Reconstructing the cemented block store: for each cemented cycle,
    the context of each block along with their associated metadatas
    are restored.
    - Reconstructing the floating block stores: this step is only
    necessary if the store was recently imported from a snapshot as
    some metadata will be missing.

    As the reconstruction procedure changes the state of the storage,
    it cannot be run while a node is running on the storage to
    reconstruct. If the reconstruction is interrupted, it will be
    resumed if restarted.

*)

type failure_kind =
  | Nothing_to_reconstruct
  | Context_hash_mismatch of Block_header.t * Context_hash.t * Context_hash.t
  | Cannot_read_block_hash of Block_hash.t
  | Cannot_read_block_level of Int32.t

type error += Reconstruction_failure of failure_kind

type error += Cannot_reconstruct of History_mode.t

(** [reconstruct ?patch_context ~store_dir ~context_dir genesis uau
    uapo omsl] reconstructs the storage located in [store_dir] and
    [context_dir]. The resulting storage will see its history mode
    changed to archive. *)
val reconstruct :
  ?patch_context:(Context.t -> Context.t tzresult Lwt.t) ->
  store_dir:string ->
  context_dir:string ->
  Genesis.t ->
  user_activated_upgrades:User_activated.upgrades ->
  user_activated_protocol_overrides:User_activated.protocol_overrides ->
  operation_metadata_size_limit:int option ->
  unit tzresult Lwt.t
