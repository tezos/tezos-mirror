(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Legacy storage upgrade

    The legacy store upgrade aims to migrate a storage using the LMDB
    backend (v0.0.4) to the new store representation (v0.0.5). This
    migration is available for any store running v0.0.4 with any
    history mode. The upgrade procedure is going to retrieve each the
    block and its associated data available in the old store, convert
    and store them in the new backend. It will preserve all the
    information originally contained in the LMDB store such as the
    current head, checkpoint, savepoint and caboose. *)

(** Module for handling values needed by the legacy upgrade. It
    exposes the [cycle_length] to allow upgrading [supported_networks]
    only. These values are mandatory as the new backend regroup blocks
    by cycles, something which was not available in the previous
    representation when the metadata of blocks is not fully available.
*)
module Hardcoded : sig
  type network = {name : Distributed_db_version.Name.t; cycle_length : int}

  val supported_networks : network list

  val cycle_length : chain_name:Distributed_db_version.Name.t -> int

  val check_network :
    chain_name:Distributed_db_version.Name.t -> unit tzresult Lwt.t
end

type error += Failed_to_convert_protocol of Protocol_hash.t

type error += Failed_to_upgrade of string

(** [temporary_former_store_path ~data_dir] returns the path of the
    preserved legacy store given a [data_dir] *)
val temporary_former_store_path : data_dir:string -> string

(** [raw_upgrade chain_name ~new_store ~old_store hm genesis] is the
    low level upgrade procedure which performs the store upgrade given
    the direct paths to the [new_store] and [old_store].

    {b Warning} This function is unsafe and is exposed for testing
    purposes. *)
val raw_upgrade :
  Distributed_db_version.Name.t ->
  new_store:Store.t ->
  legacy_state:Legacy_state.t ->
  History_mode.t ->
  Genesis.t ->
  unit tzresult Lwt.t

(** [upgrade_0_0_4 ~data_dir ?patch_context ~chain_name genesis]
    upgrades a store located in [data_dir] base on v.0.0.4 to a
    v0.0.5. It outputs information regarding the necessary actions in
    order to cleanely complete the upgrade. Here this case, the user
    must delete the old storage. Returns the message to display to the
    user. *)
val upgrade_0_0_4 :
  data_dir:string ->
  ?patch_context:(Context.t -> Context.t tzresult Lwt.t) ->
  chain_name:Distributed_db_version.Name.t ->
  Genesis.t ->
  unit tzresult Lwt.t
