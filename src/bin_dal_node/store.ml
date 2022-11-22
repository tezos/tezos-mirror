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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4097
   Add an interface to this module *)

(* Relative path to store directory from base-dir *)
let path = "store"

let slot_header_store = "slot_header_store"

module StoreMaker = Irmin_pack_unix.KV (Tezos_context_encoding.Context.Conf)
include StoreMaker.Make (Irmin.Contents.String)

let info message =
  let date = Unix.gettimeofday () |> int_of_float |> Int64.of_int in
  Irmin.Info.Default.v ~author:"DAL Node" ~message date

let set ~msg store path v = set_exn store path v ~info:(fun () -> info msg)

(** Store context *)
type node_store = {
  slots_store : t;
  slot_headers_store : Slot_headers_store.t;
  slots_watcher : Cryptobox.Commitment.t Lwt_watcher.input;
}

(** [open_slots_watcher node_store] opens a stream that should be notified when
    the storage is updated with a new slot. *)
let open_slots_stream {slots_watcher; _} =
  Lwt_watcher.create_stream slots_watcher

(** [init config] inits the store on the filesystem using the given [config]. *)
let init config =
  let open Lwt_syntax in
  let dir = Configuration.data_dir_path config path in
  let* slot_headers_store = Slot_headers_store.load dir in
  let slots_watcher = Lwt_watcher.create_input () in
  let* repo = Repo.v (Irmin_pack.config dir) in
  let* slots_store = main repo in
  let* () = Event.(emit store_is_ready ()) in
  Lwt.return {slots_store; slots_watcher; slot_headers_store}

module Legacy_paths = struct
  let slot_by_commitment commitment = ["slots"; commitment]

  let slot_ids_by_commitment commitment =
    slot_by_commitment commitment @ ["slot_ids"]

  let slot_shards_by_commitment commitment =
    slot_by_commitment commitment @ ["shards"]

  let slot_shard_by_commitment commitment index =
    slot_shards_by_commitment commitment @ [string_of_int index]

  let slot_shards_flag_by_commitment commitment =
    slot_by_commitment commitment @ ["shards_saved_flag"]
end

module Legacy = struct
  let encode enc v =
    Data_encoding.Binary.to_string enc v
    |> Result.map_error (fun e ->
           [Tezos_base.Data_encoding_wrapper.Encoding_error e])

  let add_slot_by_commitment node_store slot commitment =
    let open Lwt_syntax in
    let commitment_b58 = Cryptobox.Commitment.to_b58check commitment in
    let path = Legacy_paths.slot_by_commitment commitment_b58 in
    let encoded_slot = Bytes.to_string slot in
    let* () = set ~msg:"Slot stored" node_store.slots_store path encoded_slot in
    let* () = Event.(emit stored_slot_content commitment_b58) in
    Lwt_watcher.notify node_store.slots_watcher commitment ;
    return_unit

  let associate_slot_id_with_commitment node_store commitment slot_id =
    let open Lwt_syntax in
    let commitment_b58 = Cryptobox.Commitment.to_b58check commitment in
    let path = Legacy_paths.slot_id_by_commitment commitment_b58 slot_id in
    let* () = set ~msg:"Slot id stored" node_store.slots_store path "" in
    return_unit

  let exists_slot_by_commitment node_store commitment =
    let commitment_b58 = Cryptobox.Commitment.to_b58check commitment in
    let path = Legacy_paths.slot_by_commitment commitment_b58 in
    mem node_store.slots_store path
end
