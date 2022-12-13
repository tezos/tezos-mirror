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

let shard_store_path = "shard_store"

let info message =
  let date = Unix.gettimeofday () |> int_of_float |> Int64.of_int in
  Irmin.Info.Default.v ~author:"DAL Node" ~message date

let set ~msg store path v = set_exn store path v ~info:(fun () -> info msg)

let remove ~msg store path = remove_exn store path ~info:(fun () -> info msg)

(** Store context *)
type node_store = {
  store : t;
  shard_store : Shard_store.t;
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
  let* store = main repo in
  let* shard_store = Shard_store.init (Filename.concat dir shard_store_path) in
  let* () = Event.(emit store_is_ready ()) in
  Lwt.return {shard_store; store; slots_watcher; slot_headers_store}

module Legacy = struct
  module Path : sig
    type t = string list

    val to_string : ?prefix:string -> t -> string

    module Commitment : sig
      val slot : Cryptobox.commitment -> slot_size:int -> Path.t

      val headers : Cryptobox.commitment -> Path.t

      val header : Cryptobox.commitment -> Services.Types.slot_id -> Path.t

      val shards : Cryptobox.commitment -> Path.t

      type shard_index := int

      val shard :
        Cryptobox.commitment ->
        redundancy_factor:int ->
        number_of_shards:int ->
        shard_index ->
        Path.t
    end

    module Level : sig
      val accepted_header_commitment : Services.Types.slot_id -> Path.t

      val accepted_header_status : Services.Types.slot_id -> Path.t

      val other_header_status :
        Services.Types.slot_id -> Cryptobox.commitment -> Path.t
    end
  end = struct
    type t = string list

    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4457
       Avoid the wasteful [List.append]s. *)
    let ( / ) path suffix = path @ [suffix]

    let to_string ?prefix p =
      let s = String.concat "/" p in
      Option.fold ~none:s ~some:(fun pr -> pr ^ s) prefix

    module Commitment = struct
      let root = ["commitments"]

      let slot commitment ~slot_size =
        let commitment_repr = Cryptobox.Commitment.to_b58check commitment in
        root / commitment_repr / Int.to_string slot_size / "slot"

      let headers commitment =
        let commitment_repr = Cryptobox.Commitment.to_b58check commitment in
        root / commitment_repr / "headers"

      let header commitment index =
        let open Services.Types in
        let prefix = headers commitment in
        prefix
        / Int32.to_string index.slot_level
        / Int.to_string index.slot_index

      let shards commitment =
        let commitment_repr = Cryptobox.Commitment.to_b58check commitment in
        root / commitment_repr / "shards"

      let shard commitment ~redundancy_factor ~number_of_shards index =
        let prefix = shards commitment in
        let parameters_repr =
          Printf.sprintf "%d-%d" redundancy_factor number_of_shards
        in
        prefix / "parameters" / parameters_repr / "index" / Int.to_string index
    end

    module Level = struct
      let root = ["levels"]

      let headers index =
        let open Services.Types in
        root / Int32.to_string index.slot_level / Int.to_string index.slot_index

      let accepted_header index =
        let prefix = headers index in
        prefix / "accepted"

      let accepted_header_commitment index =
        let prefix = accepted_header index in
        prefix / "commitment"

      let accepted_header_status index =
        let prefix = accepted_header index in
        prefix / "status"

      let other_header_status index commitment =
        let commitment_repr = Cryptobox.Commitment.to_b58check commitment in
        let prefix = headers index in
        prefix / "others" / commitment_repr / "status"
    end
  end

  let encode_exn encoding value =
    Data_encoding.Binary.to_string_exn encoding value

  let decode encoding string =
    Data_encoding.Binary.of_string_opt encoding string

  let add_slot_by_commitment node_store cryptobox slot commitment =
    let open Lwt_syntax in
    let Cryptobox.{slot_size; _} = Cryptobox.parameters cryptobox in
    let path = Path.Commitment.slot commitment ~slot_size in
    let encoded_slot = encode_exn (Data_encoding.Fixed.bytes slot_size) slot in
    let* () = set ~msg:"Slot stored" node_store.store path encoded_slot in
    let* () = Event.(emit stored_slot_content commitment) in
    Lwt_watcher.notify node_store.slots_watcher commitment ;
    return_unit

  let associate_slot_id_with_commitment node_store commitment slot_id =
    let open Lwt_syntax in
    let path = Path.Commitment.header commitment slot_id in
    (* The path allows to reconstruct the data. *)
    let* () = set ~msg:"Slot id stored" node_store.store path "" in
    return_unit

  let exists_slot_by_commitment node_store cryptobox commitment =
    let Cryptobox.{slot_size; _} = Cryptobox.parameters cryptobox in
    let path = Path.Commitment.slot commitment ~slot_size in
    mem node_store.store path

  let find_slot_by_commitment node_store cryptobox commitment =
    let open Lwt_syntax in
    let Cryptobox.{slot_size; _} = Cryptobox.parameters cryptobox in
    let path = Path.Commitment.slot commitment ~slot_size in
    let* res_opt = find node_store.store path in
    Option.bind res_opt (decode (Data_encoding.Fixed.bytes slot_size))
    |> Lwt.return

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4383
     Remove legacy code once migration to new API is done. *)
  let legacy_add_slot_headers ~block_hash slot_headers node_store =
    let slot_headers_store = node_store.slot_headers_store in
    List.iter_s
      (fun (slot_header, status) ->
        match status with
        | Dal_plugin.Succeeded ->
            let Dal_plugin.{slot_index; commitment; _} = slot_header in
            Slot_headers_store.add
              slot_headers_store
              ~primary_key:block_hash
              ~secondary_key:slot_index
              commitment
        | Dal_plugin.Failed ->
            (* This function is only supposed to add successfully applied slot
               headers. Anyway, this piece of code will be removed once fully
               implementing the new DAL API. *)
            Lwt.return_unit)
      slot_headers

  let encode_commitment = Cryptobox.Commitment.to_b58check

  let decode_commitment = Cryptobox.Commitment.of_b58check_opt

  let add_slot_headers ~block_level ~block_hash slot_headers node_store =
    let open Lwt_syntax in
    let* () = legacy_add_slot_headers ~block_hash slot_headers node_store in
    let slots_store = node_store.store in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4388
       Handle reorgs. *)
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4389
       Handle statuses evolution. *)
    List.iter_s
      (fun (slot_header, status) ->
        let Dal_plugin.{slot_index; commitment; published_level} =
          slot_header
        in
        (* This invariant should hold. *)
        assert (Int32.equal published_level block_level) ;
        match status with
        | Dal_plugin.Succeeded ->
            let index =
              Services.Types.{slot_level = published_level; slot_index}
            in
            let commitment_path = Path.Level.accepted_header_commitment index in
            let status_path = Path.Level.accepted_header_status index in
            let data = encode_commitment commitment in
            let* () =
              set
                ~msg:
                  (Path.to_string ~prefix:"add_slot_headers:" commitment_path)
                slots_store
                commitment_path
                data
            in
            set
              ~msg:(Path.to_string ~prefix:"add_slot_headers:" status_path)
              slots_store
              status_path
              (Services.Types.header_attestation_status_to_string
                 `Waiting_for_attestations)
        | Dal_plugin.Failed ->
            let index =
              Services.Types.{slot_level = published_level; slot_index}
            in
            let path = Path.Level.other_header_status index commitment in
            set
              ~msg:(Path.to_string ~prefix:"add_slot_headers:" path)
              slots_store
              path
              (Services.Types.header_status_to_string `Not_selected))
      slot_headers

  let check_old_stored_value_is ?old_value ~path store : unit tzresult Lwt.t =
    let open Lwt_result_syntax in
    let*! data_opt = find store path in
    let* data =
      match data_opt with
      | Some data -> return data
      | None ->
          failwith
            "check_old_value_is: No data found under %s"
            (Path.to_string path)
    in
    match old_value with
    | None -> return_unit
    | Some expected ->
        if String.equal expected data then return_unit
        else
          failwith "check_old_value_is: expecting %s but got %s" expected data

  let update_slot_headers_attestation ~published_level store slots =
    let open Lwt_result_syntax in
    let attestation_status, slots =
      match slots with
      | `Attested l -> (`Attested, l)
      | `Unattested l -> (`Unattested, l)
    in
    let attestation_status =
      Services.Types.header_attestation_status_to_string attestation_status
    in
    let old_value =
      Services.Types.header_attestation_status_to_string
        `Waiting_for_attestations
    in
    List.iter_es
      (fun slot_index ->
        let index = Services.Types.{slot_level = published_level; slot_index} in
        let status_path = Path.Level.accepted_header_status index in
        let* () =
          check_old_stored_value_is ~old_value ~path:status_path store
        in
        let msg =
          Path.to_string ~prefix:"update_slot_headers_attestation:" status_path
        in
        let*! () = remove ~msg store status_path in
        let*! () = set ~msg store status_path attestation_status in
        return_unit)
      slots

  let update_selected_slot_headers_statuses ~block_level ~attestation_lag
      attested unattested node_store =
    let open Lwt_result_syntax in
    let store = node_store.store in
    let published_level = Int32.(sub block_level (of_int attestation_lag)) in
    let* () = update_slot_headers_attestation ~published_level store attested in
    update_slot_headers_attestation ~published_level store unattested

  let get_commitment_by_published_level_and_index ~level ~slot_index node_store
      =
    let open Lwt_result_syntax in
    let index = Services.Types.{slot_level = level; slot_index} in
    let*! commitment_str_opt =
      find node_store.store @@ Path.Level.accepted_header_commitment index
    in
    Option.fold
      commitment_str_opt
      ~none:(fail (Ok `Not_found))
      ~some:(fun c_str ->
        Option.fold
          ~none:(Lwt.return (Error (error_with "Commitment decoding failed")))
          ~some:return
        @@ decode_commitment c_str)
end
