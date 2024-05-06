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

module KVS = Key_value_store
module StoreMaker = Irmin_pack_unix.KV (Tezos_context_encoding.Context.Conf)
module Irmin = StoreMaker.Make (Irmin.Contents.String)

type irmin = Irmin.t

let trace_decoding_error ~data_kind ~tztrace_of_error r =
  let open Result_syntax in
  match r with
  | Ok r -> return r
  | Error err ->
      let tztrace = tztrace_of_error err in
      fail @@ Errors.decoding_failed data_kind tztrace

module Stores_dirs = struct
  let shard = "shard_store"

  let slot = "slot_store"
end

let info message =
  let date = Unix.gettimeofday () |> int_of_float |> Int64.of_int in
  Irmin.Info.v ~author:"DAL Node" ~message date

let set ~msg store path v =
  Irmin.set_exn store path v ~info:(fun () -> info msg)

module Value_size_hooks = struct
  (* The [value_size] required by [Tezos_key_value_store.directory] is known when
     the daemon loads a protocol, after the store is activated. We use the closure
     [value_size_fun] to perform delayed protocol-specific parameter passing.

     Note that this mechanism is not sufficient to make the key-value store
     robust to dynamic changes in [value_size]. For instance, there could be
     concurrent writes for protocol P-1 and protocol P, if they define
     distinct [value_size] this will make it so that [P-1] uses the [value_size]
     of [P].

     A potential solution would have a function [Cryptobox.share_encoding : t -> share encoding]
     with the property that the produced encodings are of [`Fixed] class.
     The [Key_value_store.t] type could be parameterized by an extra type parameter
     corresponding to some dynamic state (corresponding to the cryptobox in our
     use case), passed explicitly to the [write] and [read] functions.

     Correcting this is left to future work.

     TODO: https://gitlab.com/tezos/tezos/-/issues/6034 *)

  (* We used the [share_size] callback to pass the share size to the store
     in a delayed fashion, when the protocol becomes known to the daemon. *)
  let share_size_ref = ref None

  let set_share_size size =
    match !share_size_ref with
    | None -> share_size_ref := Some size
    | Some previous_size ->
        if Int.equal size previous_size then ()
        else
          Stdlib.failwith
            "Store.set_share_size: new share size incompatible with current \
             store"

  let share_size () =
    match !share_size_ref with None -> assert false | Some size -> size
end

module Shards = struct
  type nonrec t = (Types.slot_id, int, Cryptobox.share) KVS.t

  let file_layout ~root_dir (slot_id : Types.slot_id) =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7045

       Make Key-Value store layout resilient to crypto parameters change.  Also,
       putting a value not far from the real number of shards allows saving disk
       storage. *)
    let number_of_shards = 4096 in
    let slot_id_string =
      Format.asprintf "%ld_%d" slot_id.slot_level slot_id.slot_index
    in
    let filepath = Filename.concat root_dir slot_id_string in
    Key_value_store.layout
      ~encoded_value_size:(Value_size_hooks.share_size ())
      ~encoding:Cryptobox.share_encoding
      ~filepath
      ~eq:Stdlib.( = )
      ~index_of:Fun.id
      ~number_of_keys_per_file:number_of_shards
      ()

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4973
     Make storage more resilient to DAL parameters change. *)
  let are_shards_available store slot_id shard_indexes =
    List.for_all_es (KVS.value_exists store file_layout slot_id) shard_indexes

  let write_all shards_store slot_id shards =
    let open Lwt_result_syntax in
    let shards =
      Seq.map (fun {Cryptobox.index; share} -> (slot_id, index, share)) shards
    in
    let* () =
      KVS.write_values shards_store file_layout shards
      |> Errors.other_lwt_result
    in
    let*! () =
      List.of_seq shards
      |> Lwt_list.iter_s (fun (_slot_id, index, _share) ->
             Dal_metrics.shard_stored () ;
             Event.(
               emit
                 stored_slot_shard
                 (slot_id.slot_level, slot_id.slot_index, index)))
    in
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4974

       DAL/Node: rehaul the store  abstraction & notification system.
    *)
    return_unit

  let read_all shards_store slot_id ~number_of_shards =
    Seq.ints 0
    |> Seq.take_while (fun x -> x < number_of_shards)
    |> Seq.map (fun shard_index -> (slot_id, shard_index))
    |> KVS.read_values shards_store file_layout

  let read store slot_id shard_id =
    let open Lwt_result_syntax in
    let*! res = KVS.read_value store file_layout slot_id shard_id in
    let data_kind = Types.Store.Shard in
    match res with
    | Ok share -> return {Cryptobox.share; index = shard_id}
    | Error [KVS.Missing_stored_kvs_data _] -> fail Errors.not_found
    | Error err -> fail @@ Errors.decoding_failed data_kind err

  let count_values store slot_id = KVS.count_values store file_layout slot_id

  let remove store slot_id = KVS.remove_file store file_layout slot_id

  let init node_store_dir shard_store_dir =
    let root_dir = Filename.concat node_store_dir shard_store_dir in
    KVS.init ~lru_size:Constants.shards_store_lru_size ~root_dir
end

module Slots = struct
  type t = (Types.slot_id * int, unit, bytes) KVS.t

  let file_layout ~root_dir ((slot_id : Types.slot_id), slot_size) =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/7045

       Make Key-Value store layout resilient to crypto parameters change. *)
    let number_of_slots = 1 in
    let slot_id_string =
      Format.asprintf "%ld_%d" slot_id.slot_level slot_id.slot_index
    in
    let filename = Format.sprintf "%s_%d" slot_id_string slot_size in
    let filepath = Filename.concat root_dir filename in
    Key_value_store.layout
      ~encoding:(Data_encoding.Fixed.bytes slot_size)
      ~filepath
      ~eq:Stdlib.( = )
      ~index_of:(fun () -> 0)
      ~number_of_keys_per_file:number_of_slots
      ()

  let init node_store_dir slot_store_dir =
    let root_dir = Filename.concat node_store_dir slot_store_dir in
    KVS.init ~lru_size:Constants.slots_store_lru_size ~root_dir

  let add_slot t ~slot_size slot (slot_id : Types.slot_id) =
    let open Lwt_result_syntax in
    let* () =
      KVS.write_value ~override:true t file_layout (slot_id, slot_size) () slot
      |> Errors.other_lwt_result
    in
    let*! () =
      Event.(emit stored_slot_content (slot_id.slot_level, slot_id.slot_index))
    in
    return_unit

  let exists_slot t ~slot_size slot_id =
    let open Lwt_syntax in
    let+ res = KVS.value_exists t file_layout (slot_id, slot_size) () in
    trace_decoding_error
      ~data_kind:Types.Store.Slot
      ~tztrace_of_error:Fun.id
      res

  let find_slot t ~slot_size slot_id =
    let open Lwt_result_syntax in
    let*! res = KVS.read_value t file_layout (slot_id, slot_size) () in
    let data_kind = Types.Store.Slot in
    match res with
    | Ok slot -> return slot
    | Error [KVS.Missing_stored_kvs_data _] -> fail Errors.not_found
    | Error err -> fail @@ Errors.decoding_failed data_kind err

  let remove_slot t ~slot_size slot_id =
    KVS.remove_file t file_layout (slot_id, slot_size)
end

module Commitment_indexed_cache =
  Aches.Vache.Map (Aches.Vache.LRU_Precise) (Aches.Vache.Strong)
    (struct
      type t = Cryptobox.Commitment.t

      let equal = Cryptobox.Commitment.equal

      let hash = Hashtbl.hash
    end)

module Shard_proofs_cache = Commitment_indexed_cache
module Shard_cache = Commitment_indexed_cache
module Slot_cache = Commitment_indexed_cache

(** Store context *)
type t = {
  store : irmin;
  shards : Shards.t;
  slots : Slots.t;
  in_memory_shard_proofs : Cryptobox.shard_proof array Shard_proofs_cache.t;
      (* The length of the array is the number of shards per slot *)
  not_yet_published_shards : Cryptobox.share array Shard_cache.t;
  not_yet_published_slots : Cryptobox.slot Slot_cache.t;
}

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4641

   handle with_proof flag -> store proofs on disk? *)
let cache_shard_proofs node_store commitment shard_proofs =
  Shard_proofs_cache.replace
    node_store.in_memory_shard_proofs
    commitment
    shard_proofs

let cache_shards node_store commitment shards =
  Shard_cache.replace node_store.not_yet_published_shards commitment shards

let cache_slot node_store commitment slot =
  Slot_cache.replace node_store.not_yet_published_slots commitment slot

(** [init config] inits the store on the filesystem using the
    given [config]. *)
let init config =
  let open Lwt_result_syntax in
  let base_dir = Configuration_file.store_path config in
  let*! repo = Irmin.Repo.v (Irmin_pack.config base_dir) in
  let*! store = Irmin.main repo in
  let* shards = Shards.init base_dir Stores_dirs.shard in
  let* slots = Slots.init base_dir Stores_dirs.slot in
  let*! () = Event.(emit store_is_ready ()) in
  return
    {
      shards;
      slots;
      store;
      in_memory_shard_proofs =
        Shard_proofs_cache.create Constants.shards_proofs_cache_size;
      not_yet_published_shards = Shard_cache.create Constants.shard_cache_size;
      not_yet_published_slots = Slot_cache.create Constants.slot_cache_size;
    }

let tztrace_of_read_error read_err =
  [Exn (Data_encoding.Binary.Read_error read_err)]

let encode_header_status =
  Data_encoding.Binary.to_string_exn Types.header_status_encoding

let decode_header_status v =
  trace_decoding_error
    ~data_kind:Types.Store.Header_status
    ~tztrace_of_error:tztrace_of_read_error
  @@ Data_encoding.Binary.of_string Types.header_status_encoding v

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4975

   DAL/Node: Replace Irmin storage for paths
*)
module Legacy = struct
  module Path : sig
    type t = string list

    val to_string : ?prefix:string -> t -> string

    module Level : sig
      (**
         Part of the storage for slots' headers where paths are indexed by slots
         indices.

         The status associated to a slot header is either
         [`Waiting_attesattion], [`Attested], or [`Unattested]. *)

      val status : Types.slot_id -> Irmin.Path.t
    end
  end = struct
    type t = string list

    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4457
       Avoid the wasteful [List.append]s. *)
    let ( / ) path suffix = path @ [suffix]

    let to_string ?prefix p =
      let s = String.concat "/" p in
      Option.fold ~none:s ~some:(fun pr -> pr ^ s) prefix

    module Level = struct
      let root = ["levels"]

      let slots_indices slot_level = root / Int32.to_string slot_level

      let headers index =
        let open Types.Slot_id in
        slots_indices index.slot_level / Int.to_string index.slot_index

      let status index =
        let prefix = headers index in
        prefix / "status"
    end
  end

  let add_slot_headers ~number_of_slots ~block_level slot_headers node_store =
    let module SI = Set.Make (Int) in
    let open Lwt_result_syntax in
    let slots_store = node_store.store in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4388
       Handle reorgs. *)
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4389
             https://gitlab.com/tezos/tezos/-/issues/4528
       Handle statuses evolution. *)
    let* waiting =
      List.fold_left_es
        (fun waiting (slot_header, status) ->
          let Dal_plugin.{slot_index; commitment = _; published_level} =
            slot_header
          in
          (* This invariant should hold. *)
          assert (Int32.equal published_level block_level) ;
          let index =
            Types.Slot_id.{slot_level = published_level; slot_index}
          in
          match status with
          | Dal_plugin.Succeeded ->
              let status_path = Path.Level.status index in
              let*! () =
                set
                  ~msg:(Path.to_string ~prefix:"add_slot_headers:" status_path)
                  slots_store
                  status_path
                  (encode_header_status `Waiting_attestation)
              in
              return (SI.add slot_index waiting)
          | Dal_plugin.Failed -> return waiting)
        SI.empty
        slot_headers
    in
    List.iter
      (fun i ->
        Dal_metrics.slot_waiting_for_attestation ~set:(SI.mem i waiting) i)
      (0 -- (number_of_slots - 1)) ;
    return_unit

  let update_slot_headers_attestation ~published_level ~number_of_slots store
      attested =
    let open Lwt_syntax in
    let module S = Set.Make (Int) in
    let attested = List.fold_left (fun s e -> S.add e s) S.empty attested in
    let attested_str = encode_header_status `Attested in
    let unattested_str = encode_header_status `Unattested in
    List.iter_s
      (fun slot_index ->
        let index = Types.Slot_id.{slot_level = published_level; slot_index} in
        let status_path = Path.Level.status index in
        let msg =
          Path.to_string ~prefix:"update_slot_headers_attestation:" status_path
        in
        if S.mem slot_index attested then (
          Dal_metrics.slot_attested ~set:true slot_index ;
          set ~msg store status_path attested_str)
        else
          let* old_data_opt = Irmin.find store status_path in
          Dal_metrics.slot_attested ~set:false slot_index ;
          if Option.is_some old_data_opt then
            set ~msg store status_path unattested_str
          else
            (* There is no header that has been included in a block and selected
               for  this index. So, the slot cannot be attested or
               unattested. *)
            return_unit)
      (0 -- (number_of_slots - 1))

  let update_selected_slot_headers_statuses ~block_level ~attestation_lag
      ~number_of_slots attested node_store =
    let store = node_store.store in
    let published_level = Int32.(sub block_level (of_int attestation_lag)) in
    update_slot_headers_attestation
      ~published_level
      ~number_of_slots
      store
      attested

  let get_slot_status ~slot_id node_store =
    let open Lwt_result_syntax in
    let store = node_store.store in
    let status_path = Path.Level.status slot_id in
    let*! status_opt = Irmin.find store status_path in
    match status_opt with
    | None -> fail Errors.not_found
    | Some status_str ->
        let*? status = decode_header_status status_str in
        return status
end
