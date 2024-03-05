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

module StoreMaker = Irmin_pack_unix.KV (Tezos_context_encoding.Context.Conf)
include StoreMaker.Make (Irmin.Contents.String)

let shard_store_dir = "shard_store"

let info message =
  let date = Unix.gettimeofday () |> int_of_float |> Int64.of_int in
  Info.v ~author:"DAL Node" ~message date

let set ~msg store path v = set_exn store path v ~info:(fun () -> info msg)

let remove ~msg store path = remove_exn store path ~info:(fun () -> info msg)

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
  module KVS = Key_value_store

  type nonrec t = (Cryptobox.Commitment.t, int, Cryptobox.share) KVS.t

  let file_layout ~root_dir commitment =
    let commitment_string = Cryptobox.Commitment.to_b58check commitment in
    let filepath = Filename.concat root_dir commitment_string in
    Key_value_store.layout
      ~encoded_value_size:(Value_size_hooks.share_size ())
      ~encoding:Cryptobox.share_encoding
      ~filepath
      ~eq:Stdlib.( = )
      ~index_of:Fun.id
      ()

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4973
     Make storage more resilient to DAL parameters change. *)
  let are_shards_available store commitment shard_indexes =
    List.for_all_es
      (KVS.value_exists store file_layout commitment)
      shard_indexes

  let save_and_notify shards_store shards_watcher commitment shards =
    let open Lwt_result_syntax in
    let shards =
      Seq.map
        (fun {Cryptobox.index; share} -> (commitment, index, share))
        shards
    in
    let* () =
      KVS.write_values shards_store file_layout shards
      |> Errors.other_lwt_result
    in
    let*! () =
      List.of_seq shards
      |> Lwt_list.iter_s (fun (_commitment, index, _share) ->
             Dal_metrics.shard_stored () ;
             Event.(emit stored_slot_shard (commitment, index)))
    in
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4974

       DAL/Node: rehaul the store  abstraction & notification system.
    *)
    return @@ Lwt_watcher.notify shards_watcher commitment

  let read_all shards_store commitment ~number_of_shards =
    Seq.ints 0
    |> Seq.take_while (fun x -> x < number_of_shards)
    |> Seq.map (fun shard_index -> (commitment, shard_index))
    |> KVS.read_values shards_store file_layout

  let read_value store commitment shard_id =
    KVS.read_value store file_layout commitment shard_id

  let read_values store keys = KVS.read_values store file_layout keys

  let init node_store_dir shard_store_dir =
    let root_dir = Filename.concat node_store_dir shard_store_dir in
    KVS.init ~lru_size:Constants.shards_store_lru_size ~root_dir
end

module Shard_proofs_cache =
  Aches.Vache.Map (Aches.Vache.LRU_Precise) (Aches.Vache.Strong)
    (struct
      type t = Cryptobox.Commitment.t

      let equal = Cryptobox.Commitment.equal

      let hash = Hashtbl.hash
    end)

(** Store context *)
type node_store = {
  store : t;
  shard_store : Shards.t;
  shards_watcher : Cryptobox.Commitment.t Lwt_watcher.input;
  in_memory_shard_proofs : Cryptobox.shard_proof array Shard_proofs_cache.t;
      (* The length of the array is the number of shards per slot *)
}

(** [open_shards_stream node_store] opens a stream that should be notified when
    the storage is updated with new shards. *)
let open_shards_stream {shards_watcher; _} =
  Lwt_watcher.create_stream shards_watcher

(** [init config] inits the store on the filesystem using the
    given [config]. *)
let init config =
  let open Lwt_result_syntax in
  let base_dir = Configuration_file.store_path config in
  let shards_watcher = Lwt_watcher.create_input () in
  let*! repo = Repo.v (Irmin_pack.config base_dir) in
  let*! store = main repo in
  let* shard_store = Shards.init base_dir shard_store_dir in
  let*! () = Event.(emit store_is_ready ()) in
  return
    {
      shard_store;
      store;
      shards_watcher;
      in_memory_shard_proofs =
        Shard_proofs_cache.create Constants.shards_proofs_cache_size;
    }

let trace_decoding_error ~data_kind ~tztrace_of_error r =
  let open Result_syntax in
  match r with
  | Ok r -> return r
  | Error err ->
      let tztrace = tztrace_of_error err in
      fail @@ `Decoding_failed (data_kind, tztrace)

let tztrace_of_read_error read_err =
  [Exn (Data_encoding.Binary.Read_error read_err)]

let encode_commitment = Cryptobox.Commitment.to_b58check

let decode_commitment v =
  trace_decoding_error
    ~data_kind:Types.Store.Commitment
    ~tztrace_of_error:(fun tztrace -> tztrace)
  @@ Cryptobox.Commitment.of_b58check v

let encode_header_status =
  Data_encoding.Binary.to_string_exn Types.header_status_encoding

let decode_header_status v =
  trace_decoding_error
    ~data_kind:Types.Store.Header_status
    ~tztrace_of_error:tztrace_of_read_error
  @@ Data_encoding.Binary.of_string Types.header_status_encoding v

let decode_slot_id v =
  trace_decoding_error
    ~data_kind:Types.Store.Slot_id
    ~tztrace_of_error:tztrace_of_read_error
  @@ Data_encoding.Binary.of_string Types.slot_id_encoding v

let encode_slot slot_size =
  Data_encoding.Binary.to_string_exn (Data_encoding.Fixed.bytes slot_size)

let decode_slot slot_size v =
  trace_decoding_error
    ~data_kind:Types.Store.Slot
    ~tztrace_of_error:tztrace_of_read_error
  @@ Data_encoding.Binary.of_string (Data_encoding.Fixed.bytes slot_size) v

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/4975

   DAL/Node: Replace Irmin storage for paths
*)
module Legacy = struct
  module Path : sig
    type t = string list

    val to_string : ?prefix:string -> t -> string

    module Commitment : sig
      val slot : Cryptobox.commitment -> slot_size:int -> Path.t

      val headers : Cryptobox.commitment -> Path.t

      val header : Cryptobox.commitment -> Types.slot_id -> Path.t

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
      (**
         Part of the storage for slots' headers where paths are indexed by slots
         indices.

         "Accepted" path(s) are used to store information about slots headers
         that are either [`Waiting_attesattion], [`Attested], or [`Unattested].

         "Others" path(s) are used to store information of slots headers when
         their statuses are [`Not_selected] or [`Unseen_or_not_finalized]. *)

      val slots_indices : Types.level -> Path.t

      val accepted_header_commitment : Types.slot_id -> Path.t

      val accepted_header_status : Types.slot_id -> Path.t

      val others : Types.slot_id -> Path.t

      val other_header_status : Types.slot_id -> Cryptobox.commitment -> Path.t
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
        let open Types in
        let prefix = headers commitment in
        prefix / Data_encoding.Binary.to_string_exn slot_id_encoding index

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

      let slots_indices slot_level = root / Int32.to_string slot_level

      let headers index =
        let open Types in
        slots_indices index.slot_level / Int.to_string index.slot_index

      let accepted_header index =
        let prefix = headers index in
        prefix / "accepted"

      let accepted_header_commitment index =
        let prefix = accepted_header index in
        prefix / "commitment"

      let accepted_header_status index =
        let prefix = accepted_header index in
        prefix / "status"

      let others index =
        let prefix = headers index in
        prefix / "others"

      let other_header_status index commitment =
        let commitment_repr = Cryptobox.Commitment.to_b58check commitment in
        others index / commitment_repr / "status"
    end
  end

  let add_slot_by_commitment node_store cryptobox slot commitment =
    let open Lwt_syntax in
    let Cryptobox.{slot_size; _} = Cryptobox.parameters cryptobox in
    let path = Path.Commitment.slot commitment ~slot_size in
    let encoded_slot = encode_slot slot_size slot in
    let* () = set ~msg:"Slot stored" node_store.store path encoded_slot in
    let* () = Event.(emit stored_slot_content commitment) in
    return_unit

  let associate_slot_id_with_commitment node_store commitment slot_id =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4528
       Improve the implementation of this handler.
    *)
    let open Lwt_syntax in
    let store = node_store.store in
    let header_path = Path.Commitment.header commitment slot_id in
    let levels_path = Path.Level.other_header_status slot_id commitment in
    let* known_levels = mem store levels_path in
    let* known_header = mem store header_path in
    (* An invariant that should hold for the storage. *)
    assert (known_levels = known_header) ;
    if known_levels then return_unit
    else
      (* The path allows to reconstruct the data. *)
      let* () =
        set
          ~msg:
            (Path.to_string
               ~prefix:"associate_slot_id_with_commitment:"
               header_path)
          store
          header_path
          ""
      in
      set
        ~msg:
          (Path.to_string
             ~prefix:"associate_slot_id_with_commitment:"
             levels_path)
        store
        levels_path
        (encode_header_status `Unseen_or_not_finalized)

  let exists_slot_by_commitment node_store cryptobox commitment =
    let Cryptobox.{slot_size; _} = Cryptobox.parameters cryptobox in
    let path = Path.Commitment.slot commitment ~slot_size in
    mem node_store.store path

  let find_slot_by_commitment node_store cryptobox commitment =
    let open Lwt_result_syntax in
    let Cryptobox.{slot_size; _} = Cryptobox.parameters cryptobox in
    let path = Path.Commitment.slot commitment ~slot_size in
    let*! res_opt = find node_store.store path in
    Option.fold
      ~none:(return None)
      ~some:(fun v ->
        let*? dec = decode_slot slot_size v in
        return @@ Some dec)
      res_opt

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
          let Dal_plugin.{slot_index; commitment; published_level} =
            slot_header
          in
          (* This invariant should hold. *)
          assert (Int32.equal published_level block_level) ;
          let index = Types.{slot_level = published_level; slot_index} in
          let header_path = Path.Commitment.header commitment index in
          let*! () =
            set
              ~msg:(Path.to_string ~prefix:"add_slot_headers:" header_path)
              slots_store
              header_path
              ""
          in
          let others_path = Path.Level.other_header_status index commitment in
          match status with
          | Dal_plugin.Succeeded ->
              let commitment_path =
                Path.Level.accepted_header_commitment index
              in
              let status_path = Path.Level.accepted_header_status index in
              let data = encode_commitment commitment in
              (* Before adding the item in accepted path, we should remove it from
                 others path, as it may appear there with an
                 Unseen_or_not_finalized status. *)
              let*! () =
                remove
                  ~msg:(Path.to_string ~prefix:"add_slot_headers:" others_path)
                  slots_store
                  others_path
              in
              let*! () =
                set
                  ~msg:
                    (Path.to_string ~prefix:"add_slot_headers:" commitment_path)
                  slots_store
                  commitment_path
                  data
              in
              let*! () =
                set
                  ~msg:(Path.to_string ~prefix:"add_slot_headers:" status_path)
                  slots_store
                  status_path
                  (encode_header_status `Waiting_attestation)
              in
              return (SI.add slot_index waiting)
          | Dal_plugin.Failed ->
              let*! () =
                set
                  ~msg:(Path.to_string ~prefix:"add_slot_headers:" others_path)
                  slots_store
                  others_path
                  (encode_header_status `Not_selected)
              in
              return waiting)
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
        let index = Types.{slot_level = published_level; slot_index} in
        let status_path = Path.Level.accepted_header_status index in
        let msg =
          Path.to_string ~prefix:"update_slot_headers_attestation:" status_path
        in
        if S.mem slot_index attested then (
          Dal_metrics.slot_attested ~set:true slot_index ;
          set ~msg store status_path attested_str)
        else
          let* old_data_opt = find store status_path in
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

  let get_commitment_by_published_level_and_index ~level ~slot_index node_store
      =
    let open Lwt_result_syntax in
    let index = Types.{slot_level = level; slot_index} in
    let*! commitment_str_opt =
      find node_store.store @@ Path.Level.accepted_header_commitment index
    in
    Option.fold
      ~none:(fail `Not_found)
      ~some:(fun c_str -> Lwt.return @@ decode_commitment c_str)
      commitment_str_opt

  (** Filter the given list of indices according to the values of the given slot
      level and index. *)
  let filter_indexes =
    let keep_field v = function None -> true | Some f -> f = v in
    fun ?slot_level ?slot_index indexes ->
      let open Result_syntax in
      let* indexes =
        List.map_e (fun (slot_id, _) -> decode_slot_id slot_id) indexes
      in
      List.filter
        (fun {Types.slot_level = l; slot_index = i} ->
          keep_field l slot_level && keep_field i slot_index)
        indexes
      |> return

  (* See doc-string in {!Legacy.Path.Level} for the notion of "accepted"
     header. *)
  let get_accepted_headers ~skip_commitment slot_ids store accu =
    let open Lwt_result_syntax in
    List.fold_left_es
      (fun acc slot_id ->
        let commitment_path = Path.Level.accepted_header_commitment slot_id in
        let*! commitment_opt = find store commitment_path in
        match commitment_opt with
        | None -> return acc
        | Some read_commitment -> (
            let*? decision = skip_commitment read_commitment in
            match decision with
            | `Skip -> return acc
            | `Keep commitment -> (
                let status_path = Path.Level.accepted_header_status slot_id in
                let*! status_opt = find store status_path in
                match status_opt with
                | None -> return acc
                | Some status_str ->
                    let*? status = decode_header_status status_str in
                    return
                    @@ {
                         Types.slot_id;
                         commitment;
                         status = (status :> Types.header_status);
                       }
                       :: acc)))
      accu
      slot_ids

  (* See doc-string in {!Legacy.Path.Level} for the notion of "accepted"
     header. *)
  let get_accepted_headers_of_commitment commitment slot_ids store accu =
    let encoded_commitment = encode_commitment commitment in
    let skip_commitment read_commitment =
      Result_syntax.return
        (if String.equal read_commitment encoded_commitment then
         `Keep commitment
        else `Skip)
    in
    get_accepted_headers ~skip_commitment slot_ids store accu

  (* See doc-string in {!Legacy.Path.Level} for the notion of "other(s)"
     header. *)
  let get_other_headers_of_identified_commitment commitment slot_id store acc =
    let open Lwt_result_syntax in
    let*! status_opt =
      find store @@ Path.Level.other_header_status slot_id commitment
    in
    match status_opt with
    | None -> return acc
    | Some status_str ->
        let*? status = decode_header_status status_str in
        return @@ ({Types.slot_id; commitment; status} :: acc)

  (* See doc-string in {!Legacy.Path.Level} for the notion of "other(s)"
     header. *)
  let get_other_headers_of_commitment commitment slot_ids store accu =
    List.fold_left_es
      (fun acc slot_id ->
        get_other_headers_of_identified_commitment commitment slot_id store acc)
      accu
      slot_ids

  let get_commitment_headers commitment ?slot_level ?slot_index node_store =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4528
       Improve the implementation of this handler.
    *)
    let open Lwt_result_syntax in
    let store = node_store.store in
    (* Get the list of known slot identifiers for [commitment]. *)
    let*! indexes = list store @@ Path.Commitment.headers commitment in
    (* Filter the list of indices by the values of [slot_level] [slot_index]. *)
    let*? slot_ids = filter_indexes ?slot_level ?slot_index indexes in
    let* accu = get_other_headers_of_commitment commitment slot_ids store [] in
    get_accepted_headers_of_commitment commitment slot_ids store accu

  (* See doc-string in {!Legacy.Path.Level} for the notion of "other(s)"
     header. *)
  let get_other_headers slot_ids store accu =
    let open Lwt_result_syntax in
    List.fold_left_es
      (fun acc slot_id ->
        let*! commitments_with_statuses =
          list store @@ Path.Level.others slot_id
        in
        List.fold_left_es
          (fun acc (encoded_commitment, _status_tree) ->
            let*? commitment = decode_commitment encoded_commitment in
            get_other_headers_of_identified_commitment
              commitment
              slot_id
              store
              acc)
          acc
          commitments_with_statuses)
      accu
      slot_ids

  let get_published_level_headers ~published_level ?header_status node_store =
    let open Lwt_result_syntax in
    let store = node_store.store in
    (* Get the list of slots indices from the given level. *)
    let*! slots_indices =
      list store @@ Path.Level.slots_indices published_level
    in
    (* Build the list of slot IDs. *)
    let slot_ids =
      List.rev_map
        (fun (index, _tree) ->
          {Types.slot_level = published_level; slot_index = int_of_string index})
        slots_indices
    in
    let* accu = get_other_headers slot_ids store [] in
    let* accu =
      let skip_commitment c =
        let open Result_syntax in
        let* commit = decode_commitment c in
        return @@ `Keep commit
      in
      get_accepted_headers ~skip_commitment slot_ids store accu
    in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4541
       Enable the same filtering for GET /commitments/<commitment>/headers
       (function get_commitment_headers above). Push this filtering into the result
       construction? *)
    return
    @@
    match header_status with
    | None -> accu
    | Some hs ->
        List.filter_map
          (fun header -> if header.Types.status = hs then Some header else None)
          accu

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4641

     handle with_proof flag -> store proofs on disk? *)
  let save_shard_proofs node_store commitment shard_proofs =
    Shard_proofs_cache.replace
      node_store.in_memory_shard_proofs
      commitment
      shard_proofs
end
