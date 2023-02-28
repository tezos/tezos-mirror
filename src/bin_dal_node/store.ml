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

module StoreMaker = Irmin_pack_unix.KV (Tezos_context_encoding.Context.Conf)
include StoreMaker.Make (Irmin.Contents.String)

let shard_store_dir = "shard_store"

let info message =
  let date = Unix.gettimeofday () |> int_of_float |> Int64.of_int in
  Info.v ~author:"DAL Node" ~message date

let set ~msg store path v = set_exn store path v ~info:(fun () -> info msg)

let remove ~msg store path = remove_exn store path ~info:(fun () -> info msg)

module Shards = struct
  include Key_value_store

  type nonrec t = (Cryptobox.Commitment.t * int, Cryptobox.share) t

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4973
     Make storage more resilient to DAL parameters change. *)
  let are_shards_available store commitment shard_indexes =
    let open Lwt_result_syntax in
    List.for_all_es
      (fun index ->
        let*! value = read_value store (commitment, index) in
        match value with
        | Ok _ -> return true
        | Error [Stored_data.Missing_stored_data _] -> return false
        | Error e -> fail e)
      shard_indexes

  let save_and_notify shards_store shards_watcher commitment shards =
    let open Lwt_result_syntax in
    let shards =
      Seq.map
        (fun {Cryptobox.index; share} -> ((commitment, index), share))
        shards
    in
    let* () = write_values shards_store shards |> Errors.other_lwt_result in
    let*! () =
      Event.(emit stored_slot_shards (commitment, Seq.length shards))
    in
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4974

       DAL/Node: rehaul the store  abstraction & notification system.
    *)
    return @@ Lwt_watcher.notify shards_watcher commitment

  let init node_store_dir shard_store_dir =
    let ( // ) = Filename.concat in
    let dir_path = node_store_dir // shard_store_dir in
    init ~lru_size:Constants.shards_max_mutexes (fun (commitment, index) ->
        let commitment_string = Cryptobox.Commitment.to_b58check commitment in
        let filename = string_of_int index in
        let filepath = dir_path // commitment_string // filename in
        Stored_data.make_file ~filepath Cryptobox.share_encoding Stdlib.( = ))
end

(** Store context *)
type node_store = {
  store : t;
  shard_store : Shards.t;
  shards_watcher : Cryptobox.Commitment.t Lwt_watcher.input;
}

(** [open_shards_stream node_store] opens a stream that should be notified when
    the storage is updated with new shards. *)
let open_shards_stream {shards_watcher; _} =
  Lwt_watcher.create_stream shards_watcher

(** [init config] inits the store on the filesystem using the given [config]. *)
let init config =
  let open Lwt_result_syntax in
  let base_dir = Configuration.data_dir_path config path in
  let shards_watcher = Lwt_watcher.create_input () in
  let*! repo = Repo.v (Irmin_pack.config base_dir) in
  let*! store = main repo in
  let shard_store = Shards.init base_dir shard_store_dir in
  let*! () = Event.(emit store_is_ready ()) in
  return {shard_store; store; shards_watcher}

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
    ~data_kind:Types.Commitment
    ~tztrace_of_error:(fun tztrace -> tztrace)
  @@ Cryptobox.Commitment.of_b58check v

let encode_header_status =
  Data_encoding.Binary.to_string_exn Services.Types.header_status_encoding

let decode_header_status v =
  trace_decoding_error
    ~data_kind:Types.Header_status
    ~tztrace_of_error:tztrace_of_read_error
  @@ Data_encoding.Binary.of_string Services.Types.header_status_encoding v

let decode_slot_id v =
  trace_decoding_error
    ~data_kind:Types.Slot_id
    ~tztrace_of_error:tztrace_of_read_error
  @@ Data_encoding.Binary.of_string Services.Types.slot_id_encoding v

let encode_slot slot_size =
  Data_encoding.Binary.to_string_exn (Data_encoding.Fixed.bytes slot_size)

let decode_slot slot_size v =
  trace_decoding_error
    ~data_kind:Types.Slot
    ~tztrace_of_error:tztrace_of_read_error
  @@ Data_encoding.Binary.of_string (Data_encoding.Fixed.bytes slot_size) v

let encode_profile profile =
  Data_encoding.Binary.to_string_exn Services.Types.profile_encoding profile

let decode_profile profile =
  trace_decoding_error
    ~data_kind:Types.Profile
    ~tztrace_of_error:tztrace_of_read_error
  @@ Data_encoding.Binary.of_string Services.Types.profile_encoding profile

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
      (**
         Part of the storage for slots' headers where paths are indexed by slots
         indices.

         "Accepted" path(s) are used to store information about slots headers
         that are either [`Waiting_attesattion], [`Attested], or [`Unattested].

         "Others" path(s) are used to store information of slots headers when
         their statuses are [`Not_selected] or [`Unseen]. *)

      val slots_indices : Services.Types.level -> Path.t

      val accepted_header_commitment : Services.Types.slot_id -> Path.t

      val accepted_header_status : Services.Types.slot_id -> Path.t

      val others : Services.Types.slot_id -> Path.t

      val other_header_status :
        Services.Types.slot_id -> Cryptobox.commitment -> Path.t
    end

    module Profile : sig
      val profiles : Path.t

      val profile : Services.Types.profile -> Path.t
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
        let open Services.Types in
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

    module Profile = struct
      let root = ["profiles"]

      let profiles = root

      let profile profile = root / encode_profile profile
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
        (encode_header_status `Unseen)

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

  let add_slot_headers ~block_level ~block_hash:_ slot_headers node_store =
    let open Lwt_syntax in
    let slots_store = node_store.store in
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4388
       Handle reorgs. *)
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4389
             https://gitlab.com/tezos/tezos/-/issues/4528
       Handle statuses evolution. *)
    List.iter_s
      (fun (slot_header, status) ->
        let Dal_plugin.{slot_index; commitment; published_level} =
          slot_header
        in
        (* This invariant should hold. *)
        assert (Int32.equal published_level block_level) ;
        let index = Services.Types.{slot_level = published_level; slot_index} in
        let header_path = Path.Commitment.header commitment index in
        let* () =
          set
            ~msg:(Path.to_string ~prefix:"add_slot_headers:" header_path)
            slots_store
            header_path
            ""
        in
        let others_path = Path.Level.other_header_status index commitment in
        match status with
        | Dal_plugin.Succeeded ->
            let commitment_path = Path.Level.accepted_header_commitment index in
            let status_path = Path.Level.accepted_header_status index in
            let data = encode_commitment commitment in
            (* Before adding the item in accepted path, we should remove it from
               others path, as it may appear there with an Unseen status. *)
            let* () =
              remove
                ~msg:(Path.to_string ~prefix:"add_slot_headers:" others_path)
                slots_store
                others_path
            in
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
              (encode_header_status `Waiting_attestation)
        | Dal_plugin.Failed ->
            set
              ~msg:(Path.to_string ~prefix:"add_slot_headers:" others_path)
              slots_store
              others_path
              (encode_header_status `Not_selected))
      slot_headers

  let update_slot_headers_attestation ~published_level ~number_of_slots store
      attested =
    let open Lwt_syntax in
    let module S = Set.Make (Int) in
    let attested = List.fold_left (fun s e -> S.add e s) S.empty attested in
    let attested_str = encode_header_status `Attested in
    let unattested_str = encode_header_status `Unattested in
    List.iter_s
      (fun slot_index ->
        let index = Services.Types.{slot_level = published_level; slot_index} in
        let status_path = Path.Level.accepted_header_status index in
        let msg =
          Path.to_string ~prefix:"update_slot_headers_attestation:" status_path
        in
        if S.mem slot_index attested then
          set ~msg store status_path attested_str
        else
          let* old_data_opt = find store status_path in
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
    let index = Services.Types.{slot_level = level; slot_index} in
    let*! commitment_str_opt =
      find node_store.store @@ Path.Level.accepted_header_commitment index
    in
    Option.fold
      ~none:(fail `Not_found)
      ~some:(fun c_str -> Lwt.return @@ decode_commitment c_str)
      commitment_str_opt

  let get_profiles node_store =
    let open Lwt_syntax in
    let path = Path.Profile.profiles in
    let* profiles = list node_store.store path in
    return @@ List.map_e (fun (p, _) -> decode_profile p) profiles

  let add_profile node_store profile =
    let path = Path.Profile.profile profile in
    set
      ~msg:(Printf.sprintf "New profile added: %s" (Path.to_string path))
      node_store.store
      path
      ""

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
        (fun {Services.Types.slot_level = l; slot_index = i} ->
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
                         Services.Types.slot_id;
                         commitment;
                         status = (status :> Services.Types.header_status);
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
        return @@ ({Services.Types.slot_id; commitment; status} :: acc)

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
          {
            Services.Types.slot_level = published_level;
            slot_index = int_of_string index;
          })
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
          (fun header ->
            if header.Services.Types.status = hs then Some header else None)
          accu
end
