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

(* In this legacy module, we only use legacy events. *)
module Event = Event_legacy

type error +=
  | Splitting_failed of string
  | Merging_failed of string
  | Invalid_slot_header of string * string
  | Missing_shards of {provided : int; required : int}
  | Illformed_shard
  | Slot_not_found
  | Illformed_pages
  | Invalid_shards_slot_header_association

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.split_failed"
    ~title:"Split failed"
    ~description:"Splitting the slot failed"
    ~pp:(fun ppf msg -> Format.fprintf ppf "%s" msg)
    Data_encoding.(obj1 (req "msg" string))
    (function Splitting_failed parameter -> Some parameter | _ -> None)
    (fun parameter -> Splitting_failed parameter) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.merge_failed"
    ~title:"Merge failed"
    ~description:"Merging the slot failed"
    ~pp:(fun ppf msg -> Format.fprintf ppf "%s" msg)
    Data_encoding.(obj1 (req "msg" string))
    (function Merging_failed parameter -> Some parameter | _ -> None)
    (fun parameter -> Merging_failed parameter) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.invalid_slot_header"
    ~title:"Invalid slot_header"
    ~description:"The slot header is not valid"
    ~pp:(fun ppf (msg, com) -> Format.fprintf ppf "%s : %s" msg com)
    Data_encoding.(obj2 (req "msg" string) (req "com" string))
    (function Invalid_slot_header (msg, com) -> Some (msg, com) | _ -> None)
    (fun (msg, com) -> Invalid_slot_header (msg, com)) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.missing_shards"
    ~title:"Missing shards"
    ~description:"Some shards are missing"
    ~pp:(fun ppf (provided, required) ->
      Format.fprintf
        ppf
        "Some shards are missing, expected at least %d, found %d. Store is \
         invalid."
        provided
        required)
    Data_encoding.(obj2 (req "provided" int31) (req "required" int31))
    (function
      | Missing_shards {provided; required} -> Some (provided, required)
      | _ -> None)
    (fun (provided, required) -> Missing_shards {provided; required}) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.slot_not_found"
    ~title:"Slot not found"
    ~description:"Slot not found at this slot header"
    ~pp:(fun ppf () -> Format.fprintf ppf "Slot not found on given slot header")
    Data_encoding.(unit)
    (function Slot_not_found -> Some () | _ -> None)
    (fun () -> Slot_not_found) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.illformed_shard"
    ~title:"Illformed shard"
    ~description:"Illformed shard found in the store"
    ~pp:(fun ppf () -> Format.fprintf ppf "Illformed shard found in the store")
    Data_encoding.(unit)
    (function Illformed_shard -> Some () | _ -> None)
    (fun () -> Illformed_shard) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.illformed_pages"
    ~title:"Illformed pages"
    ~description:"Illformed pages found in the store"
    ~pp:(fun ppf () -> Format.fprintf ppf "Illformed pages found in the store")
    Data_encoding.(unit)
    (function Illformed_pages -> Some () | _ -> None)
    (fun () -> Illformed_pages) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.invalid_shards_slot_header_association"
    ~title:"Invalid shards with slot header association"
    ~description:"Shards commit to a different slot header."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Association between shards and slot header is invalid")
    Data_encoding.(unit)
    (function Invalid_shards_slot_header_association -> Some () | _ -> None)
    (fun () -> Invalid_shards_slot_header_association)

type slot = bytes

let wrap_encoding_error =
  Result.map_error (fun e ->
      [Tezos_base.Data_encoding_wrapper.Encoding_error e])

let encode enc v = Data_encoding.Binary.to_string enc v |> wrap_encoding_error

let decode_share s =
  Data_encoding.Binary.of_string Cryptobox.share_encoding s
  |> Result.map_error (fun e ->
         [Tezos_base.Data_encoding_wrapper.Decoding_error e])

let save store watcher slot_header shards =
  let open Lwt_result_syntax in
  let slot_header_b58 = Cryptobox.Commitment.to_b58check slot_header in
  let* () =
    Cryptobox.IntMap.iter_es
      (fun i share ->
        let path =
          Store.Legacy_paths.slot_shard_by_commitment slot_header_b58 i
        in
        let*? share = encode Cryptobox.share_encoding share in
        let*! metadata = Store.set ~msg:"Share stored" store path share in
        return metadata)
      shards
  in
  let*! () =
    Event.(
      emit stored_slot_shards (slot_header_b58, Cryptobox.IntMap.cardinal shards))
  in
  Lwt_watcher.notify watcher slot_header ;
  return_unit

let split_and_store watcher dal_constants store slot =
  let r =
    let open Result_syntax in
    let* polynomial = Cryptobox.polynomial_from_slot dal_constants slot in
    let slot_header = Cryptobox.commit dal_constants polynomial in
    return (polynomial, slot_header)
  in
  let open Lwt_result_syntax in
  match r with
  | Ok (polynomial, slot_header) ->
      let shards = Cryptobox.shards_from_polynomial dal_constants polynomial in
      let* () = save store watcher slot_header shards in
      Lwt.return_ok slot_header
  | Error (`Slot_wrong_size msg) -> Lwt.return_error [Splitting_failed msg]

let check_slot_consistency dal_parameters shards =
  let open Result_syntax in
  if shards = [] then fail [Slot_not_found]
  else
    let required_shards =
      dal_parameters.Cryptobox.number_of_shards
      / dal_parameters.Cryptobox.redundancy_factor
    in
    if Compare.List_length_with.(shards >= required_shards) then Ok ()
    else
      fail
        [
          Missing_shards
            {provided = List.length shards; required = required_shards};
        ]

let polynomial_from_shards dal_constants shards =
  match Cryptobox.polynomial_from_shards dal_constants shards with
  | Ok p -> Ok p
  | Error (`Invert_zero msg | `Not_enough_shards msg) ->
      Error [Merging_failed msg]

let save_shards store watcher dal_constants slot_header shards =
  let open Lwt_result_syntax in
  let*? polynomial = polynomial_from_shards dal_constants shards in
  let rebuilt_slot_header = Cryptobox.commit dal_constants polynomial in
  let*? () =
    if Cryptobox.Commitment.equal slot_header rebuilt_slot_header then Ok ()
    else Result_syntax.fail [Invalid_shards_slot_header_association]
  in
  save store watcher slot_header shards

let fold_stored_shards ~check_shards store dal_parameters f init slot_header =
  let open Lwt_result_syntax in
  let slot_header = Cryptobox.Commitment.to_b58check slot_header in
  let path = Store.Legacy_paths.slot_shards_by_commitment slot_header in
  let*! shards = Store.list store path in
  let*? () =
    if check_shards then check_slot_consistency dal_parameters shards else Ok ()
  in
  List.fold_left_es
    (fun shards (i, tree) ->
      let i = int_of_string i in
      let* share =
        match Store.Tree.destruct tree with
        | `Node _ -> fail [Illformed_shard]
        | `Contents (c, _metadata) ->
            catch_es (fun () ->
                let*! share = Store.Tree.Contents.force_exn c in
                return share)
      in
      let*? share = decode_share share in
      return (f i share shards))
    init
    shards

module Shard_id_set = Set.Make (Int)

let get_shard store slot_header shard_id =
  let open Lwt_result_syntax in
  let slot_header = Cryptobox.Commitment.to_b58check slot_header in
  let path = Store.Legacy_paths.slot_shard_by_commitment slot_header shard_id in
  let* share =
    Lwt.catch
      (fun () ->
        let*! r = Store.get store path in
        return r)
      (function
        | Invalid_argument _ -> fail [Slot_not_found] | e -> fail [Exn e])
  in
  let*? share = decode_share share in
  return Cryptobox.{index = shard_id; share}

let get_shards store dal_parameters slot_header shard_ids =
  fold_stored_shards
    ~check_shards:false
    store
    dal_parameters
    (fun id share acc ->
      if Shard_id_set.mem id shard_ids then Cryptobox.{index = id; share} :: acc
      else acc)
    []
    slot_header

let get_slot dal_parameters dal_constants store slot_header =
  let open Lwt_result_syntax in
  let* shards =
    fold_stored_shards
      ~check_shards:true
      store
      dal_parameters
      Cryptobox.IntMap.add
      Cryptobox.IntMap.empty
      slot_header
  in
  let*? polynomial = polynomial_from_shards dal_constants shards in
  let slot = Cryptobox.polynomial_to_bytes dal_constants polynomial in
  let*! () =
    Event.(
      emit fetched_slot (Bytes.length slot, Cryptobox.IntMap.cardinal shards))
  in
  return slot

let get_slot_pages ({Cryptobox.page_size; _} as initial_constants) dal_constants
    store slot_header =
  let open Lwt_result_syntax in
  let* slot = get_slot initial_constants dal_constants store slot_header in
  (* The slot size `Bytes.length slot` should be an exact multiple of `page_size`.
     If this is not the case, we throw an `Illformed_pages` error.
  *)
  (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3900
     Implement `Bytes.chunk_bytes` which returns a list of bytes directly. *)
  let*? pages =
    String.chunk_bytes
      page_size
      slot
      ~error_on_partial_chunk:(TzTrace.make Illformed_pages)
  in
  return @@ List.map (fun page -> String.to_bytes page) pages

let store_slot_headers store block_hash slot_headers =
  List.iter_s
    (fun (slot_index, slot_header) ->
      Slot_headers_store.add
        store
        ~primary_key:block_hash
        ~secondary_key:slot_index
        slot_header)
    slot_headers
