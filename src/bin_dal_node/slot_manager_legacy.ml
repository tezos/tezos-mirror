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
  | Merging_failed of string
  | Invalid_commitment of string * string
  | Missing_shards of {provided : int; required : int}
  | Illformed_shard
  | Slot_not_found
  | Illformed_pages
  | Invalid_shards_commitment_association
  | Invalid_degree_strictly_less_than_expected of {given : int; expected : int}

let () =
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
    ~id:"dal.node.invalid_commitment"
    ~title:"Invalid commitment"
    ~description:"The slot header is not valid"
    ~pp:(fun ppf (msg, com) -> Format.fprintf ppf "%s : %s" msg com)
    Data_encoding.(obj2 (req "msg" string) (req "com" string))
    (function Invalid_commitment (msg, com) -> Some (msg, com) | _ -> None)
    (fun (msg, com) -> Invalid_commitment (msg, com)) ;
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
    ~id:"dal.node.invalid_shards_commitment_association"
    ~title:"Invalid shards with slot header association"
    ~description:"Shards commit to a different slot header."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Association between shards and slot header is invalid")
    Data_encoding.(unit)
    (function Invalid_shards_commitment_association -> Some () | _ -> None)
    (fun () -> Invalid_shards_commitment_association) ;
  register_error_kind
    `Permanent
    ~id:"dal.node.invalid_degree"
    ~title:"Invalid degree"
    ~description:"The degree of the polynomial is too high"
    ~pp:(fun ppf (given, expected) ->
      Format.fprintf
        ppf
        "Got %d, expecting a value strictly less than %d"
        given
        expected)
    Data_encoding.(obj2 (req "given" int31) (req "expected" int31))
    (function
      | Invalid_degree_strictly_less_than_expected {given; expected} ->
          Some (given, expected)
      | _ -> None)
    (fun (given, expected) ->
      Invalid_degree_strictly_less_than_expected {given; expected})

type slot = bytes

let polynomial_from_shards cryptobox shards =
  match Cryptobox.polynomial_from_shards cryptobox shards with
  | Ok p -> Ok p
  | Error
      ( `Not_enough_shards msg
      | `Shard_index_out_of_range msg
      | `Invalid_shard_length msg ) ->
      Error [Merging_failed msg]

let commit cryptobox polynomial =
  match Cryptobox.commit cryptobox polynomial with
  | Ok cm -> Ok cm
  | Error
      (`Invalid_degree_strictly_less_than_expected Cryptobox.{given; expected})
    ->
      Error [Invalid_degree_strictly_less_than_expected {given; expected}]

let save_shards store cryptobox commitment shards =
  let open Lwt_result_syntax in
  let*? polynomial = polynomial_from_shards cryptobox shards in
  let*? rebuilt_commitment = commit cryptobox polynomial in
  let*? () =
    if Cryptobox.Commitment.equal commitment rebuilt_commitment then Ok ()
    else Result_syntax.fail [Invalid_shards_commitment_association]
  in
  Store.(
    Shards.save_and_notify
      store.shard_store
      store.shards_watcher
      commitment
      shards)
  |> Errors.to_tzresult

let get_shard store commitment shard_id =
  let open Lwt_result_syntax in
  let* share = Store.Shards.read_value store (commitment, shard_id) in
  return {Cryptobox.share; index = shard_id}

let with_commitment_as_seq commitment shards_indices =
  Seq.map (fun index -> (commitment, index)) @@ List.to_seq shards_indices

let rev_fetched_values_as seq f init =
  Seq_s.E.fold_left
    (fun acc ((_commitment, shard_id), v) ->
      match v with
      | Error err -> Error err
      | Ok v -> Ok (f Cryptobox.{index = shard_id; share = v} acc))
    init
    seq

let get_shards store commitment shard_ids =
  let open Lwt_result_syntax in
  let res =
    Store.Shards.read_values store
    @@ with_commitment_as_seq commitment shard_ids
  in
  let* rev_list = rev_fetched_values_as res List.cons [] in
  return @@ List.rev rev_list

let get_slot cryptobox store commitment =
  let open Lwt_result_syntax in
  let {Cryptobox.number_of_shards; redundancy_factor; _} =
    Cryptobox.parameters cryptobox
  in
  let minimal_number_of_shards =
    assert (number_of_shards mod redundancy_factor = 0) ;
    number_of_shards / redundancy_factor
  in
  let rec loop acc shard_id remaining =
    if remaining <= 0 then return acc
    else if shard_id >= number_of_shards then
      let provided = minimal_number_of_shards - remaining in
      tzfail @@ Missing_shards {provided; required = minimal_number_of_shards}
    else
      let*! res = get_shard store commitment shard_id in
      match res with
      | Ok res -> loop (Seq.cons res acc) (shard_id + 1) (remaining - 1)
      | Error _ -> loop acc (shard_id + 1) remaining
  in
  let* shards = loop Seq.empty 0 minimal_number_of_shards in
  let*? polynomial = polynomial_from_shards cryptobox shards in
  let slot = Cryptobox.polynomial_to_slot cryptobox polynomial in
  let*! () = Event.(emit fetched_slot (Bytes.length slot, Seq.length shards)) in
  return slot

let get_slot_pages cryptobox store commitment =
  let open Lwt_result_syntax in
  let dal_parameters = Cryptobox.parameters cryptobox in
  let* slot = get_slot cryptobox store commitment in
  (* The slot size `Bytes.length slot` should be an exact multiple of `page_size`.
     If this is not the case, we throw an `Illformed_pages` error.
  *)
  (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3900
     Implement `Bytes.chunk_bytes` which returns a list of bytes directly. *)
  let*? pages =
    String.chunk_bytes
      dal_parameters.page_size
      slot
      ~error_on_partial_chunk:(TzTrace.make Illformed_pages)
  in
  return @@ List.map (fun page -> String.to_bytes page) pages
