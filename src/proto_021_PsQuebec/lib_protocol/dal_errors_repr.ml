(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech <contact@trili.tech>                        *)
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

type error +=
  | Dal_feature_disabled
  | Dal_slot_index_above_hard_limit of {given : int; limit : int}
  | Dal_publish_commitment_invalid_index of {
      given : Dal_slot_index_repr.t;
      maximum : Dal_slot_index_repr.t;
    }
  | Dal_publish_commitment_candidate_with_low_fees of {
      proposed_fees : Tez_repr.t;
    }
  | Dal_attestation_size_limit_exceeded of {maximum_size : int; got : int}
  | Dal_publish_commitment_duplicate of {slot_header : Dal_slot_repr.Header.t}
  | Dal_publish_commitment_invalid_proof of {
      commitment : Dal.commitment;
      commitment_proof : Dal.commitment_proof;
    }
  | Dal_data_availibility_attester_not_in_committee of {
      attester : Signature.Public_key_hash.t;
      level : Raw_level_repr.t;
      slot : Slot_repr.t;
    }
  | Dal_cryptobox_error of {explanation : string}
  | Dal_register_invalid_slot_header of {
      length : int;
      slot_header : Dal_slot_repr.Header.t;
    }

let () =
  let open Data_encoding in
  let description =
    "Data-availability layer will be enabled in a future proposal."
  in
  register_error_kind
    `Permanent
    ~id:"operation.dal_disabled"
    ~title:"DAL is disabled"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.unit
    (function Dal_feature_disabled -> Some () | _ -> None)
    (fun () -> Dal_feature_disabled) ;

  let description = "Slot index above hard limit" in
  register_error_kind
    `Permanent
    ~id:"dal_slot_index_negative_orabove_hard_limit"
    ~title:"DAL slot index negative or above hard limit"
    ~description
    ~pp:(fun ppf (given, limit) ->
      Format.fprintf
        ppf
        "%s (given %Ld): Maximum allowed %Ld."
        description
        given
        limit)
    (obj2 (req "given" Data_encoding.int64) (req "limit" Data_encoding.int64))
    (function
      | Dal_slot_index_above_hard_limit {given; limit} ->
          Some (Int64.of_int given, Int64.of_int limit)
      | _ -> None)
    (fun (given, limit) ->
      Dal_slot_index_above_hard_limit
        {given = Int64.to_int given; limit = Int64.to_int limit}) ;
  let description = "Bad index for slot header" in
  register_error_kind
    `Permanent
    ~id:"dal_publish_commitment_invalid_index"
    ~title:"DAL slot header invalid index"
    ~description
    ~pp:(fun ppf (given, maximum) ->
      Format.fprintf
        ppf
        "%s: Given %a. Maximum %a."
        description
        Dal_slot_index_repr.pp
        given
        Dal_slot_index_repr.pp
        maximum)
    (obj2
       (req "given" Dal_slot_index_repr.encoding)
       (req "got" Dal_slot_index_repr.encoding))
    (function
      | Dal_publish_commitment_invalid_index {given; maximum} ->
          Some (given, maximum)
      | _ -> None)
    (fun (given, maximum) ->
      Dal_publish_commitment_invalid_index {given; maximum}) ;
  (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3114
     Better error message *)
  let description = "Slot header with too low fees" in
  register_error_kind
    `Permanent
    ~id:"dal_publish_commitment_with_low_fees"
    ~title:"DAL slot header with low fees"
    ~description
    ~pp:(fun ppf proposed ->
      Format.fprintf
        ppf
        "%s: Proposed fees %a."
        description
        Tez_repr.pp
        proposed)
    (obj1 (req "proposed" Tez_repr.encoding))
    (function
      | Dal_publish_commitment_candidate_with_low_fees {proposed_fees} ->
          Some proposed_fees
      | _ -> None)
    (fun proposed_fees ->
      Dal_publish_commitment_candidate_with_low_fees {proposed_fees}) ;
  let description = "The attestation for data availability is a too big" in
  register_error_kind
    `Permanent
    ~id:"dal_attestation_size_limit_exceeded"
    ~title:"DAL attestation exceeded the limit"
    ~description
    ~pp:(fun ppf (maximum_size, got) ->
      Format.fprintf
        ppf
        "%s: Maximum is %d. Got %d."
        description
        maximum_size
        got)
    (obj2 (req "maximum_size" int31) (req "got" int31))
    (function
      | Dal_attestation_size_limit_exceeded {maximum_size; got} ->
          Some (maximum_size, got)
      | _ -> None)
    (fun (maximum_size, got) ->
      Dal_attestation_size_limit_exceeded {maximum_size; got}) ;
  (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3114
     Better error message. *)
  let description = "A slot header for this slot was already proposed" in
  register_error_kind
    `Permanent
    ~id:"dal_publish_commitment_duplicate"
    ~title:"DAL publish slot header duplicate"
    ~description
    ~pp:(fun ppf _proposed -> Format.fprintf ppf "%s" description)
    (obj1 (req "proposed" Dal_slot_repr.Header.encoding))
    (function
      | Dal_publish_commitment_duplicate {slot_header} -> Some slot_header
      | _ -> None)
    (fun slot_header -> Dal_publish_commitment_duplicate {slot_header}) ;
  let description = "The slot header's commitment proof does not check" in
  register_error_kind
    `Permanent
    ~id:"dal_publish_commitment_invalid_proof"
    ~title:"DAL publish slot header invalid proof"
    ~description
    ~pp:(fun ppf _proposed -> Format.fprintf ppf "%s" description)
    (obj2
       (req "commitment" Dal.Commitment.encoding)
       (req "commitment_proof" Dal.Commitment_proof.encoding))
    (function
      | Dal_publish_commitment_invalid_proof {commitment; commitment_proof} ->
          Some (commitment, commitment_proof)
      | _ -> None)
    (fun (commitment, commitment_proof) ->
      Dal_publish_commitment_invalid_proof {commitment; commitment_proof}) ;
  register_error_kind
    `Permanent
    ~id:"Dal_data_availibility_attester_not_in_committee"
    ~title:"The attester is not part of the DAL committee for this level"
    ~description:"The attester is not part of the DAL committee for this level"
    ~pp:(fun ppf (attester, level, slot) ->
      Format.fprintf
        ppf
        "The attester %a, with slot %a, is not part of the DAL committee for \
         the level %a."
        Signature.Public_key_hash.pp
        attester
        Slot_repr.pp
        slot
        Raw_level_repr.pp
        level)
    Data_encoding.(
      obj3
        (req "attester" Signature.Public_key_hash.encoding)
        (req "level" Raw_level_repr.encoding)
        (req "slot" Slot_repr.encoding))
    (function
      | Dal_data_availibility_attester_not_in_committee {attester; level; slot}
        ->
          Some (attester, level, slot)
      | _ -> None)
    (fun (attester, level, slot) ->
      Dal_data_availibility_attester_not_in_committee {attester; level; slot}) ;
  register_error_kind
    `Permanent
    ~id:"dal_cryptobox_error"
    ~title:"DAL cryptobox error"
    ~description:"Error occurred while initialising the cryptobox"
    ~pp:(fun ppf e ->
      Format.fprintf ppf "DAL cryptobox initialisation error: %s" e)
    (obj1 (req "error" (string Plain)))
    (function
      | Dal_cryptobox_error {explanation} -> Some explanation | _ -> None)
    (fun explanation -> Dal_cryptobox_error {explanation}) ;
  register_error_kind
    `Permanent
    ~id:"dal_register_invalid_slot"
    ~title:"Dal register invalid slot"
    ~description:
      "Attempt to register a slot which is invalid (the index is out of \
       bounds)."
    ~pp:(fun ppf (length, slot) ->
      Format.fprintf
        ppf
        "The slot provided is invalid. Slot index should be between 0 and %d. \
         Found: %a."
        length
        Dal_slot_index_repr.pp
        slot.Dal_slot_repr.Header.id.index)
    Data_encoding.(
      obj2
        (req "length" int31)
        (req "slot_header" Dal_slot_repr.Header.encoding))
    (function
      | Dal_register_invalid_slot_header {length; slot_header} ->
          Some (length, slot_header)
      | _ -> None)
    (fun (length, slot_header) ->
      Dal_register_invalid_slot_header {length; slot_header})
