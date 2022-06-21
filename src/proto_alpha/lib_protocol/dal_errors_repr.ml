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
  | Dal_slot_index_above_hard_limit
  | Dal_subscribe_rollup_invalid_slot_index of {
      given : Dal_slot_repr.Index.t;
      maximum : Dal_slot_repr.Index.t;
    }
  | Dal_endorsement_unexpected_size of {expected : int; got : int}
  | Dal_publish_slot_header_invalid_index of {
      given : Dal_slot_repr.Index.t;
      maximum : Dal_slot_repr.Index.t;
    }
  | Dal_publish_slot_header_candidate_with_low_fees of {
      proposed_fees : Tez_repr.t;
    }
  | Dal_endorsement_size_limit_exceeded of {maximum_size : int; got : int}
  | Dal_publish_slot_header_duplicate of {slot : Dal_slot_repr.t}
  | Dal_rollup_already_registered_to_slot of
      (Sc_rollup_repr.t * Dal_slot_repr.Index.t)
  | Dal_requested_subscriptions_at_future_level of
      (Raw_level_repr.t * Raw_level_repr.t)

let () =
  let open Data_encoding in
  let description = "Bad index for slot" in
  register_error_kind
    `Permanent
    ~id:"dal_subscribe_rollup_invalid_slot_index"
    ~title:"DAL slot invalid index for subscribing sc rollup"
    ~description
    ~pp:(fun ppf (given, maximum) ->
      Format.fprintf
        ppf
        "%s: Given %a. Maximum %a."
        description
        Dal_slot_repr.Index.pp
        given
        Dal_slot_repr.Index.pp
        maximum)
    (obj2
       (req "given" Dal_slot_repr.Index.encoding)
       (req "got" Dal_slot_repr.Index.encoding))
    (function
      | Dal_subscribe_rollup_invalid_slot_index {given; maximum} ->
          Some (given, maximum)
      | _ -> None)
    (fun (given, maximum) ->
      Dal_subscribe_rollup_invalid_slot_index {given; maximum}) ;
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

  let description =
    "The endorsement for data availability has a different size"
  in
  register_error_kind
    `Permanent
    ~id:"dal_endorsement_unexpected_size"
    ~title:"DAL endorsement unexpected size"
    ~description
    ~pp:(fun ppf (expected, got) ->
      Format.fprintf ppf "%s: Expected %d. Got %d." description expected got)
    (obj2 (req "expected_size" int31) (req "got" int31))
    (function
      | Dal_endorsement_unexpected_size {expected; got} -> Some (expected, got)
      | _ -> None)
    (fun (expected, got) -> Dal_endorsement_unexpected_size {expected; got}) ;
  let description = "Slot index above hard limit" in
  register_error_kind
    `Permanent
    ~id:"dal_slot_index_negative_orabove_hard_limit"
    ~title:"DAL slot index negative or above hard limit"
    ~description
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "%s: Maximum allowed %a."
        description
        Dal_slot_repr.Index.pp
        Dal_slot_repr.Index.max_value)
    Data_encoding.unit
    (function Dal_slot_index_above_hard_limit -> Some () | _ -> None)
    (fun () -> Dal_slot_index_above_hard_limit) ;
  let description = "Bad index for slot header" in
  register_error_kind
    `Permanent
    ~id:"dal_publish_slot_header_invalid_index"
    ~title:"DAL slot header invalid index"
    ~description
    ~pp:(fun ppf (given, maximum) ->
      Format.fprintf
        ppf
        "%s: Given %a. Maximum %a."
        description
        Dal_slot_repr.Index.pp
        given
        Dal_slot_repr.Index.pp
        maximum)
    (obj2
       (req "given" Dal_slot_repr.Index.encoding)
       (req "got" Dal_slot_repr.Index.encoding))
    (function
      | Dal_publish_slot_header_invalid_index {given; maximum} ->
          Some (given, maximum)
      | _ -> None)
    (fun (given, maximum) ->
      Dal_publish_slot_header_invalid_index {given; maximum}) ;
  (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3114
     Better error message *)
  let description = "Slot header with too low fees" in
  register_error_kind
    `Permanent
    ~id:"dal_publish_slot_header_with_low_fees"
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
      | Dal_publish_slot_header_candidate_with_low_fees {proposed_fees} ->
          Some proposed_fees
      | _ -> None)
    (fun proposed_fees ->
      Dal_publish_slot_header_candidate_with_low_fees {proposed_fees}) ;
  let description = "The endorsement for data availability is a too big" in
  register_error_kind
    `Permanent
    ~id:"dal_endorsement_size_limit_exceeded"
    ~title:"DAL endorsement exceeded the limit"
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
      | Dal_endorsement_size_limit_exceeded {maximum_size; got} ->
          Some (maximum_size, got)
      | _ -> None)
    (fun (maximum_size, got) ->
      Dal_endorsement_size_limit_exceeded {maximum_size; got}) ;
  (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3114
     Better error message. *)
  let description = "A slot header for this slot was already proposed" in
  register_error_kind
    `Permanent
    ~id:"dal_publish_slot_heade_duplicate"
    ~title:"DAL publish slot header duplicate"
    ~description
    ~pp:(fun ppf _proposed -> Format.fprintf ppf "%s" description)
    (obj1 (req "proposed" Dal_slot_repr.encoding))
    (function
      | Dal_publish_slot_header_duplicate {slot} -> Some slot | _ -> None)
    (fun slot -> Dal_publish_slot_header_duplicate {slot}) ;
  register_error_kind
    `Permanent
    ~id:"Dal_rollup_already_subscribed_to_slot"
    ~title:"DAL rollup already subscribed to slot"
    ~description
    ~pp:(fun ppf (rollup, slot_index) ->
      Format.fprintf
        ppf
        "Rollup %a is already subscribed to data availability slot %a"
        Sc_rollup_repr.pp
        rollup
        Dal_slot_repr.Index.pp
        slot_index)
    Data_encoding.(
      obj2
        (req "rollup" Sc_rollup_repr.encoding)
        (req "slot_index" Dal_slot_repr.Index.encoding))
    (function
      | Dal_rollup_already_registered_to_slot (rollup, slot_index) ->
          Some (rollup, slot_index)
      | _ -> None)
    (fun (rollup, slot_index) ->
      Dal_rollup_already_registered_to_slot (rollup, slot_index)) ;
  let description =
    "Requested List of subscribed rollups to slot at a future level"
  in
  register_error_kind
    `Temporary
    ~id:"Dal_requested_subscriptions_at_future_level"
    ~title:"Requested list of subscribed dal slots at a future level"
    ~description
    ~pp:(fun ppf (current_level, future_level) ->
      Format.fprintf
        ppf
        "The list of subscribed dal slot indices has been requested for level \
         %a, but the current level is %a"
        Raw_level_repr.pp
        future_level
        Raw_level_repr.pp
        current_level)
    Data_encoding.(
      obj2
        (req "current_level" Raw_level_repr.encoding)
        (req "future_level" Raw_level_repr.encoding))
    (function
      | Dal_requested_subscriptions_at_future_level (current_level, future_level)
        ->
          Some (current_level, future_level)
      | _ -> None)
    (fun (current_level, future_level) ->
      Dal_requested_subscriptions_at_future_level (current_level, future_level))
