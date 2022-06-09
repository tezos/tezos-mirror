(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Every function of this file should check the feature flag. *)

open Alpha_context

type error += Dal_feature_disabled

let () =
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
    (fun () -> Dal_feature_disabled)

let assert_dal_feature_enabled ctxt =
  let open Constants in
  let Parametric.{dal = {feature_enable; _}; _} = parametric ctxt in
  error_unless Compare.Bool.(feature_enable = true) Dal_feature_disabled

let only_if_dal_feature_enabled ctxt ~default f =
  let open Constants in
  let Parametric.{dal = {feature_enable; _}; _} = parametric ctxt in
  if feature_enable then f ctxt else default ctxt

type error +=
  | Dal_endorsement_size_limit_exceeded of {maximum_size : int; got : int}

let () =
  let open Data_encoding in
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
      Dal_endorsement_size_limit_exceeded {maximum_size; got})

let validate_data_availability ctxt data_availability =
  assert_dal_feature_enabled ctxt >>? fun () ->
  let open Constants in
  let Parametric.{dal = {number_of_slots; _}; _} = parametric ctxt in
  let maximum_size =
    Dal.Endorsement.expected_size_in_bits ~max_index:(number_of_slots - 1)
  in
  let size = Dal.Endorsement.occupied_size_in_bits data_availability in
  error_unless
    Compare.Int.(size <= maximum_size)
    (Dal_endorsement_size_limit_exceeded {maximum_size; got = size})

let apply_data_availability ctxt data_availability ~endorser =
  assert_dal_feature_enabled ctxt >>?= fun () ->
  let shards = Dal.Endorsement.shards ctxt ~endorser in
  Dal.Endorsement.record_available_shards ctxt data_availability shards
  |> return

type error +=
  | Dal_publish_slot_header_invalid_index of {given : int; maximum : int}

let () =
  let open Data_encoding in
  let description = "Bad index for slot header" in
  register_error_kind
    `Permanent
    ~id:"dal_publish_slot_header_invalid_index"
    ~title:"DAL slot header invalid index"
    ~description
    ~pp:(fun ppf (given, maximum) ->
      Format.fprintf ppf "%s: Given %d. Maximum %d." description given maximum)
    (obj2 (req "given" int31) (req "got" int31))
    (function
      | Dal_publish_slot_header_invalid_index {given; maximum} ->
          Some (given, maximum)
      | _ -> None)
    (fun (given, maximum) ->
      Dal_publish_slot_header_invalid_index {given; maximum})

let validate_publish_slot_header ctxt Dal.Slot.{index; _} =
  assert_dal_feature_enabled ctxt >>? fun () ->
  let open Constants in
  let Parametric.{dal = {number_of_slots; _}; _} = parametric ctxt in
  error_unless
    Compare.Int.(0 <= index && index < number_of_slots)
    (Dal_publish_slot_header_invalid_index
       {given = index; maximum = number_of_slots - 1})

type error += Dal_publish_slot_header_duplicate of {slot : Dal.Slot.t}

(* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3114

   Better error message *)
let () =
  let open Data_encoding in
  let description = "A slot header for this slot was already proposed" in
  register_error_kind
    `Permanent
    ~id:"dal_publish_slot_heade_duplicate"
    ~title:"DAL publish slot header duplicate"
    ~description
    ~pp:(fun ppf _proposed -> Format.fprintf ppf "%s" description)
    (obj1 (req "proposed" Dal.Slot.encoding))
    (function
      | Dal_publish_slot_header_duplicate {slot} -> Some slot | _ -> None)
    (fun slot -> Dal_publish_slot_header_duplicate {slot})

let apply_publish_slot_header ctxt slot =
  assert_dal_feature_enabled ctxt >>? fun () ->
  Dal.Slot.register_slot ctxt slot >>? fun (ctxt, updated) ->
  if updated then ok ctxt else error (Dal_publish_slot_header_duplicate {slot})

let dal_finalisation ctxt =
  only_if_dal_feature_enabled
    ctxt
    ~default:(fun ctxt -> return (ctxt, None))
    (fun ctxt ->
      Dal.Slot.finalize_current_slots ctxt >>= fun ctxt ->
      Dal.Slot.finalize_pending_slots ctxt >|=? fun (ctxt, slot_availability) ->
      (ctxt, Some slot_availability))
