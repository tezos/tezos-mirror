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

include Slot_manager_legacy

type error += Invalid_slot_size of {provided : int; expected : int}

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.invalid_slot_size"
    ~title:"Invalid slot size"
    ~description:"The size of the given slot is not as expected"
    ~pp:(fun ppf (provided, expected) ->
      Format.fprintf
        ppf
        "The size (%d) of the given slot is not as expected (%d)"
        provided
        expected)
    Data_encoding.(obj2 (req "provided" int31) (req "expected" int31))
    (function
      | Invalid_slot_size {provided; expected} -> Some (provided, expected)
      | _ -> None)
    (fun (provided, expected) -> Invalid_slot_size {provided; expected})

(* Used wrapper functions on top of Cryptobox. *)

let polynomial_from_slot cryptobox slot =
  let open Result_syntax in
  match Cryptobox.polynomial_from_slot cryptobox slot with
  | Ok r -> return r
  | Error (`Slot_wrong_size _) ->
      let open Cryptobox in
      let provided = Bytes.length slot in
      let {slot_size = expected; _} = parameters cryptobox in
      tzfail @@ Invalid_slot_size {provided; expected}

let commitment_should_exist node_store cryptobox commitment =
  let open Lwt_result_syntax in
  let*! exists =
    Store.Legacy.exists_slot_by_commitment node_store cryptobox commitment
  in
  if not exists then fail `Not_found else return_unit

(* Main functions *)

let add_slots node_store slot cryptobox =
  let open Lwt_result_syntax in
  let*? polynomial = polynomial_from_slot cryptobox slot in
  let commitment = Cryptobox.commit cryptobox polynomial in
  let*! exists =
    Store.Legacy.exists_slot_by_commitment node_store cryptobox commitment
  in
  let*! () =
    if exists then Lwt.return_unit
    else
      Store.Legacy.add_slot_by_commitment node_store cryptobox slot commitment
  in
  return commitment

let add_slot_id node_store cryptobox commitment slot_id =
  let open Lwt_result_syntax in
  let* () = commitment_should_exist node_store cryptobox commitment in
  let*! () =
    Store.Legacy.associate_slot_id_with_commitment node_store commitment slot_id
  in
  return_unit

let find_slot node_store cryptobox commitment =
  let open Lwt_result_syntax in
  let*! slot_opt =
    Store.Legacy.find_slot_by_commitment node_store cryptobox commitment
  in
  match slot_opt with
  | None -> fail `Not_found
  | Some slot_content -> return slot_content

let store_slot_headers ~block_level ~block_hash slot_headers node_store =
  Store.Legacy.add_slot_headers ~block_level ~block_hash slot_headers node_store

let update_selected_slot_headers_statuses ~block_level ~attestation_lag
    attested_slots unattested_slots node_store =
  Store.Legacy.update_selected_slot_headers_statuses
    ~block_level
    attested_slots
    unattested_slots
    node_store
    ~attestation_lag
