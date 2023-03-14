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

module Publish_slot_header = struct
  type t = {
    published_level : Raw_level_repr.t;
    slot_index : Dal_slot_index_repr.t;
    commitment : Dal_slot_repr.Commitment.t;
    commitment_proof : Dal_slot_repr.Commitment_proof.t;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {published_level; slot_index; commitment; commitment_proof} ->
        (published_level, slot_index, commitment, commitment_proof))
      (fun (published_level, slot_index, commitment, commitment_proof) ->
        {published_level; slot_index; commitment; commitment_proof})
      (obj4
         (req "published_level" Raw_level_repr.encoding)
         (req "slot_index" Dal_slot_index_repr.encoding)
         (req "commitment" Dal_slot_repr.Commitment.encoding)
         (req "commitment_proof" Dal_slot_repr.Commitment_proof.encoding))

  let pp fmt {published_level; slot_index; commitment; commitment_proof = _} =
    Format.fprintf
      fmt
      "published_level: %a, slot_index: %a, commitment: %a"
      Raw_level_repr.pp
      published_level
      Dal_slot_index_repr.pp
      slot_index
      Dal.Commitment.pp
      commitment

  let check_level ~current_level {published_level; _} =
    let open Result_syntax in
    let* () =
      error_when
        Raw_level_repr.(current_level < published_level)
        (Dal_errors_repr.Dal_publish_slot_header_future_level
           {provided = published_level; expected = current_level})
    in
    let* () =
      error_when
        Raw_level_repr.(current_level > published_level)
        (Dal_errors_repr.Dal_publish_slot_header_past_level
           {provided = published_level; expected = current_level})
    in
    return_unit

  let slot_header ~cryptobox ~number_of_slots ~current_level
      ({published_level; slot_index; commitment; commitment_proof} as operation)
      =
    let open Result_syntax in
    let* max_slot_index = Dal_slot_index_repr.of_int (number_of_slots - 1) in
    let* () =
      error_unless
        Compare.Int.(
          Dal_slot_index_repr.compare slot_index max_slot_index <= 0
          && Dal_slot_index_repr.compare slot_index Dal_slot_index_repr.zero
             >= 0)
        (Dal_errors_repr.Dal_publish_slot_header_invalid_index
           {given = slot_index; maximum = max_slot_index})
    in
    let* () = check_level ~current_level operation in
    let* proof_ok =
      Dal_slot_repr.Header.verify_commitment
        cryptobox
        operation.commitment
        operation.commitment_proof
    in
    let* () =
      error_unless
        proof_ok
        (Dal_errors_repr.Dal_publish_slot_header_invalid_proof
           {commitment; commitment_proof})
    in
    return
      Dal_slot_repr.Header.
        {id = {published_level; index = slot_index}; commitment}
end
