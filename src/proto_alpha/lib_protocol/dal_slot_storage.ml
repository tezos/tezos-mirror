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

let find_slot_headers ctxt level = Storage.Dal.Slot.Headers.find ctxt level

let finalize_current_slot_headers ctxt =
  let current_level = Raw_context.current_level ctxt in
  let slot_headers = Raw_context.Dal.candidates ctxt in
  match slot_headers with
  | [] -> Lwt.return ctxt
  | _ :: _ -> Storage.Dal.Slot.Headers.add ctxt current_level.level slot_headers

let compute_attested_slot_headers ~is_slot_attested seen_slot_headers =
  let open Dal_slot_repr in
  let fold_attested_slots (rev_attested_slot_headers, attestation) slot =
    if is_slot_attested slot then
      ( slot :: rev_attested_slot_headers,
        Dal_attestation_repr.commit attestation slot.Header.id.index )
    else (rev_attested_slot_headers, attestation)
  in
  let rev_attested_slot_headers, bitset =
    List.fold_left
      fold_attested_slots
      ([], Dal_attestation_repr.empty)
      seen_slot_headers
  in
  (List.rev rev_attested_slot_headers, bitset)

let get_slot_headers_history ctxt =
  let open Lwt_result_syntax in
  let+ slots_history = Storage.Dal.Slot.History.find ctxt in
  match slots_history with
  | None -> Dal_slot_repr.History.genesis
  | Some slots_history -> slots_history

let update_skip_list ctxt ~confirmed_slot_headers ~level_attested
    ~number_of_slots =
  let open Lwt_result_syntax in
  let* slots_history = get_slot_headers_history ctxt in
  let*? slots_history =
    Dal_slot_repr.History.add_confirmed_slot_headers_no_cache
      ~number_of_slots
      slots_history
      level_attested
      confirmed_slot_headers
  in
  let*! ctxt = Storage.Dal.Slot.History.add ctxt slots_history in
  return ctxt

let finalize_pending_slot_headers ctxt ~number_of_slots =
  let open Lwt_result_syntax in
  let {Level_repr.level = raw_level; _} = Raw_context.current_level ctxt in
  let Constants_parametric_repr.{dal; _} = Raw_context.constants ctxt in
  match Raw_level_repr.(sub raw_level dal.attestation_lag) with
  | None -> return (ctxt, Dal_attestation_repr.empty)
  | Some level_attested ->
      let* seen_slots = find_slot_headers ctxt level_attested in
      let*! ctxt = Storage.Dal.Slot.Headers.remove ctxt level_attested in
      let* ctxt, attestation, confirmed_slot_headers =
        match seen_slots with
        | None -> return (ctxt, Dal_attestation_repr.empty, [])
        | Some seen_slots ->
            let attested_slot_headers, attestation =
              let is_slot_attested slot =
                Raw_context.Dal.is_slot_index_attested
                  ctxt
                  slot.Dal_slot_repr.Header.id.index
              in
              compute_attested_slot_headers ~is_slot_attested seen_slots
            in
            return (ctxt, attestation, attested_slot_headers)
      in
      let* ctxt =
        update_skip_list
          ctxt
          ~confirmed_slot_headers
          ~level_attested
          ~number_of_slots
      in
      return (ctxt, attestation)
