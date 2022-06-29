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

let find ctxt level = Storage.Dal.Slot_headers.find ctxt level

let finalize_current_slots ctxt =
  let current_level = Raw_context.current_level ctxt in
  let slots = Raw_context.Dal.candidates ctxt in
  match slots with
  | [] -> Lwt.return ctxt
  | _ :: _ -> Storage.Dal.Slot_headers.add ctxt current_level.level slots

let compute_available_slots ctxt slots =
  let fold_available_slots available_slots slot =
    if Raw_context.Dal.is_slot_available ctxt slot.Dal_slot_repr.index then
      Dal_endorsement_repr.commit available_slots slot.Dal_slot_repr.index
    else available_slots
  in
  List.fold_left fold_available_slots Dal_endorsement_repr.empty slots

let finalize_pending_slots ctxt =
  let current_level = Raw_context.current_level ctxt in
  let Constants_parametric_repr.{dal; _} = Raw_context.constants ctxt in
  match Raw_level_repr.(sub current_level.level dal.endorsement_lag) with
  | None -> return (ctxt, Dal_endorsement_repr.empty)
  | Some level_endorsed -> (
      Storage.Dal.Slot_headers.find ctxt level_endorsed >>=? function
      | None -> return (ctxt, Dal_endorsement_repr.empty)
      | Some slots ->
          let available_slots = compute_available_slots ctxt slots in
          (* DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3112

             At this point, available slots can be integrated into
             SCORU inboxes *)
          Storage.Dal.Slot_headers.remove ctxt level_endorsed >>= fun ctxt ->
          return (ctxt, available_slots))
