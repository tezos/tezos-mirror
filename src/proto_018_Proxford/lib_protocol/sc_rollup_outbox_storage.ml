(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

let level_index ctxt level =
  let max_active_levels =
    Constants_storage.sc_rollup_max_active_outbox_levels ctxt
  in
  Int32.rem (Raw_level_repr.to_int32 level) max_active_levels

let record_applied_message ctxt rollup level ~message_index =
  let open Lwt_result_syntax in
  (* Check that the 0 <= message index < maximum number of outbox messages per
     level. *)
  let*? () =
    let max_outbox_messages_per_level =
      Constants_storage.sc_rollup_max_outbox_messages_per_level ctxt
    in
    error_unless
      Compare.Int.(
        0 <= message_index && message_index < max_outbox_messages_per_level)
      Sc_rollup_errors.Sc_rollup_invalid_outbox_message_index
  in
  let level_index = level_index ctxt level in
  let* ctxt, level_and_bitset_opt =
    Storage.Sc_rollup.Applied_outbox_messages.find (ctxt, rollup) level_index
  in
  let*? bitset, ctxt =
    let open Result_syntax in
    let* bitset, ctxt =
      match level_and_bitset_opt with
      | Some (existing_level, bitset)
        when Raw_level_repr.(existing_level = level) ->
          (* The level at the index is the same as requested. Fail if the
             message has been applied already. *)
          let* already_applied = Bitset.mem bitset message_index in
          let* () =
            error_when
              already_applied
              Sc_rollup_errors.Sc_rollup_outbox_message_already_applied
          in
          return (bitset, ctxt)
      | Some (existing_level, _bitset)
        when Raw_level_repr.(level < existing_level) ->
          tzfail Sc_rollup_errors.Sc_rollup_outbox_level_expired
      | Some _ | None ->
          (* The old level is outdated or there is no previous bitset at
             this index. *)
          return (Bitset.empty, ctxt)
    in
    let* bitset = Bitset.add bitset message_index in
    return (bitset, ctxt)
  in
  let+ ctxt, size_diff, _is_new =
    Storage.Sc_rollup.Applied_outbox_messages.add
      (ctxt, rollup)
      level_index
      (level, bitset)
  in
  (Z.of_int size_diff, ctxt)
