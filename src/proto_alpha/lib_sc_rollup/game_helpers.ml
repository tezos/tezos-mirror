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

open Protocol.Alpha_context.Sc_rollup

let default_new_dissection ~default_number_of_sections
    ~(start_chunk : Game.dissection_chunk)
    ~(our_stop_chunk : Game.dissection_chunk) =
  let max_number_of_sections = Z.of_int default_number_of_sections in
  let trace_length = Tick.distance our_stop_chunk.tick start_chunk.tick in
  let number_of_sections = Z.min max_number_of_sections trace_length in
  let rem = Z.(rem trace_length number_of_sections) in
  let first_section_length, section_length =
    if Compare.Z.(trace_length <= max_number_of_sections) then
      (* In this case, every section is of length one. *)
      Z.(one, one)
    else
      let section_length = Z.(max one (div trace_length number_of_sections)) in
      if Compare.Z.(section_length = Z.one) && not Compare.Z.(rem = Z.zero) then
        (* If we put [section_length] in this situation, we will most likely
           have a very long last section. *)
        (rem, section_length)
      else (section_length, section_length)
  in
  (* [k] is the number of sections in [rev_dissection]. *)
  let rec make rev_dissection k tick =
    if Z.(equal k number_of_sections) then List.rev rev_dissection
    else
      let next_tick = Tick.jump tick section_length in
      make (tick :: rev_dissection) (Z.succ k) next_tick
  in
  make [] Z.one (Tick.jump start_chunk.tick first_section_length)

let make_dissection ~state_of_tick ~state_hash_of_eval_result ?start_state
    ~start_chunk ~our_stop_chunk ticks =
  let rec make_dissection_aux start_state ticks acc =
    let open Lwt_result_syntax in
    match ticks with
    | tick :: rst ->
        let* eval_state = state_of_tick ?start_state tick in
        let state_hash = Option.map state_hash_of_eval_result eval_state in
        let chunk = Dissection_chunk.{tick; state_hash} in
        make_dissection_aux eval_state rst (chunk :: acc)
    | [] -> return @@ List.rev (our_stop_chunk :: acc)
  in
  make_dissection_aux start_state ticks [start_chunk]

module Wasm = struct
  let new_dissection ~default_number_of_sections ~start_chunk ~our_stop_chunk =
    let open Dissection_chunk in
    let dist = Tick.distance start_chunk.tick our_stop_chunk.tick in
    let ticks_per_snapshot = Wasm_2_0_0PVM.ticks_per_snapshot in
    if Compare.Z.(dist <= ticks_per_snapshot) then
      (*
         There are two cases that require us to fall back to the
         default behavior.  Either [start_chunk] is not aligned on the
         size of a snapshot (meaning the PVM is stuck) or the distance
         between the start and stop chunk is lesser than a snapshot,
         meaning we have already found the kernel_run invocation we
         were looking for.
      *)
      default_new_dissection
        ~default_number_of_sections
        ~start_chunk
        ~our_stop_chunk
    else
      let is_stop_chunk_aligned =
        Compare.Z.(
          Z.rem (Tick.to_z our_stop_chunk.tick) ticks_per_snapshot = Z.zero)
      in
      let final_tick =
        Tick.of_z
          Z.(
            div (Tick.to_z our_stop_chunk.tick) ticks_per_snapshot
            * ticks_per_snapshot)
      in
      let dist = Tick.distance start_chunk.tick final_tick in
      let max_number_of_sections = Z.(div dist ticks_per_snapshot) in
      let number_of_sections =
        Z.min
          (Z.of_int
             (* If [is_stop_chunk_aligned] is false, we allocate one
                sections for the surplus. *)
             (if is_stop_chunk_aligned then default_number_of_sections
             else default_number_of_sections - 1))
          max_number_of_sections
      in

      (* [go remaining_sections last_tick dist] tries to compute
         [remaining_sections] sections as evenly as possible, starting
         from [last_tick] and covering [dist] ticks. *)
      let rec go remaining_sections last_tick dist rev_acc =
        (* The last section is created by [make_dissection] when it
           adds the [stop_chunk]. *)
        if Z.(remaining_sections <= one) then
          let rev_acc =
            (* If [is_stop_chunk_aligned] is false, we insert the
               last snapshot point. *)
            if is_stop_chunk_aligned then rev_acc else final_tick :: rev_acc
          in
          List.rev rev_acc
        else
          (*
             We compute the length of the next section of the
             dissection as the maximum size such that if we would give
             this number to all remaining sections, we would not
             consume more than [dist] ticks. This is ensured by
             [Z.div], which computes a lower rounding.
          *)
          let section_len =
            Z.(
              dist
              / (ticks_per_snapshot * remaining_sections)
              * ticks_per_snapshot)
          in
          let next_tick = Tick.jump last_tick section_len in
          let next_dist = Z.(dist - section_len) in
          (*
             There are two cases to consider here.

               1. Either [dist] was a multiple of
                  [ticks_per_snapshot]. In that case, the same
                  [section_len] will be computed in all subsequent
                  calls of [go].
               2. Or [dist] was not a multiple of
                  [ticks_per_snapshot]. In that case, the next
                  [section_len] in float will be slightly higher than
                  the previous one, because it will benefit from the
                  unconsumed reminder of the previous computation,
                  until enough is left that [dist] becomes a
                  multiplier of [ticks_per_snapshot]. In that case, we
                  will fall back to case 1.

             Take case of dividing 60 into 32 chunks.

               - [remaining_sections = 60], [remaining_sections = 32], and
                 [section_len = 1] (60 / 32 ~= 1.87)
               - [remaining_sections = 59], [remaining_sections = 31], and
                 [section_len = 1] (59 / 31 ~= 1.90)
               - [remaining_sections = 58], [remaining_sections = 30], and
                 [section_len = 1] (58 / 30 ~= 1.93)
               - [remaining_sections = 57], [remaining_sections = 29], and
                 [section_len = 1] (57 / 29 ~= 1.97)
               - [remaining_sections = 56], [remaining_sections = 28], and
                 [section_len = 2] (56 / 28 = 2)

               All remaining sections will be of length 2.
          *)
          go
            Z.(pred remaining_sections)
            next_tick
            next_dist
            (next_tick :: rev_acc)
      in
      go number_of_sections start_chunk.tick dist []
end
