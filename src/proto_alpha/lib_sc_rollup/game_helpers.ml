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

let new_dissection ~state_hash_from_tick ~default_number_of_sections
    ~(start_chunk : Game.dissection_chunk)
    ~(our_stop_chunk : Game.dissection_chunk) =
  let open Lwt_result_syntax in
  let max_number_of_sections = Z.of_int default_number_of_sections in
  let trace_length =
    Z.succ (Tick.distance our_stop_chunk.tick start_chunk.tick)
  in
  let number_of_sections = Z.min max_number_of_sections trace_length in
  let rem = Z.(rem trace_length number_of_sections) in
  let first_section_length, section_length =
    if Compare.Z.(trace_length < max_number_of_sections) then
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
  let rec make rev_dissection k tick : Game.dissection_chunk list tzresult Lwt.t
      =
    if Z.(equal k (pred number_of_sections)) then
      return
      @@ List.rev
           (({
               state_hash = our_stop_chunk.state_hash;
               tick = our_stop_chunk.tick;
             }
              : Game.dissection_chunk)
           :: rev_dissection)
    else
      let* state_hash = state_hash_from_tick tick in
      let next_tick = Tick.jump tick section_length in
      make ({state_hash; tick} :: rev_dissection) (Z.succ k) next_tick
  in
  make
    [{state_hash = start_chunk.state_hash; tick = start_chunk.tick}]
    Z.one
    (Tick.jump start_chunk.tick first_section_length)
