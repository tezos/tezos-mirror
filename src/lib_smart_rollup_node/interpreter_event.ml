(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

module Simple = struct
  include Internal_event.Simple

  let section = ["smart_rollup_node"; "interpreter"]

  let transitioned_pvm =
    declare_4
      ~section
      ~name:"smart_rollup_node_interpreter_transitioned_pvm"
      ~msg:
        "Transitioned PVM at inbox level {inbox_level} to {state_hash} at tick \
         {ticks} with {num_messages} messages"
      ~level:Info
      ("inbox_level", Data_encoding.int32)
      ("state_hash", State_hash.encoding)
      ("ticks", Data_encoding.z)
      ("num_messages", Data_encoding.int31)

  let intended_failure =
    declare_4
      ~section
      ~name:"smart_rollup_node_interpreter_intended_failure"
      ~msg:
        "Intended failure at level {level} for message indexed {message_index} \
         and at the tick {message_tick} of message processing (internal = \
         {internal})"
      ~level:Notice
      ("level", Data_encoding.int31)
      ("message_index", Data_encoding.int31)
      ("message_tick", Data_encoding.int64)
      ("internal", Data_encoding.bool)

  let missing_pre_image =
    declare_1
      ~section
      ~name:"smart_rollup_node_interpreter_missing_pre_image"
      ~msg:"The pre-image {hash} is missing locally"
      ~level:Info
      ("hash", Data_encoding.string)
      ~pp1:Format.pp_print_string

  let fetched_incorrect_pre_image =
    declare_2
      ~section
      ~name:"smart_rollup_node_interpreter_fetched_incorrect_pre_image"
      ~msg:
        "Fetched pre-image for {content_hash} instead of {expected_hash} from \
         pre-image-service"
      ~level:Error
      ("expected_hash", Data_encoding.string)
      ("content_hash", Data_encoding.string)
      ~pp1:Format.pp_print_string
      ~pp2:Format.pp_print_string
end

(** [transition_pvm inbox_level hash tick n] emits the event that a PVM
   transition is leading to the state of the given [hash] by
   processing [n] messages at [tick]. *)
let transitioned_pvm inbox_level hash tick num_messages =
  Simple.(emit transitioned_pvm (inbox_level, hash, tick, num_messages))

(** [intended_failure level message_index message_tick internal] emits
   the event that an intended failure has been injected at some given
   [level], during the processing of a given [message_index] and at
   tick [message_tick] during this message processing. [internal] is
   [true] if the failure is injected in a PVM internal
   step. [internal] is [false] if the failure is injected in the input
   to the PVM. *)
let intended_failure ~level ~message_index ~message_tick ~internal =
  Simple.(emit intended_failure (level, message_index, message_tick, internal))

let missing_pre_image ~hash = Simple.(emit missing_pre_image) hash

let fetched_incorrect_pre_image ~expected_hash ~content_hash =
  Simple.(emit fetched_incorrect_pre_image) (expected_hash, content_hash)
