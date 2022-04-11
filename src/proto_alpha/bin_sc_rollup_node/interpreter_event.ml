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

open Protocol.Alpha_context.Sc_rollup

module Simple = struct
  include Internal_event.Simple

  let section = ["sc_rollup_node"; "interpreter"]

  let transitioned_pvm =
    declare_3
      ~section
      ~name:"sc_rollup_node_interpreter_transitioned_pvm"
      ~msg:
        "Transitioned PVM to {state_hash} at tick {ticks} with {num_messages} \
         messages"
      ~level:Notice
      ("state_hash", State_hash.encoding)
      ("ticks", Tick.encoding)
      ("num_messages", Data_encoding.z)
end

let transitioned_pvm state num_messages =
  let open Lwt_syntax in
  let* hash = Arith_pvm.state_hash state in
  let* ticks = Arith_pvm.get_tick state in
  Simple.(emit transitioned_pvm (hash, ticks, num_messages))
