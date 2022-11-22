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

module Simple = struct
  include Internal_event.Simple

  let section = ["sc_rollup_node"; "inbox"]

  let starting =
    declare_0
      ~section
      ~name:"sc_rollup_node_inbox_starting"
      ~msg:"Starting inbox tracker of the smart rollup node"
      ~level:Notice
      ()

  let stopping =
    declare_0
      ~section
      ~name:"sc_rollup_node_inbox_stopping"
      ~msg:"Stopping inbox tracker of the smart rollup node"
      ~level:Notice
      ()

  let get_messages =
    declare_3
      ~section
      ~name:"sc_rollup_node_layer_1_get_messages"
      ~msg:
        "Fetching {number_of_messages} messages from block {hash} at level \
         {level}"
      ~level:Notice
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)
      ("number_of_messages", Data_encoding.int32)
end

let starting = Simple.(emit starting)

let stopping = Simple.(emit stopping)

let get_messages hash level number_of_messages =
  Simple.(emit get_messages (hash, level, Int32.of_int number_of_messages))
