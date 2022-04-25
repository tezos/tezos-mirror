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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2880 
   Add corresponding .mli file. *)

module Simple = struct
  include Internal_event.Simple

  let section = ["sc_rollup_node"; "layer_1"]

  let starting =
    declare_0
      ~section
      ~name:"sc_rollup_node_layer_1_starting"
      ~msg:"Starting layer 1 tracker of the smart contract rollup node"
      ~level:Notice
      ()

  let stopping =
    declare_0
      ~section
      ~name:"sc_rollup_node_layer_1_stopping"
      ~msg:"Stopping layer 1 tracker of the smart contract rollup node"
      ~level:Notice
      ()

  let setting_new_head =
    declare_2
      ~section
      ~name:"sc_rollup_node_layer_1_new_head"
      ~msg:"Setting layer 1 head to {hash} at level {level}"
      ~level:Notice
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)

  let rollback =
    declare_2
      ~section
      ~name:"sc_rollup_node_layer_1_rollback"
      ~msg:"Rolling back layer 1 head to {hash} at level {level}"
      ~level:Notice
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)

  let reacting_to_reorganization =
    declare_2
      ~section
      ~name:"sc_rollup_node_layer_1_reorganization"
      ~msg:
        "Reacting to layer 1 reorganization: rollback to {rollback_hash}, \
         process {new_blocks}"
      ~level:Notice
      ("rollback_hash", Block_hash.encoding)
      ("new_blocks", Data_encoding.list Block_hash.encoding)

  let new_head_processed =
    declare_2
      ~section
      ~name:"sc_rollup_node_layer_1_new_head_processed"
      ~msg:"Finished processing layer 1 head {hash} at level {level}"
      ~level:Notice
      ("hash", Block_hash.encoding)
      ("level", Data_encoding.int32)
end

let starting = Simple.(emit starting)

let stopping = Simple.(emit stopping)

let setting_new_head hash level = Simple.(emit setting_new_head (hash, level))

let new_head_processed hash level =
  Simple.(emit new_head_processed (hash, level))

let reacting_to_reorganization h hs =
  Simple.(emit reacting_to_reorganization (h, hs))

let rollback h hs = Simple.(emit rollback (h, hs))
