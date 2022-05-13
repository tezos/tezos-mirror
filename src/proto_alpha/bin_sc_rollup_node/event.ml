(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

  let section = ["sc_rollup_node"]

  let starting_node =
    declare_0
      ~section
      ~name:"starting_sc_rollup_node"
      ~msg:"Starting the smart contract rollup node"
      ~level:Notice
      ()

  let shutdown_node =
    declare_1
      ~section
      ~name:"stopping_sc_rollup_node"
      ~msg:"Stopping the smart contract rollup node"
      ~level:Notice
      ("exit_status", Data_encoding.int8)

  let node_is_ready =
    declare_2
      ~section
      ~name:"sc_rollup_node_is_ready"
      ~msg:"The smart contract rollup node is listening to {addr}:{port}"
      ~level:Notice
      ("addr", Data_encoding.string)
      ("port", Data_encoding.uint16)

  let rollup_exists =
    declare_2
      ~section
      ~name:"sc_rollup_node_knows_its_rollup"
      ~msg:
        "The smart contract rollup node is interacting with rollup {addr} of \
         kind {kind}"
      ~level:Notice
      ("addr", Protocol.Alpha_context.Sc_rollup.Address.encoding)
      ("kind", Data_encoding.string)
end

let starting_node = Simple.(emit starting_node)

let shutdown_node exit_status = Simple.(emit shutdown_node exit_status)

let node_is_ready ~rpc_addr ~rpc_port =
  Simple.(emit node_is_ready (rpc_addr, rpc_port))

let rollup_exists ~addr ~kind =
  let kind = Protocol.Sc_rollups.string_of_kind kind in
  Simple.(emit rollup_exists (addr, kind))
