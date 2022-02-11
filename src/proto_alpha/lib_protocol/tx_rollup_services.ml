(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
open Alpha_context

let custom_root =
  (RPC_path.(open_root / "context" / "tx_rollup")
    : RPC_context.t RPC_path.context)

module S = struct
  let state =
    RPC_service.get_service
      ~description:"Access the state of a rollup."
      ~query:RPC_query.empty
      ~output:Tx_rollup_state.encoding
      RPC_path.(custom_root /: Tx_rollup.rpc_arg / "state")

  let inbox =
    RPC_service.get_service
      ~description:"Get the inbox of a transaction rollup"
      ~query:RPC_query.empty
      ~output:Tx_rollup_inbox.encoding
      RPC_path.(custom_root /: Tx_rollup.rpc_arg / "inbox")

  let commitments =
    RPC_service.get_service
      ~description:"."
      ~query:RPC_query.empty
      ~output:Tx_rollup_commitments.encoding
      RPC_path.(custom_root /: Tx_rollup.rpc_arg / "commitments")
end

let register () =
  let open Services_registration in
  opt_register1 ~chunked:false S.state (fun ctxt tx_rollup () () ->
      Tx_rollup_state.find ctxt tx_rollup >|=? snd) ;
  opt_register1 ~chunked:false S.inbox (fun ctxt tx_rollup () () ->
      Tx_rollup_inbox.find ctxt tx_rollup ~level:`Current >|=? snd) ;
  register1 ~chunked:false S.commitments (fun ctxt tx_rollup () () ->
      let level = (Level.current ctxt).level in
      Tx_rollup_commitments.get_commitments ctxt tx_rollup level >|=? snd)

let state ctxt block tx_rollup =
  RPC_context.make_call1 S.state ctxt block tx_rollup () ()

let inbox ctxt block tx_rollup =
  RPC_context.make_call1 S.inbox ctxt block tx_rollup () ()

let commitments ctxt block tx_rollup =
  RPC_context.make_call1 S.commitments ctxt block tx_rollup () ()

let current_tezos_head () =
  RPC_service.get_service
    ~description:"Get the current head stored in the node"
    ~query:RPC_query.empty
    ~output:(Data_encoding.option Block_hash.encoding)
    RPC_path.(open_root / "tezos_head")

let current_inbox () =
  RPC_service.get_service
    ~description:"Get the current inbox stored in the node"
    ~query:RPC_query.empty
    ~output:(Data_encoding.option Tx_rollup_inbox.encoding)
    RPC_path.(open_root / "current_inbox")
