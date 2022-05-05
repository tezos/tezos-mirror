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
      ~output:Data_encoding.(option Tx_rollup_inbox.encoding)
      RPC_path.(
        custom_root /: Tx_rollup.rpc_arg / "inbox" /: Tx_rollup_level.rpc_arg)

  let commitment =
    RPC_service.get_service
      ~description:"Return the commitment for a level, if any"
      ~query:RPC_query.empty
      ~output:
        Data_encoding.(
          option Tx_rollup_commitment.Submitted_commitment.encoding)
      RPC_path.(
        custom_root /: Tx_rollup.rpc_arg / "commitment"
        /: Tx_rollup_level.rpc_arg)

  let pending_bonded_commitments =
    RPC_service.get_service
      ~description:
        "Get the number of pending bonded commitments for a pkh on a rollup"
      ~query:RPC_query.empty
      ~output:Data_encoding.int32
      RPC_path.(
        custom_root /: Tx_rollup.rpc_arg / "pending_bonded_commitments"
        /: Signature.Public_key_hash.rpc_arg)
end

let register () =
  let open Services_registration in
  opt_register1 ~chunked:false S.state (fun ctxt tx_rollup () () ->
      Tx_rollup_state.find ctxt tx_rollup >|=? snd) ;
  register2 ~chunked:false S.inbox (fun ctxt tx_rollup level () () ->
      Tx_rollup_inbox.find ctxt level tx_rollup >|=? snd) ;
  register2 ~chunked:false S.commitment (fun ctxt tx_rollup level () () ->
      Tx_rollup_state.get ctxt tx_rollup >>=? fun (ctxt, state) ->
      Tx_rollup_commitment.find ctxt tx_rollup state level
      >|=? fun (_, commitment) -> commitment) ;
  register2
    ~chunked:false
    S.pending_bonded_commitments
    (fun ctxt tx_rollup pkh () () ->
      Tx_rollup_commitment.pending_bonded_commitments ctxt tx_rollup pkh
      >|=? fun (_, count) -> Int32.of_int count)

let state ctxt block tx_rollup =
  RPC_context.make_call1 S.state ctxt block tx_rollup () ()

let inbox ctxt block tx_rollup level =
  RPC_context.make_call2 S.inbox ctxt block tx_rollup level () ()

let commitment ctxt block tx_rollup level =
  RPC_context.make_call2 S.commitment ctxt block tx_rollup level () ()
