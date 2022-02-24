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

  let commitment_query =
    let open RPC_query in
    query (fun offset -> offset)
    |+ opt_field ~descr:"offset" "offset" RPC_arg.int (fun t -> t)
    |> seal

  let commitment =
    RPC_service.get_service
      ~description:"Return the commitment for a level, if any"
      ~query:commitment_query
      ~output:
        (Data_encoding.option
           Tx_rollup_commitment.Submitted_commitment.encoding)
      RPC_path.(custom_root /: Tx_rollup.rpc_arg / "commitment")

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
  opt_register1 ~chunked:false S.inbox (fun ctxt tx_rollup () () ->
      Tx_rollup_inbox.find ctxt tx_rollup ~level:`Current >|=? snd) ;
  register1 ~chunked:false S.commitment (fun ctxt tx_rollup offset () ->
      let level =
        match offset with
        | None -> Level.current ctxt
        | Some offset -> (
            if Compare.Int.(offset < 0) then
              failwith "offset should not be negative." ;
            match Level.sub ctxt (Level.current ctxt) offset with
            | None ->
                failwith "the offset is not valid: The block level is negative."
            | Some level -> level)
      in
      Tx_rollup_commitment.get_commitment ctxt tx_rollup level.level >|=? snd) ;
  register2
    ~chunked:false
    S.pending_bonded_commitments
    (fun ctxt tx_rollup pkh () () ->
      Tx_rollup_commitment.pending_bonded_commitments ctxt tx_rollup pkh
      >|=? fun (_, count) -> Int32.of_int count)

let state ctxt block tx_rollup =
  RPC_context.make_call1 S.state ctxt block tx_rollup () ()

let inbox ctxt block tx_rollup =
  RPC_context.make_call1 S.inbox ctxt block tx_rollup () ()

let commitment ctxt block ?offset tx_rollup =
  RPC_context.make_call1 S.commitment ctxt block tx_rollup offset ()
