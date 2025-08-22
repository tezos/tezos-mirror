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

open Protocol
open Alpha_context

let update_metrics ~protocol_metadata (fitness : Tezos_base.Fitness.t)
    update_metrics_callback =
  Alpha_context.Fitness.round_from_raw fitness
  >>?= (fun round ->
  let proto_metrics =
    match
      Data_encoding.Binary.of_bytes_opt
        Protocol.block_header_metadata_encoding
        protocol_metadata
    with
    | None ->
        (* this is the case of the genesis block and the activation block
                  in a sandbox environment *)
        None
    | Some protocol_data ->
        let cycle =
          Int32.to_float (Cycle.to_int32 protocol_data.level_info.cycle)
        in
        let consumed_gas =
          Z.to_float
            (Gas.Arith.integral_to_z
               (Gas.Arith.ceil protocol_data.consumed_gas))
        in
        Some (cycle, consumed_gas)
  in
  match proto_metrics with
  | Some (cycle, consumed_gas) ->
      return
      @@ update_metrics_callback
           ~cycle
           ~consumed_gas
           ~round:(Int32.to_float (Round.to_int32 round))
  | None -> return_unit)
  >|= Environment.wrap_tzresult
  >>= function
  | _ -> Lwt.return_unit
