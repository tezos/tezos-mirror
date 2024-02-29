(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
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

open Rollup_services
open Transaction_format

module MakeBackend (Base : sig
  val base : Uri.t
end) : Services_backend_sig.Backend = struct
  module READER = struct
    let read path =
      call_service
        ~base:Base.base
        durable_state_value
        ((), Block_id.Head)
        {key = path}
        ()
  end

  module TxEncoder = struct
    type transactions = {
      raw : string list;
      delayed : Ethereum_types.Delayed_transaction.t list;
    }

    type messages = string list

    let encode_transactions ~smart_rollup_address
        ~transactions:({raw = transactions; _} : transactions) =
      let open Result_syntax in
      let* rev_hashes, messages =
        List.fold_left_e
          (fun (tx_hashes, to_publish) tx_raw ->
            let* tx_hash, messages =
              make_encoded_messages ~smart_rollup_address tx_raw
            in
            return (tx_hash :: tx_hashes, to_publish @ messages))
          ([], [])
          transactions
      in
      return (List.rev rev_hashes, messages)
  end

  module Publisher = struct
    type messages = TxEncoder.messages

    let publish_messages ~timestamp:_ ~smart_rollup_address:_ ~messages =
      let open Lwt_result_syntax in
      (* The injection's service returns a notion of L2 message hash (defined
         by the rollup node) used to track the message's injection in the batcher.
         We do not wish to follow the message's inclusion, and thus, ignore
         the resulted hash. *)
      let* _answer =
        call_service ~base:Base.base batcher_injection () () messages
      in
      return_unit
  end

  module SimulatorBackend = struct
    let simulate_and_read ~input =
      let open Lwt_result_syntax in
      let* json = call_service ~base:Base.base simulation () () input in
      let eval_result =
        Data_encoding.Json.destruct Simulation.Encodings.eval_result json
      in
      match eval_result.insights with
      | [data] -> return data
      | _ -> failwith "Inconsistent simulation results"
  end

  let inject_kernel_upgrade ~payload:_ = Lwt_result_syntax.return_unit

  let inject_sequencer_upgrade ~payload:_ = Lwt_result_syntax.return_unit
end

module Make (Base : sig
  val base : Uri.t
end) =
  Services_backend_sig.Make (MakeBackend (Base))
