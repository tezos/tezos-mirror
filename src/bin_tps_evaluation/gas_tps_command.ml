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

open Constants

let estimate_gas_tps ~average_block_path () =
  Protocol.write_parameter_file
    ~base:(Either.right (protocol, Some protocol_constants))
    []
  >>= fun parameter_file ->
  Client.init_with_protocol
    ~nodes_args:Node.[Connections 0; Synchronisation_threshold 0]
    ~parameter_file
    ~timestamp_delay:0.0
    `Client
    ~protocol
    ()
  >>= fun (node, client) ->
  Average_block.load average_block_path >>= fun average_block ->
  Average_block.check_for_unknown_smart_contracts average_block >>= fun () ->
  Baker.init ~protocol ~delegates node client >>= fun _baker ->
  Log.info "Originating smart contracts" ;
  Client.stresstest_originate_smart_contracts originating_bootstrap client
  >>= fun () ->
  Log.info "Waiting to reach the next level" ;
  Node.wait_for_level node 2 >>= fun _ ->
  (* It is important to give the chain time to include the smart contracts
     we have originated before we run gas estimations. *)
  Client.stresstest_estimate_gas client >>= fun transaction_costs ->
  let average_transaction_cost =
    Gas.average_transaction_cost transaction_costs average_block
  in
  Log.info "Average transaction cost: %d" average_transaction_cost ;
  let gas_tps =
    Gas.deduce_tps ~protocol ~protocol_constants ~average_transaction_cost ()
  in
  Log.info "Gas TPS: %d" gas_tps ;
  Node.terminate ~kill:true node >>= fun () ->
  Lwt.return @@ float_of_int gas_tps

let register () =
  Long_test.register
    ~__FILE__
    ~title:"tezos_gas_tps"
    ~tags:["tezos_gas_tps"]
    ~timeout:(Long_test.Minutes 60)
    ~executors:Long_test.[x86_executor1]
    (fun () ->
      let average_block_path =
        Cli.get ~default:None (fun s -> Some (Some s)) "average-block"
      in
      let previous_count =
        Cli.get_int ~default:10 "regression-previous-sample-count"
      in
      Long_test.measure_and_check_regression_lwt
        ~previous_count
        ~minimum_previous_count:previous_count
        ~stddev:false
        ~repeat:1
        "tps_evaluation"
      @@ estimate_gas_tps ~average_block_path)
