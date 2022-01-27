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

module Term = struct
  let average_block_path_arg =
    let open Cmdliner in
    let doc = "Path to the file with description of the average block" in
    let docv = "AVERAGE_BLOCK_PATH" in
    Arg.(value & opt (some string) None & info ["average-block"] ~docv ~doc)

  let tezt_args =
    let open Cmdliner in
    let doc =
      "Extra arguments after -- to be passed directly to Tezt. Contains `-i` \
       by default to display info log level."
    in
    let docv = "TEZT_ARGS" in
    Arg.(value & pos_all string [] & info [] ~docv ~doc)

  let previous_count_arg =
    let open Cmdliner in
    let doc =
      "The number of previously recorded samples that must be compared to the \
       result of this benchmark"
    in
    let docv = "PREVIOUS_SAMPLE_COUNT" in
    Arg.(
      value & opt int 10 & info ["regression-previous-sample-count"] ~docv ~doc)

  let term =
    let process average_block_path tezt_args previous_count =
      (* We are going to need to call the client stress test command here in
         order to get an estimation of gas cost of various transactions that
         stress test uses. This functionality is also protocol-dependent, so
         we need to start a node, too. Hence we use the tezt network to spin
         up the network. *)
      (try Cli.init ~args:("-i" :: tezt_args) ()
       with Arg.Help help_str ->
         Format.eprintf "%s@." help_str ;
         exit 0) ;
      Long_test.init () ;
      let executors = Long_test.[x86_executor1] in
      Long_test.register
        ~__FILE__
        ~title:"tezos_tps_gas"
        ~tags:[]
        ~timeout:(Long_test.Minutes 1)
        ~executors
        (fun () ->
          Long_test.measure_and_check_regression_lwt
            ~previous_count
            ~minimum_previous_count:previous_count
            ~stddev:false
            ~repeat:1
            "tps_evaluation"
          @@ estimate_gas_tps ~average_block_path) ;
      Test.run () ;
      `Ok ()
    in
    let open Cmdliner.Term in
    ret (const process $ average_block_path_arg $ tezt_args $ previous_count_arg)
end

module Manpage = struct
  let command_description = "Estimate TPS based on gas"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Term.info ~doc:command_description ~man "gas-tps"
end

let cmd = (Term.term, Manpage.info)
