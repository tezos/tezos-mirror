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

(** The protocol we are using. *)
let protocol = Protocol.Alpha

(** The constants we are using. *)
let constants = Protocol.Constants_mainnet

(** The level at which the benchmark starts. We wait till level 3 because we
   need to inject transactions that target already decided blocks. In
   Tenderbake, a block is decided when there are 2 blocks on top of it. We
   cannot target genesis (level 0) because it is not yet in the right
   protocol, thus we wait till level 1 is decided, i.e. we want level 3. *)
let benchmark_starting_level = 3

(** Print out the file at [path] to the stdout. *)
let print_out_file path =
  let open Lwt_io in
  with_file ~mode:Input path (fun ic ->
      read ic >>= fun str ->
      write stdout str >>= fun () ->
      write stdout "\n\n" >>= fun () -> flush stdout)

(** Get a list of hashes of the given number of most recent blocks. *)
let get_blocks blocks_total client =
  let path = ["chains"; "main"; "blocks"] in
  let query_string = [("length", Int.to_string blocks_total)] in
  Client.rpc ~query_string GET path client >|= fun json ->
  List.map JSON.as_string (JSON.as_list (JSON.geti 0 json))

(** Get the number of applied transactions in the block with the given
    hash. *)
let get_total_applied_transactions_for_block block client =
  (*
    N.B. Grouping of the operations by validation passes:
    - 0: consensus
    - 1: governance (voting)
    - 2: anonymous (denounciations)
    - 3: manager operations

    We are interested in 3, so we select that.
   *)
  let path = ["chains"; "main"; "blocks"; block; "operation_hashes"; "3"] in
  Client.rpc GET path client >|= fun json -> List.length (JSON.as_list json)

(** The entry point of the benchmark. *)
let run_benchmark ~accounts_total ~blocks_total ~average_block_path () =
  let all_bootstraps =
    List.filter
      (fun {Account.alias; _} -> alias <> "activator")
      Constant.all_secret_keys
  in
  let delegates =
    List.take_n
      accounts_total
      (List.map (fun {Account.alias; _} -> alias) all_bootstraps)
  in
  Format.printf "Tezos TPS benchmark@\n" ;
  Format.printf "Protocol: %s@\n" (Protocol.name protocol) ;
  Format.printf "Total number of accounts to use: %d@\n" accounts_total ;
  Format.printf "Blocks to bake: %d@." blocks_total ;
  Protocol.write_parameter_file
    ~base:(Either.right (protocol, Some constants))
    []
  >>= fun parameter_file ->
  Average_block.load average_block_path >>= fun average_block ->
  let transaction_cost = Gas.average_transaction_cost average_block in
  Format.printf "Average transaction cost: %d@\n" transaction_cost ;
  Format.printf "Spinning up the network...@." ;
  (* For now we disable operations precheck, but ideally we should
     pre-populate enough bootstrap accounts and do 1 transaction per
     account per block. *)
  Client.init_with_protocol
    ~nodes_args:
      Node.
        [
          Connections 0; Synchronisation_threshold 0; Disable_operations_precheck;
        ]
    ~parameter_file
    ~timestamp_delay:0.0
    `Client
    ~protocol
    ()
  >>= fun (node, client) ->
  Baker.init ~protocol ~delegates node client >>= fun _baker ->
  Format.printf "Using the parameter file: %s@.@." parameter_file ;
  print_out_file parameter_file >>= fun () ->
  Format.printf "Waiting to reach level %d@." benchmark_starting_level ;
  Node.wait_for_level node benchmark_starting_level >>= fun _ ->
  let bench_start = Unix.gettimeofday () in
  Format.printf "The benchmark has been started@." ;
  (* It is important to use a good estimate of max possible TPS that is
     theoretically achievable. If we send operations with lower TPS than
     this we risk obtaining a sub-estimated value for TPS. If we use a
     higher TPS than the maximal possible we risk to saturate the mempool
     and again obtain a less-than-perfect estimation in the end. *)
  let tps_of_injection =
    Gas.deduce_tps ~protocol ~constants ~transaction_cost ()
  in
  let client_stresstest_process =
    Client.spawn_stresstest
      ~tps:
        tps_of_injection
        (* The stresstest command allows a small probability of creating
           new accounts along the way. We do not want that, so we set it to
           0. *)
      ~fresh_probability:0.0
      client
  in
  Node.wait_for_level node (benchmark_starting_level + blocks_total)
  >>= fun _level ->
  Process.kill client_stresstest_process ;
  let bench_end = Unix.gettimeofday () in
  let bench_duration = bench_end -. bench_start in
  Format.printf
    "Produced %d block(s) in %.2f seconds@."
    blocks_total
    bench_duration ;
  get_blocks blocks_total client >>= fun produced_block_hashes ->
  let total_applied_transactions = ref 0 in
  let handle_one_block block_hash =
    get_total_applied_transactions_for_block block_hash client
    >|= fun applied_transactions ->
    total_applied_transactions :=
      !total_applied_transactions + applied_transactions ;
    Format.printf "%s -> %d@." block_hash applied_transactions
  in
  List.iter_s handle_one_block (List.rev produced_block_hashes) >>= fun () ->
  Format.printf "Total applied transactions: %d@." !total_applied_transactions ;
  let empirical_tps =
    Float.of_int !total_applied_transactions /. bench_duration
  in
  Format.printf "TPS of injection: %d@\n" tps_of_injection ;
  Format.printf "Empirical TPS: %f@." empirical_tps ;
  Node.terminate ~kill:true node

module Term = struct
  let accounts_total_arg =
    let open Cmdliner in
    let doc = "The number of bootstrap accounts to use in the benchmark" in
    let docv = "ACCOUNTS_TOTAL" in
    Arg.(value & opt int 5 & info ["accounts-total"] ~docv ~doc)

  let blocks_total_arg =
    let open Cmdliner in
    let doc = "The number of blocks to bake during the benchmark" in
    let docv = "BLOCKS_TOTAL" in
    Arg.(value & opt int 10 & info ["blocks-total"] ~docv ~doc)

  let average_block_path_arg =
    let open Cmdliner in
    let doc = "Path to the file with description of the average block" in
    let docv = "AVERAGE_BLOCK_PATH" in
    Arg.(value & opt (some string) None & info ["average-block"] ~docv ~doc)

  let tezt_args =
    let open Cmdliner in
    let doc = "Extra arguments after -- to be passed directly to Tezt" in
    let docv = "TEZT_ARGS" in
    Arg.(value & pos_all string [] & info [] ~docv ~doc)

  let term =
    let process accounts_total blocks_total average_block_path tezt_args =
      (try Cli.init ~args:tezt_args ()
       with Arg.Help help_str ->
         Format.eprintf "%s@." help_str ;
         exit 0) ;
      Test.register
        ~__FILE__
        ~title:"tezos_tps_benchmark"
        ~tags:[]
        (run_benchmark ~accounts_total ~blocks_total ~average_block_path) ;
      Test.run () ;
      `Ok ()
    in
    let open Cmdliner.Term in
    ret
      (const process $ accounts_total_arg $ blocks_total_arg
     $ average_block_path_arg $ tezt_args)
end

module Manpage = struct
  let command_description =
    "Run the benchmark and print out the results on stdout"

  let description = [`S "DESCRIPTION"; `P command_description]

  let man = description

  let info = Cmdliner.Term.info ~doc:command_description ~man "benchmark-tps"
end

let cmd = (Term.term, Manpage.info)
