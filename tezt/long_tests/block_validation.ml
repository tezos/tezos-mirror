(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(* Testing
   -------
   Component: Block_validation
   Invocation: dune exec tezt/long_tests/main.exe -- --file block_validation.ml
   Subject: Benchmarking the validation of blocks
*)

(** This module contains benchmarks that are used to prevent the regression
    of performance in the block validation processing.

    It works by running a node with the replay command on specific blocks
    or batch of blocks, using node events to extract the time taken by their
    validation.

    Each benchmark is run several times in a row to get a good approximation
    and reduce the noise induced by system calls, scheduling of Lwt, garbage
    collection, etc...

    In order to get reliable and consistent measurement between different
    executions, the same datadir should be provided to the launched node.
    Since the validation is run against specific blocks, this datadir must
    have been bootstrapped at level 1479022 with at least 10000 nodes of
    context history.

    The datadir must be present in the folder configured inside the
    field [test_data_path] or the performance regression framework
    configuration as:
    - A standard datadir folder named ["mainnet-1479022-hist-10000-datadir"]
    - A tar.gz archive named ["mainnet-1479022-hist-10000-datadir.tar.gz"]
    if the datadir folder could not be found. In this case, the module
    will first uncompress the archive before running the benchmarks. *)

let apply_or_raise lazy_res f () =
  let* x = Lazy.force lazy_res in
  match x with Ok v -> f v | Error e -> failwith e

(** Extends the Tezos_time_measurement_runtime.Measurement
    module to add some helpers for measurement exploitation. *)
module Measurement = struct
  include Tezos_time_measurement_runtime.Measurement

  module Key = struct
    type t = key

    let compare = compare_keys
  end

  module Map = Lwtreslib.Bare.Map.Make (Key)

  (** [group_by_key measurements] groups the [measurements] of the given
      list by their [key], evaluating into a map in which each [Measurement.key]
      gives access to the list of corresponding [Measurement.value].

      The order of elements in the result is unspecified. *)
  let group_by_key =
    List.fold_left
      (fun map (k, v) ->
        let current = Map.find k map |> Option.value ~default:[] in
        Map.add k (v :: current) map)
      Map.empty

  (** [register_map_as_datapoints influxDB_measurement map] takes a
      [map] of [Measurement.value] list indexed by [Measurement.key]
      and creates a new influxDB datapoints serie inside the given
      [influxDB_measurement] for each of them.

      Since this data is mainly useful to see the evolution of each
      subpart (using dashboards), but contains micro-measurements,
      it is not a problem and better not to check regressions to avoid
      false positive detections as variance may be high. Regressions
      will be checked for the whole validation process. *)
  let register_map_as_datapoints influxDB_measurement =
    Map.iter_s (fun (label, _) durations ->
        let tags = [("step", label)] in
        let data_points =
          List.map
            (fun duration ->
              InfluxDB.data_point
                ~tags
                influxDB_measurement
                ("duration", Float duration))
            durations
        in
        Log.debug "Registering data points for step: %s" label ;
        return @@ List.iter Long_test.add_data_point data_points)
end

(** Defines functions to wait for the same event repeatedly. *)
module Event_looper = struct
  (** Waits for a raw event to occur several times and return all occurrences.

      Same as [Node.wait_for] but [wait_for_n node name filter n] waits for
      [n] raw events named [name] that passes the [filter] to occur.

      The resulting promise fulfills in a list of the result of each [filter]
      application. The order of the filter applications is preserved in the
      returned list. *)
  let wait_for_n ?where daemon name filter n =
    Node.wait_for ?where daemon name (Daemon.n_events n filter)

  (** Waits for a full event to occur several times and return the last occurrence.

      Same as [Node.wait_for_full] but [wait_for_the_nth_full node name filter n]
      waits for the [nth] full event named [name] that passes the [filter] to occur. *)
  let wait_for_the_nth_full ?where daemon name filter n =
    Node.wait_for_full ?where daemon name (Daemon.nth_event n filter)
end

(** Defines several features to handle an Octez node
    and wait for specific events. *)
module Node = struct
  (** Instantiates a [Tezt.Node] with no open connection. *)
  let create data_dir = Node.create ~data_dir [Node.Connections 0]

  (** [wait_for_last_event_time ?repeat event node] waits for [repeat]
      events matching the name [event] to occur inside the given
      [node] and evaluates in the timestamp of the last detected
      event.

      If [repeat] is not specified, it only waits for one event to
      occur. *)
  let wait_for_last_event_time ?(repeat = 1) event node =
    let json_filter json =
      Option.some
      @@ JSON.(json |-> "fd-sink-item.v0" |-> "time_stamp" |> as_float)
    in
    Event_looper.wait_for_the_nth_full node event json_filter repeat

  (** [wait_for_validation_start node] waits for the event of the
      [node] that represents the start of the block validation, and
      returns its timestamp. *)
  let wait_for_validation_start =
    wait_for_last_event_time "block_validation_start.v0"

  (** Like [wait_for_last_event_time] but for events representing
      the end of a validation. *)
  let wait_for_last_validation_end ?repeat =
    wait_for_last_event_time ?repeat "block_validation_end.v0"

  (** [wait_for_validation_subparts repeat node] waits for [repeat]
      time measurement events to occur and collects each measurement
      inside a map that associates each [Measurement.key] to a list
      of the corresponding [Measurement.value]s. *)
  let wait_for_validation_subparts ?(repeat = 1) node =
    let json_filter json =
      let open Measurement in
      try
        JSON.unannotate json
        |> Data_encoding.Json.destruct measurements_encoding
        |> Option.some
      with _ ->
        Test.fail "could not deserialize event: %s" @@ JSON.encode json
    in
    let event = "time_measurements.v0" in
    let* (measurements_per_iteration : Measurement.t list list) =
      Lwt.catch
        (fun () -> Event_looper.wait_for_n node event json_filter repeat)
        (function
          | Node.Terminated_before_event _ ->
              Test.fail
                "time_measurement event did not occur. Please check that the \
                 node was compiled with the tezos-time-measurement \
                 instrumentation backend activated. For example, run the \
                 following command:\n\
                 make enable-time-measurement"
          | exn -> raise exn)
    in
    List.flatten measurements_per_iteration
    |> Measurement.group_by_key |> return

  (** [replay_and_wait_for_termination blocks node] launches the [replay]
      command to start the validation of the given [blocks] on the given
      [node]. It then waits for the [node] to stop properly. *)
  let replay_and_wait_for_termination ?strict ?network blocks node =
    let node_cli_arguments =
      match network with None -> [] | Some network -> [Node.Network network]
    in
    let callback, resolver = Lwt.wait () in
    let on_terminate status =
      match Process.validate_status status with
      | Ok () -> Lwt.wakeup_later resolver ()
      | Error (`Invalid_status reason) ->
          failwith @@ Format.sprintf "Node %s" reason
    in
    let* () =
      Node.replay ~on_terminate ~blocks ?strict node node_cli_arguments
    in
    callback
end

module Validation = struct
  let dry_run blocks datadir =
    Log.debug "Performing a dry-run replay to initialize disk pages cache" ;
    let node = Node.create datadir in
    let* () = Node.replay_and_wait_for_termination blocks node in
    unit

  let run_and_measure blocks datadir measure_on_node =
    let node = Node.create datadir in
    (* Starting a promise that waits for the measurement to be complete
       before the node is started to be sure no event needed for measuring
       will be missed because of scheduling. *)
    let do_measure = measure_on_node node in
    (* - Binding to the first promise completion to wait for the
       events needed for measuring to occur.
       - Starting and waiting for a second promise to start the
       validation and wait for a proper termination of the node. *)
    let* measures = do_measure
    and* () = Node.replay_and_wait_for_termination blocks node in
    return measures

  let run_and_measure_mean_duration blocks datadir =
    let size = List.length blocks in
    (* Prevents dividision by 0*)
    if size = 0 then
      invalid_arg "run_and_measure_mean_duration: blocks size must be <> 0." ;
    run_and_measure blocks datadir @@ fun node ->
    let* start = Node.wait_for_validation_start node in
    let* stop = Node.wait_for_last_validation_end ~repeat:size node in
    return @@ ((stop -. start) /. float_of_int size)

  let run_and_measure_subparts_duration blocks datadir =
    run_and_measure blocks datadir @@ fun node ->
    let size = List.length blocks in
    let* _ = Node.wait_for_validation_start node in
    Node.wait_for_validation_subparts ~repeat:size node

  let replay ?network blocks datadir =
    let node = Node.create datadir in
    Lwt_seq.iter_s
      (fun range ->
        Node.replay_and_wait_for_termination ~strict:true ?network range node)
      (Lwt_seq.of_seq blocks)
end

(** Regroups the different benchmarks of the validation of blocks.

    To reduce the impact of noise that could be caused by different factors,
    following benchmarks will generally launch several validations in a row on the
    same node and measure the total time of its execution.

    Each benchmark execution is also repeated a certain number of times to be able
    to perform statistical analysis and compare the obtained result with previous
    runs. *)
module Benchmark = struct
  (* The following block consumed the highest gas (5871442981)
     between the 10000 blocks preceding block 1479022.

     As a reminder, if the benchmark needs other high consumed
     gas blocks later, blocks with highest gas are:
     - level: 1470469 -> consumed gas: 5668799660
     - level: 1478300 -> consumed gas: 5661805784
     - level: 1478984 -> consumed gas: 4769430818
     - level: 1478976 -> consumed gas: 3630404743
     - level: 1478964 -> consumed gas: 3001839414 *)
  let block_with_highest_gas = "1475742"

  let chunk_title = "shell.validation.block.chunk"

  let specific_title = "shell.validation.block." ^ block_with_highest_gas

  let subparts_title =
    "shell.validation.block.subpart." ^ block_with_highest_gas

  let subparts_steps =
    [
      "operations_parsing";
      "application_beginning";
      "operations_application";
      "block_finalization";
      "metadata_serde_check";
      "metadata_hash";
      "context_commitment";
    ]

  let influxDB_measurement = "block_validation"

  let mean_block_validation_duration ~repeat dry_run_blocks blocks datadir =
    if repeat <= 0 then
      invalid_arg "mean_block_validation_duration: repeat must be > 0" ;
    let* () = Validation.dry_run dry_run_blocks datadir in
    Long_test.measure_and_check_regression_lwt ~repeat influxDB_measurement
    @@ fun () -> Validation.run_and_measure_mean_duration blocks datadir

  (** [chunk_of_consecutive_blocks_total ~size ~repeat datadir]
      is a benchmark that measures the total time taken by the
      valitation of the [size] consecutive blocks that precede
      the chain head of the current [datadir].

      The benchmark is performed [repeat] times in a row to
      form a dataset of measurement on which statistical
      analysis can be performed. *)
  let chunk_of_consecutive_blocks_total ~size ~repeat datadir =
    if size <= 0 then
      invalid_arg "chunk_of_consecutive_blocks_total: size must be > 0" ;
    let open List in
    let init_blocks size offset =
      init size (fun i -> "head~" ^ string_of_int (offset - i - 1))
    in
    (* Dry running on the 50 first blocks seems to be enough as it
       permit to initiate most relevant disk pages and only takes
       1 minute when disk pages are not yet pre-loaded. *)
    let dry_run_blocks = init_blocks (min size 50) size in
    let blocks = init_blocks size size in
    mean_block_validation_duration ~repeat dry_run_blocks blocks datadir

  (** [batch_of_same_block_total ~size ~repeat block datadir]
      is a benchmark that measures the total time taken by the
      validation of the same [block] [size] times in a row.

      The benchmark is performed [repeat] times in a row to
      form a dataset of measurement on which statistical
      analysis can be performed. *)
  let batch_of_same_block_total ~size ~repeat block datadir =
    if size <= 0 then invalid_arg "batch_of_same_block_total: size must be > 0" ;
    let blocks = List.init size (fun _ -> block) in
    (* No need to dry-run the validation of the whole batch
       as it contains only one (repeated) block.*)
    mean_block_validation_duration ~repeat [block] blocks datadir

  (** [batch_of_same_block_total ~size block datadir]
      is a fine-grained benchmark that measures the time taken by each
      substep of the validation of the given [block]. The benchmark
      is performed [size] time to collect several measurements for each substep.
      Then, it performs some statistical analysis by substep. *)
  let batch_of_same_block_subparts ~size block datadir =
    if size <= 0 then
      invalid_arg "batch_of_same_block_subparts: size must be > 0" ;
    let blocks = List.init size (fun _ -> block) in
    (* No need to dry-run the validation of the whole batch
       as it contains only one (repeated) block.*)
    let* () = Validation.dry_run [block] datadir in
    let* measurements =
      Validation.run_and_measure_subparts_duration blocks datadir
    in
    Measurement.register_map_as_datapoints influxDB_measurement measurements
end

(** Provides an access to the set of data that will be used
    as input by this benchmarks. Providing the same data for several
    executions is mandatory to get reliable and consistent results. *)
module Fixture = struct
  (** The name of the datadir folder that must be found on the filesystem. *)
  let datadir_name = "mainnet-1479022-hist-10000-datadir"

  let error src dst msg =
    Error
      (Format.sprintf
         "Couldn't untar the source archive %s in the destination %s: %s"
         src
         dst
         msg)

  let untar src dst =
    let error = error src dst in
    try
      Log.debug
        "Running tar -xzf %s -C %s\n\
         Please, wait. This could take a few minutes..."
        src
        dst ;
      let status = Sys.command @@ Format.sprintf "tar -xzf %s -C %s" src dst in
      if status = 0 then Ok ()
      else
        error
          ("Process execution failed with status code " ^ string_of_int status)
    with Sys_error msg -> error msg

  (** Localises the datadir on the file system, uncompress it if needed and
      ensures it is up to date with last version of the store.
      Then, evaluates in a result of either the path of the datadir or
      the cause of the error if the datadir could not be found or uncompressed. *)
  let datadir () =
    let ( let*! ) = Lwt_result.bind in
    let dst = Long_test.test_data_path () in
    let output = dst // datadir_name in
    let src = output ^ ".tar.gz" in
    let*! file_exist =
      try Lwt_result.return @@ Sys.file_exists output
      with Sys_error msg -> Lwt.return @@ error src dst msg
    in
    let*! () =
      if file_exist then
        Lwt_result.return @@ Log.debug "Found file %s. No need to untar." output
      else Lwt.return @@ untar src dst
    in
    let node = Node.create output in
    let* () = Tezt_tezos.Node.upgrade_storage node in
    Lwt_result.return output
end

(** For tests focused on the semantic of replay, checking that blocks continue
    to be valid even when changing the underlying libraries and such. *)
module Semantic = struct
  let max_level_available_in_data_dir = 2_500_000

  (** [stitching_blocks] contains the block levels blocks in which stitching
      between two protocols happen. *)
  let stitching_blocks =
    [
      "2244608";
      "1916928";
      "1589247";
      "1466367";
      "1343488";
      "1212416";
      "851968";
      "655360";
      "458752";
    ]

  let replay ?network ~seed ~chunk_size ~runtime () =
    if chunk_size < 0 then invalid_arg "replay: chunk_size must be >= 0" ;
    let datadir = Long_test.test_data_path () in
    Log.debug "Using seed: %d" seed ;
    let prng = Random.State.make [|seed|] in
    let test_start = Mtime_clock.now () in
    let blocks =
      Seq.unfold
        (fun () ->
          let step_start = Mtime_clock.now () in
          if Mtime.(Span.compare (span test_start step_start) runtime) >= 0 then (
            Log.debug "Time limit reached: ending test" ;
            None)
          else
            let starts =
              Random.State.int
                prng
                (max_level_available_in_data_dir - chunk_size - 1)
            in
            let starts =
              starts + 1 (* we can't replay Genesis so the min level is 1 *)
            in
            let ends = starts + chunk_size - 1 in
            let range = string_of_int starts ^ ".." ^ string_of_int ends in
            Log.debug "Blockrange: %s" range ;
            Some ([range], ()))
        ()
    in
    let blocks () =
      Log.debug "Blockrange: stitching blocks for each protocol" ;
      Seq.Cons (stitching_blocks, blocks)
    in
    let* () = Validation.replay ?network blocks datadir in
    return ()
end

let grafana_panels =
  let step_tag = "step" in
  let simple_graph ?interval title test tags =
    Grafana.simple_graph
      ?interval
      ~title
      ~tags
      ~measurement:Benchmark.influxDB_measurement
      ~field:"duration"
      ~test
      ()
  in
  let specific_benchmark_tags =
    List.map (fun label -> (step_tag, label)) Benchmark.subparts_steps
  in
  [
    Grafana.Row "Block Validation";
    simple_graph
      "Mean validation duration per block on a chunk of consecutive blocks"
      Benchmark.chunk_title
      [];
    simple_graph
      ~interval:(Minutes 10)
      ("Mean validation duration for the block of level "
     ^ Benchmark.block_with_highest_gas)
      Benchmark.specific_title
      [];
    Grafana.graphs_per_tags
      ~title:
        ("Mean validation's subparts duration for the block of level "
       ^ Benchmark.block_with_highest_gas)
      ~measurement:Benchmark.influxDB_measurement
      ~field:"duration"
      ~test:Benchmark.subparts_title
      ~tags:specific_benchmark_tags
      ();
  ]

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/5621
   The tests [shell_validation.block.*] have disabled by adding the
   tag [ci_disabled] due to consistent failures. *)
let register ~executors () =
  let datadir = lazy (Fixture.datadir ()) in

  Long_test.register
    ~__FILE__
    ~title:Benchmark.chunk_title
    ~tags:["shell"; "validation"; "block"; "chunk"; Tag.ci_disabled]
    ~timeout:(Long_test.Minutes 20)
    ~executors
  @@ apply_or_raise datadir
  @@ Benchmark.chunk_of_consecutive_blocks_total ~size:1000 ~repeat:1 ;

  Long_test.register
    ~__FILE__
    ~title:Benchmark.specific_title
    ~tags:["shell"; "validation"; "block"; "specific"; Tag.ci_disabled]
    ~timeout:(Long_test.Minutes 20)
    ~executors
  @@ apply_or_raise datadir
  @@ Benchmark.batch_of_same_block_total
       ~size:10
       ~repeat:30
       Benchmark.block_with_highest_gas ;

  Long_test.register
    ~__FILE__
    ~title:Benchmark.subparts_title
    ~tags:["shell"; "validation"; "block"; "subpart"; Tag.ci_disabled]
    ~timeout:(Long_test.Minutes 20)
    ~executors
  @@ apply_or_raise datadir
  @@ Benchmark.batch_of_same_block_subparts
       ~size:30
       Benchmark.block_with_highest_gas

let register_semantic_regression_test ~executors () =
  (* we randomly generate a seed that the tests can use, for full portability we
     generate at most a 30-bit integer*)
  let seed =
    let seeder = Random.State.make_self_init () in
    Random.State.int seeder ((1 lsl 30) - 1)
  in

  Long_test.register
    ~__FILE__
    ~title:"shell.validation.replay"
    ~tags:["shell"; "validation"; "replay"]
    ~timeout:(Long_test.Hours 7)
    ~executors
  @@ Semantic.replay
       ~network:"mainnet"
       ~seed
       ~chunk_size:500
       ~runtime:Mtime.Span.(6 * hour)
