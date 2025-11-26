(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Test_helpers
open Floodgate_lib

type parameters = {
  mutable profiling : bool;
  mutable time_between_blocks :
    [ `Manual of float
      (** [`Manual t] the sandbox node will not produce any block by itself but
          the tezt benchmark will call [produce_block] every [t] seconds *)
    | `Auto of float
      (** [`Auto t] the sandbox node will produce a block every [t] seconds *)
    ];
  mutable iterations : int;
  mutable accounts : int option;
  mutable contracts : int option;
  mutable timeout : float;
  mutable spp : int;
  mutable width : int;
  mutable height : int;
  mutable swap_hops : int;
}

let parameters =
  (* default values *)
  {
    profiling = false;
    time_between_blocks = `Manual 0.5;
    iterations = 5;
    accounts = None;
    contracts = None;
    timeout = 2.0;
    spp = 2;
    width = 64;
    height = 48;
    swap_hops = 1;
  }

module type PARAMETERS = sig
  val profiling : bool

  val time_between_blocks : [`Manual of float | `Auto of float]

  val iterations : int

  val accounts : int option

  val contracts : int option

  val timeout : float

  val spp : int

  val width : int

  val height : int

  val swap_hops : int
end

module Parameters () : PARAMETERS = struct
  let section =
    Clap.section
      "EVM NODE BENCHMARK"
      ~description:"Parameters for running benchmarks on the EVM node"

  let profiling =
    Clap.flag
      ~section
      ~set_long:"profile"
      false
      ~description:
        "Report profiling information (needs perf on linux and xctrace on \
         macos)"

  let tbb =
    Clap.typ
      ~name:"time_between_blocks"
      ~dummy:parameters.time_between_blocks
      ~parse:(fun s ->
        match String.split_on_char ':' s with
        | ["auto"; v] -> Some (`Auto (float_of_string v))
        | ["manual"; v] -> Some (`Manual (float_of_string v))
        | _ -> None)
      ~show:(function
        | `Auto v -> "auto:" ^ string_of_float v
        | `Manual v -> "manual:" ^ string_of_float v)

  let time_between_blocks =
    Clap.default
      tbb
      ~section
      ~long:"time_between_blocks"
      ~short:'T'
      ~placeholder:"[auto|manual]:seconds"
      ~description:
        "Number of seconds between blocks. With auto:t the sequencer will \
         produce a block every t seconds, while with manual:t the sequencer \
         will not produce any block by itself but the tezt benchmark will call \
         produce_block every t seconds."
      parameters.time_between_blocks

  let iterations =
    Clap.default_int
      ~section
      ~long:"iterations"
      ~short:'I'
      ~placeholder:"nb"
      ~description:"Number of iterations for the benchmark"
      parameters.iterations

  let accounts =
    Clap.optional_int
      ~section
      ~long:"accounts"
      ~short:'A'
      ~placeholder:"nb"
      ~description:"Number of accounts that sign transactions for the benchmark"
      ()

  let contracts =
    Clap.optional_int
      ~section
      ~long:"contracts"
      ~short:'C'
      ~placeholder:"nb"
      ~description:"Number of ERC20 contracts for the benchmark"
      ()

  let timeout =
    Clap.default_float
      ~section
      ~long:"timeout"
      ~placeholder:"secs"
      ~description:
        "Number of seconds to wait for inclusion before considering operation \
         dropped"
      parameters.timeout

  let spp =
    Clap.default_int
      ~section
      ~long:"spp"
      ~placeholder:"nb"
      ~description:
        "Samples Per Pixel for SnailTracer benchmark. Higher values will use \
         more gas."
      parameters.spp

  let width =
    Clap.default_int
      ~section
      ~long:"width"
      ~placeholder:"pixels"
      ~description:
        "Width of image for SnailTracer. Higher values will use more gas."
      parameters.width

  let height =
    Clap.default_int
      ~section
      ~long:"height"
      ~placeholder:"pixels"
      ~description:
        "Height of image for SnailTracer. Higher values will use more gas."
      48

  let swap_hops =
    Clap.default_int
      ~section
      ~long:"swap-hops"
      ~placeholder:"nb"
      ~description:"Number of hops to do in swap path."
      parameters.swap_hops

  let () =
    parameters.profiling <- profiling ;
    parameters.time_between_blocks <- time_between_blocks ;
    parameters.iterations <- iterations ;
    parameters.accounts <- accounts ;
    parameters.contracts <- contracts ;
    parameters.timeout <- timeout ;
    parameters.spp <- spp ;
    parameters.width <- width ;
    parameters.height <- height ;
    parameters.swap_hops <- swap_hops
end

let parse_cli () =
  let module P = Parameters () in
  ()

let ( let+? ) x f =
  match x with
  | Error e ->
      Format.kasprintf
        failwith
        "Error: %a"
        Tezos_base.TzPervasives.pp_print_top_error_of_trace
        e
  | Ok r -> f r

let ( let*? ) x f =
  let* x in
  let+? x in
  f x

let nb_refused = ref 0

let nb_dropped = ref 0

let nb_confirmed = ref 0

let wait_for_application ?(time_between_blocks = parameters.time_between_blocks)
    ?(max_blocks =
      let (`Auto tbb | `Manual tbb) = time_between_blocks in
      max 1 (int_of_float ((parameters.timeout /. tbb) +. 1.))) sequencer f =
  let (`Auto tbb | `Manual tbb) = time_between_blocks in
  let produce_block =
    match time_between_blocks with
    | `Auto _ -> fun () -> Lwt.return_ok 0
    | `Manual _ -> fun () -> produce_block sequencer
  in
  let* res =
    wait_for_application f ~time_between_blocks:tbb ~max_blocks ~produce_block
  in
  if !nb_dropped <> 0 then
    Log.info ~color:Log.Color.FG.red "%d operations DROPPED" !nb_dropped ;
  if !nb_refused <> 0 then
    Log.info ~color:Log.Color.FG.red "%d operations REFUSED" !nb_refused ;
  if !nb_confirmed <> 0 then Log.info "%d operations confirmed" !nb_confirmed ;
  nb_dropped := 0 ;
  nb_refused := 0 ;
  nb_confirmed := 0 ;
  return res

let floodgate_account evm_node account =
  let evm_node_endpoint = Evm_node.endpoint evm_node |> Uri.of_string in
  let+? sk =
    Signer.secret_key_from_hex (`Hex account.Eth_account.private_key)
  in
  let signer = Signer.from_secret_key sk in
  let*? a = Account.from_signer ~evm_node_endpoint signer in
  return a

let floodgate_accounts evm_node accounts =
  let evm_node_endpoint = Evm_node.endpoint evm_node |> Uri.of_string in
  let* accounts =
    Lwt_list.map_p
      (fun Eth_account.{private_key; _} ->
        let+? sk = Signer.secret_key_from_hex (`Hex private_key) in
        let signer = Signer.from_secret_key sk in
        let*? a = Account.from_signer ~evm_node_endpoint signer in
        return a)
      (Array.to_list accounts)
  in
  return (Array.of_list accounts)

let send_deploy ?nonce ?gas_limit ~sender
    (scenario : [< `ERC20 | `Custom of string]) infos evm_node =
  let rpc_endpoint = Evm_node.endpoint evm_node |> Uri.of_string in
  let*? addr =
    Floodgate.deploy ?nonce ?gas_limit ~rpc_endpoint ~scenario infos sender
  in
  return addr

let deploy_contract ?gas_limit ~rpc_node infos ~sequencer sender contract =
  let deploy () = send_deploy ?gas_limit ~sender contract infos rpc_node in
  wait_for_application sequencer deploy

let deploy_contracts ~rpc_node infos ~sequencer accounts contract nb =
  let senders = Array.sub accounts 0 nb |> Array.to_list in
  let deploys () =
    Lwt_list.map_p
      (fun sender -> send_deploy ~sender contract infos rpc_node)
      senders
  in
  wait_for_application sequencer deploys

let estimate_gas infos node sender ~value ~to_ data =
  let open Evm_node_lib_dev_encoding.Ethereum_types in
  let data =
    Option.map (fun b -> Hash (Hex ((b : Efunc_core.Private.b) :> string))) data
  in
  let from = Account.address_et sender in
  Network_info.get_gas_limit
    ~rpc_endpoint:(Uri.of_string (Evm_node.endpoint node))
    ~base_fee_per_gas:infos.Network_info.base_fee_per_gas
    ~from
    ?data
    ~value
    ~to_
    ()

type gas_limit = Gas_limit of Z.t | Estimate of Evm_node.t

let rec pp_evm_value fmt (v : Efunc_core.Types.evm_value) =
  match v with
  | `string s -> Format.fprintf fmt "%S" s
  | `bool b -> Format.pp_print_bool fmt b
  | `int z -> Z.pp_print fmt z
  | `bytes b -> Format.pp_print_string fmt (b :> string)
  | `address a -> Format.fprintf fmt "0x%s" (a :> string)
  | `array l ->
      Format.fprintf
        fmt
        "@[<hov 1>[%a]@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           pp_evm_value)
        l

let call ~container infos rpc_node contract sender ?gas_limit ?nonce
    ?total_confirmed ?(value = Z.zero) ?name ?(check_success = false) abi params
    : [> `Confirmed | `Dropped | `Refused] Lwt.t =
  let open Evm_node_lib_dev_encoding.Ethereum_types in
  let open Tezos_error_monad.Error_monad in
  let pp_tx fmt () =
    Format.fprintf
      fmt
      "@[<hov 2>%s@[<hov 1>(%a)@]@ %sto@ %s@ from@ %s@]"
      (match name with None -> "Transfer" | Some n -> "Call " ^ n)
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         pp_evm_value)
      params
      (if Z.(equal value zero) then ""
       else sf "with %f XTZ " (Z.to_float value /. (10. ** 18.)))
      contract
      (Address.to_string (Account.address_et sender))
  in
  Log.debug "%a" pp_tx () ;
  let confirmed, waker = Lwt.task () in
  let data =
    match name with
    | None -> None
    | Some name -> Some (Efunc_core.Evm.encode ~name abi params)
  in
  let* gas_limit =
    match gas_limit with
    | Some g -> return g
    | None ->
        Log.debug " - Estimate gas limit" ;
        let*? g =
          estimate_gas
            infos
            rpc_node
            sender
            ~value
            ~to_:(Address.of_string contract)
            data
        in
        Log.debug " - Gas limit: %a" Z.pp_print g ;
        return g
  in

  let*? raw_tx, transaction_object =
    Craft.transfer_with_obj_exn
      ?nonce
      ~infos
      ~from:sender
      ~to_:(Efunc_core.Private.a contract)
      ~gas_limit
      ~value
      ?data
      ()
  in
  let fees = Z.(gas_limit * infos.Network_info.base_fee_per_gas) in
  let callback status =
    match status with
    | `Accepted -> unit
    | (`Refused | `Dropped | `Confirmed) as status ->
        let c =
          match status with
          | `Refused -> nb_refused
          | `Dropped -> nb_dropped
          | `Confirmed ->
              Account.debit sender Z.(value + fees) ;
              Account.increment_nonce sender ;
              Option.iter incr total_confirmed ;
              nb_confirmed
        in
        incr c ;
        Lwt.wakeup waker status ;
        if check_success then
          Lwt.async (fun () ->
              let tx_hash =
                Evm_node_lib_dev.Transaction_object.hash transaction_object
              in
              let* res =
                Floodgate.get_transaction_receipt
                  (Evm_node.endpoint rpc_node |> Uri.of_string)
                  tx_hash
              in
              match res with
              | Ok receipt ->
                  let tx_status = Qty.to_z receipt.status in
                  if Z.(equal tx_status one) then unit
                  else
                    let (Hash (Hex h)) = tx_hash in
                    Test.fail
                      "Transaction %s was included as failed:\n%a"
                      h
                      pp_tx
                      ()
              | Error trace ->
                  Test.fail "Error fetching receipt: %a" pp_print_trace trace) ;
        unit
  in
  let next_nonce = quantity_of_z sender.nonce in
  let (Evm_node_lib_dev.Services_backend_sig.Evm_tx_container
         (module Tx_container)) =
    container
  in
  let*? add_res =
    Tx_container.add ~callback ~next_nonce transaction_object ~raw_tx
  in
  let* () =
    match add_res with
    | Ok (_ : hash) -> unit
    | Error e -> Test.fail "Error adding to the tx_queue: %s" e
  in
  confirmed

type gas_info = {level : int; gas : Z.t; gas_per_sec : float}

type gasometer = {mutable datapoints : gas_info list; mutable total_gas : Z.t}

let empty_gasometer () = {datapoints = []; total_gas = Z.zero}

let capacity_mgas_sec ~gas ~time =
  let s = Ptime.Span.to_float_s time in
  if s > 0. then
    let mega_gas = Z.to_float gas /. 1_000_000. in
    mega_gas /. s
  else 0.

let pp_capacity fmt capacity = Format.fprintf fmt "%.3f MGas/s" capacity

let current_target =
  (* Use max int to always get the latest gas constants independently of what is
   set by the kernel. *)
  let c = Evm_node_lib_dev.Gas_price.gas_constants ~storage_version:max_int in
  Z.to_float c.target /. 1_000_000.

let capacity_color ~bg capacity =
  if capacity < current_target then
    if bg then Log.Color.BG.red else Log.Color.FG.red
  else if capacity < 2. *. current_target (* current capacity *) then
    if bg then Log.Color.BG.yellow else Log.Color.FG.yellow
  else if bg then Log.Color.BG.green
  else Log.Color.FG.green

let get_percentile p data =
  let sorted_data = List.sort Float.compare data in
  let data_array = Array.of_list sorted_data in
  let n = Array.length data_array in
  if n = 0 then 0.0
  else
    let index = p /. 100.0 *. float_of_int (n - 1) in
    let i = int_of_float index in
    let f = index -. float_of_int i in
    if i >= n - 1 then data_array.(n - 1)
    else (data_array.(i) *. (1. -. f)) +. (data_array.(i + 1) *. f)

(** We measure capacity percentiles as the proportion of blocks which execute
    with a speed in MGas/s above the returned value.  *)
let get_capacity_percentile p = get_percentile (100. -. p)

let blueprint_application_event = "blueprint_application.v0"

let install_gasometer evm_node =
  let gasometer = empty_gasometer () in
  let () =
    Evm_node.on_event evm_node @@ fun {name; value; _} ->
    if name = blueprint_application_event then (
      let open JSON in
      let level_str = value |-> "level" |> as_string in
      let process_time =
        value |-> "process_time" |> as_float |> Ptime.Span.of_float_s
        |> Option.get
      in
      let execution_gas =
        value |-> "execution_gas" |> as_string |> Z.of_string
      in
      let ignored = execution_gas < Z.of_int 100_000 in
      let block_capacity =
        capacity_mgas_sec ~gas:execution_gas ~time:process_time
      in
      Log.info
        "Level %s: %a gas consumed in %a: %a"
        level_str
        Z.pp_print
        execution_gas
        Ptime.Span.pp
        process_time
        (fun fmt (ignored, capacity) ->
          if ignored then Format.pp_print_string fmt "(ignored)"
          else pp_capacity fmt capacity)
        (ignored, block_capacity) ;
      if not ignored then (
        let level = int_of_string level_str in
        let info = {level; gas = execution_gas; gas_per_sec = block_capacity} in
        gasometer.datapoints <- info :: gasometer.datapoints ;
        gasometer.total_gas <- Z.add gasometer.total_gas execution_gas ;
        let capacities =
          List.map (fun {gas_per_sec; _} -> gas_per_sec) gasometer.datapoints
        in
        let median = get_capacity_percentile 50. capacities in
        let color = capacity_color ~bg:false median in
        Log.info
          ~color
          ~prefix:"Current median capacity"
          "%a"
          pp_capacity
          median))
  in
  gasometer

let log_capcity evm_node what capacity =
  let color = capacity_color ~bg:true capacity in
  Log.report
    ~color
    ~prefix:(Format.sprintf "%s capacity of %s" what (Evm_node.name evm_node))
    "%a"
    pp_capacity
    capacity

type monitor_result = {
  median : float;
  p90 : float;
  wall : float;
  gasometer : gasometer;
}

let monitor_gasometer evm_node f =
  let gasometer = install_gasometer evm_node in
  let start_time = Ptime_clock.now () in
  let* () = f () in
  let end_time = Ptime_clock.now () in
  let wall_time = Ptime.diff end_time start_time in
  let capacities =
    List.map (fun {gas_per_sec; _} -> gas_per_sec) gasometer.datapoints
  in
  let median = get_capacity_percentile 50. capacities in
  let p90 = get_capacity_percentile 90. capacities in
  let wall_clock_capacity =
    capacity_mgas_sec ~gas:gasometer.total_gas ~time:wall_time
  in
  log_capcity evm_node "Median" median ;
  log_capcity evm_node "90th percentile" p90 ;
  log_capcity evm_node "Wall clock" wall_clock_capacity ;
  return {median; p90; wall = wall_clock_capacity; gasometer}

let save_capacity ~csv_filename capacity =
  let y, m, d = Ptime.to_date @@ Ptime_clock.now () in
  let date_str = Printf.sprintf "%04d-%02d-%02d" y m d in
  let csv_line = Printf.sprintf "%s,%.2f\n" date_str capacity in
  let oc = open_out_gen [Open_append; Open_creat] 0o644 csv_filename in
  output_string oc csv_line ;
  close_out oc

let plot_capacity ~csv_filename ~network ~kernel output_filename =
  let gnuplot_script =
    Printf.sprintf
      {|
      set terminal pngcairo size 1024,768 enhanced font 'Verdana,10';
      set output '%s';
      set title 'Capacity over Time for %s with %s kernel';
      set xlabel 'Date';
      set ylabel 'Capacity (MGas/s)';
      set timefmt '%%Y-%%m-%%d';
      set xdata time;
      set xtics rotate by -45;
      set grid;
      set datafile separator ',';
      plot '%s' using 1:2 with linespoints title 'capacity';
      |}
      output_filename
      network
      kernel
      csv_filename
  in
  Process.run "gnuplot" ["-e"; gnuplot_script]

let plot_gas_and_capacity ~gasometer ~output_filename =
  let data_filename = Temp.file "gas_data.dat" in
  let oc = open_out data_filename in
  (* The datapoints are stored in reverse order of arrival. *)
  let datapoints = List.rev gasometer.datapoints in
  List.iter
    (fun {level; gas; gas_per_sec} ->
      Printf.fprintf
        oc
        "%d %.3f %.3f\n"
        level
        (Z.to_float gas /. 1_000_000.)
        gas_per_sec)
    datapoints ;
  close_out oc ;
  let gnuplot_script =
    Printf.sprintf
      {|
      set terminal pngcairo size 1024,768 enhanced font 'Verdana,10';
      set output '%s';
      set title 'Gas and Capacity per Block';
      set xlabel 'Block Number';
      set ylabel 'Capacity (MGas/s)';
      set y2label 'Gas/Block (MGas)';
      set ytics nomirror;
      set y2tics;
      set y2range [0:*];
      set grid;
      set key top left;
      plot '%s' using 1:3 with linespoints title 'Capacity' axes x1y1,
           '' using 1:2 with linespoints title 'Gas/Block' axes x1y2;
      |}
      output_filename
      data_filename
  in
  Process.run "gnuplot" ["-e"; gnuplot_script]

let get_mem_mb pid =
  Lwt_process.with_process_in ("ps", [|"ps"; "-p"; pid; "-o"; "rss="|])
  @@ fun p ->
  let* s = Lwt_io.read_line p#stdout in
  return (float_of_string s /. 1_000.)

let sample_memory evm_node =
  let pid = Evm_node.pid evm_node |> Option.get |> string_of_int in
  let current = ref 0. in
  let peak = ref 0. in
  let rec sample () =
    let* mem = get_mem_mb pid in
    current := mem ;
    peak := max !peak mem ;
    let* () = Lwt_unix.sleep 0.1 in
    sample ()
  in
  let background = sample () in
  fun () ->
    Lwt.cancel background ;
    Log.report ~color:Log.Color.bold "Peak memory: %.3fMB" !peak ;
    Log.report ~color:Log.Color.bold "Memory:      %.3fMB" !current

module MacOS = struct
  let xtrace_re = rex "<cycle-weight id=\"(\\d+)\".*>(\\d+)</cycle-weight>"

  let xtrace_ref_re = rex "<cycle-weight ref=\"(\\d+)\"/>"

  let total_cycles profile_file =
    let weights = Hashtbl.create 1111 in
    let total = ref 0. in
    let command =
      ( "xcrun",
        [|
          "xcrun";
          "xctrace";
          "export";
          "--input";
          profile_file;
          "--xpath";
          "/trace-toc/run/data/table[@schema=\"cpu-profile\"]";
        |] )
    in
    let* () =
      Lwt_process.with_process_in command @@ fun p ->
      Lwt_io.read_lines p#stdout
      |> Lwt_stream.iter @@ fun line ->
         let cycles =
           match line =~** xtrace_re with
           | Some (sid, scycles) ->
               let cycles =
                 try float_of_string scycles /. 1_000_000. with _ -> 0.
               in
               let id = int_of_string sid in
               Hashtbl.add weights id cycles ;
               Some cycles
           | None -> (
               match line =~* xtrace_ref_re with
               | Some sid ->
                   let id = int_of_string sid in
                   Hashtbl.find_opt weights id
               | None -> None)
         in
         match cycles with
         | None -> ()
         | Some cycles -> total := !total +. cycles
    in
    return !total

  let profile evm_node =
    let profile_file =
      Filename.concat (Temp.dir "traces") "evm_benchmark.trace"
    in
    let xctrace =
      Process.spawn
        "xcrun"
        [
          "xctrace";
          "record";
          "--template";
          "CPU Profiler";
          "--no-prompt";
          "--output";
          profile_file;
          "--attach";
          Evm_node.pid evm_node |> Option.get |> string_of_int;
        ]
    in
    let report_mem = sample_memory evm_node in
    fun () ->
      let* _ = Process.wait xctrace in
      let* mcycles = total_cycles profile_file in
      Log.report ~color:Log.Color.bold "%.3f MCycles" mcycles ;
      report_mem () ;
      unit
end

module Linux = struct
  let profile evm_node =
    let perf =
      Process.spawn
        "perf"
        ["stat"; "-p"; Evm_node.pid evm_node |> Option.get |> string_of_int]
    in
    let report_mem = sample_memory evm_node in
    fun () ->
      Process.terminate perf ;
      let* stat = Lwt_io.read (Process.stdout perf) in
      Log.report ~prefix:"perf" "%s" stat ;
      report_mem () ;
      unit
end

let profile evm_node =
  let* os = Process.run_and_read_stdout "uname" [] in
  match String.trim os with
  | "Darwin" -> return (MacOS.profile evm_node)
  | _ -> return (Linux.profile evm_node)
