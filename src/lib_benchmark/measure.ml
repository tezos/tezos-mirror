(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

open Stats

type determinizer_option = Percentile of int | Mean

type options = {
  seed : int option;
  nsamples : int;
  determinizer : determinizer_option;
  bench_number : int;
  minor_heap_size : [`words of int];
  config_dir : string option;
}

type 'workload timed_workload = {workload : 'workload; qty : float}

type 'workload workload_data = 'workload timed_workload list

type 'workload measurement = {
  bench_opts : options;
  workload_data : 'workload workload_data;
  date : Unix.tm;
}

type packed_measurement =
  | Measurement : (_, 't) Benchmark.poly * 't measurement -> packed_measurement

(* We can't deserialize the bytes before knowing the benchmark, which
   contains the workload encoding. *)
type serialized_workload = {bench_name : string; measurement_bytes : Bytes.t}

type workloads_stats = {
  max_time : float;
  min_time : float;
  mean_time : float;
  variance : float;
}

(* ------------------------------------------------------------------------- *)

let determinizer_option_encoding : determinizer_option Data_encoding.t =
  let open Data_encoding in
  union
    [
      case
        ~title:"percentile"
        (Tag 0)
        Benchmark_helpers.int_encoding
        (function Percentile i -> Some i | Mean -> None)
        (fun i -> Percentile i);
      case
        ~title:"mean"
        (Tag 1)
        unit
        (function Percentile _ -> None | Mean -> Some ())
        (fun () -> Mean);
    ]

let flush_cache_encoding : [`Cache_megabytes of int | `Dont] Data_encoding.t =
  let open Data_encoding in
  union
    [
      case
        ~title:"cache_megabytes"
        (Tag 0)
        Benchmark_helpers.int_encoding
        (function `Cache_megabytes i -> Some i | `Dont -> None)
        (fun i -> `Cache_megabytes i);
      case
        ~title:"dont"
        (Tag 1)
        unit
        (function `Cache_megabytes _ -> None | `Dont -> Some ())
        (fun () -> `Dont);
    ]

let heap_size_encoding : [`words of int] Data_encoding.t =
  let open Data_encoding in
  conv
    (function `words i -> i)
    (fun i -> `words i)
    Benchmark_helpers.int_encoding

let options_encoding =
  (* : benchmark_options Data_encoding.encoding in *)
  let open Data_encoding in
  def "benchmark_options_encoding"
  @@ conv
       (fun {
              seed;
              nsamples;
              determinizer;
              bench_number;
              minor_heap_size;
              config_dir;
            } ->
         ( seed,
           nsamples,
           determinizer,
           bench_number,
           minor_heap_size,
           config_dir ))
       (fun ( seed,
              nsamples,
              determinizer,
              bench_number,
              minor_heap_size,
              config_dir ) ->
         {
           seed;
           nsamples;
           determinizer;
           bench_number;
           minor_heap_size;
           config_dir;
         })
       (tup6
          (option Benchmark_helpers.int_encoding)
          Benchmark_helpers.int_encoding
          determinizer_option_encoding
          Benchmark_helpers.int_encoding
          heap_size_encoding
          (option string))

let unix_tm_encoding : Unix.tm Data_encoding.encoding =
  let to_tuple tm =
    let open Unix in
    ( tm.tm_sec,
      tm.tm_min,
      tm.tm_hour,
      tm.tm_mday,
      tm.tm_mon,
      tm.tm_year,
      tm.tm_wday,
      tm.tm_yday,
      tm.tm_isdst )
  in
  let of_tuple
      ( tm_sec,
        tm_min,
        tm_hour,
        tm_mday,
        tm_mon,
        tm_year,
        tm_wday,
        tm_yday,
        tm_isdst ) =
    let open Unix in
    {
      tm_sec;
      tm_min;
      tm_hour;
      tm_mday;
      tm_mon;
      tm_year;
      tm_wday;
      tm_yday;
      tm_isdst;
    }
  in
  let open Data_encoding in
  def "unix_tm_encoding"
  @@ conv
       to_tuple
       of_tuple
       (tup9
          Benchmark_helpers.int_encoding
          Benchmark_helpers.int_encoding
          Benchmark_helpers.int_encoding
          Benchmark_helpers.int_encoding
          Benchmark_helpers.int_encoding
          Benchmark_helpers.int_encoding
          Benchmark_helpers.int_encoding
          Benchmark_helpers.int_encoding
          bool)

let timed_workload_encoding workload_encoding =
  let open Data_encoding in
  conv
    (fun {workload; qty} -> (workload, qty))
    (fun (workload, qty) -> {workload; qty})
    (obj2 (req "workload" workload_encoding) (req "qty" float))

let workload_data_encoding workload_encoding =
  Data_encoding.list (timed_workload_encoding workload_encoding)

let measurement_encoding workload_encoding =
  let open Data_encoding in
  def "measurement_encoding"
  @@ conv
       (fun {bench_opts; workload_data; date} ->
         (bench_opts, workload_data, date))
       (fun (bench_opts, workload_data, date) ->
         {bench_opts; workload_data; date})
       (tup3
          options_encoding
          (workload_data_encoding workload_encoding)
          unix_tm_encoding)

let serialized_workload_encoding =
  let open Data_encoding in
  def "serialized_workload"
  @@ conv
       (fun {bench_name; measurement_bytes} -> (bench_name, measurement_bytes))
       (fun (bench_name, measurement_bytes) -> {bench_name; measurement_bytes})
       (obj2 (req "bench_name" string) (req "measurement_bytes" bytes))

(* ------------------------------------------------------------------------- *)
(* Pp *)

let pp_options fmtr (options : options) =
  let open Printf in
  let seed =
    match options.seed with
    | None -> "self-init"
    | Some seed -> string_of_int seed
  in
  let nsamples = string_of_int options.nsamples in
  let determinizer =
    match options.determinizer with
    | Percentile i -> sprintf "percentile %d" i
    | Mean -> "mean"
  in
  let config_dir = Option.value options.config_dir ~default:"None" in
  let bench_number = string_of_int options.bench_number in
  let minor_heap_size = match options.minor_heap_size with `words n -> n in
  Format.fprintf
    fmtr
    "@[<v 2>{ seed=%s;@,\
     bench #=%s;@,\
     nsamples/bench=%s;@,\
     determinizer=%s;@,\
     minor_heap_size=%d words;@,\
     config directory=%s }@]"
    seed
    bench_number
    nsamples
    determinizer
    minor_heap_size
    config_dir

let pp_stats : Format.formatter -> workloads_stats -> unit =
 fun fmtr {max_time; min_time; mean_time; variance} ->
  Format.fprintf
    fmtr
    "@[{ max_time = %f ; min_time = %f ; mean_time = %f ; sigma = %f }@]"
    max_time
    min_time
    mean_time
    (sqrt variance)

(* ------------------------------------------------------------------------- *)
(* Saving/loading workload data *)

let save :
    type c t.
    filename:string ->
    options:options ->
    bench:(c, t) Benchmark.poly ->
    workload_data:t workload_data ->
    unit =
 fun ~filename ~options ~bench ~workload_data ->
  let (module Bench) = bench in
  let date = Unix.gmtime (Unix.time ()) in
  let measurement = {bench_opts = options; workload_data; date} in
  let measurement_bytes =
    match
      Data_encoding.Binary.to_bytes
        (measurement_encoding Bench.workload_encoding)
        measurement
    with
    | Error err ->
        Format.eprintf
          "Measure.save: encoding failed (%a); exiting"
          Data_encoding.Binary.pp_write_error
          err ;
        exit 1
    | Ok res -> res
  in
  let serialized_workload = {bench_name = Bench.name; measurement_bytes} in
  let str =
    match
      Data_encoding.Binary.to_string
        serialized_workload_encoding
        serialized_workload
    with
    | Error err ->
        Format.eprintf
          "Measure.save: encoding failed (%a); exiting"
          Data_encoding.Binary.pp_write_error
          err ;
        exit 1
    | Ok res -> res
  in
  let _nwritten =
    Lwt_main.run @@ Tezos_stdlib_unix.Lwt_utils_unix.create_file filename str
  in
  ()

let load : filename:string -> packed_measurement =
 fun ~filename ->
  let cant_load err =
    Format.eprintf
      "Measure.load: can't load file (%a); exiting"
      Data_encoding.Binary.pp_read_error
      err ;
    exit 1
  in
  let str =
    Lwt_main.run @@ Tezos_stdlib_unix.Lwt_utils_unix.read_file filename
  in
  Format.eprintf "Measure.load: loaded %s\n" filename ;
  match Data_encoding.Binary.of_string serialized_workload_encoding str with
  | Ok {bench_name; measurement_bytes} -> (
      match Registration.find_benchmark bench_name with
      | None ->
          Format.eprintf
            "Measure.load: workload file requires unregistered benchmark %s, \
             aborting@."
            bench_name ;
          exit 1
      | Some bench -> (
          match Benchmark.ex_unpack bench with
          | Ex ((module Bench) as bench) -> (
              match
                Data_encoding.Binary.of_bytes
                  (measurement_encoding Bench.workload_encoding)
                  measurement_bytes
              with
              | Error err -> cant_load err
              | Ok m -> Measurement (bench, m))))
  | Error err -> cant_load err

let to_csv :
    type c t.
    filename:string ->
    bench:(c, t) Benchmark.poly ->
    workload_data:t workload_data ->
    unit =
 fun ~filename ~bench ~workload_data ->
  let (module Bench) = bench in
  let lines =
    List.map
      (fun {workload; qty} -> (Bench.workload_to_vector workload, qty))
      workload_data
  in
  let domain vec =
    vec |> String.Map.to_seq |> Seq.map fst |> String.Set.of_seq
  in
  let names =
    List.fold_left
      (fun set (vec, _) -> String.Set.union (domain vec) set)
      String.Set.empty
      lines
    |> String.Set.elements
  in
  let rows =
    List.map
      (fun (vec, qty) ->
        let row =
          List.map
            (fun name -> string_of_float (Sparse_vec.String.get vec name))
            names
        in
        row @ [string_of_float qty])
      lines
  in
  let names = names @ ["timings"] in
  let csv = names :: rows in
  Csv.export ~filename csv

(* ------------------------------------------------------------------------- *)
(* Stats on execution times *)

let fmin (x : float) (y : float) = if x < y then x else y

let fmax (x : float) (y : float) = if x > y then x else y

let farray_min (arr : float array) =
  let minimum = ref max_float in
  for i = 0 to Array.length arr - 1 do
    minimum := fmin !minimum arr.(i)
  done ;
  !minimum

let farray_min_max (arr : float array) =
  let maximum = ref @@ ~-.max_float in
  let minimum = ref max_float in
  for i = 0 to Array.length arr - 1 do
    maximum := fmax !maximum arr.(i) ;
    minimum := fmin !minimum arr.(i)
  done ;
  (!minimum, !maximum)

let collect_stats : 'a workload_data -> workloads_stats =
 fun workload_data ->
  let time_dist_data =
    List.rev_map (fun {qty; _} -> qty) workload_data |> Array.of_list
  in
  let (min, max) = farray_min_max time_dist_data in
  let dist = Emp.of_raw_data time_dist_data in
  let mean = Emp.Float.empirical_mean dist in
  let var = Emp.Float.empirical_variance dist in
  {max_time = max; min_time = min; mean_time = mean; variance = var}

(* ------------------------------------------------------------------------- *)
(* Removing outliers *)

let cull_outliers :
    nsigmas:float -> 'workload workload_data -> 'workload workload_data =
 fun ~nsigmas workload_data ->
  let stats = collect_stats workload_data in
  Format.eprintf "Removing outliers.@." ;
  Format.eprintf "Stats: %a@." pp_stats stats ;
  let delta = sqrt stats.variance *. nsigmas in
  let upper_bound = stats.mean_time +. delta in
  let lower_bound = stats.mean_time -. delta in
  Format.eprintf "Validity interval: [%f, %f].@." lower_bound upper_bound ;
  let outlier_count = ref 0 in
  let valid =
    List.filter
      (fun {qty; _} ->
        let cond = lower_bound <= qty && qty <= upper_bound in
        if not cond then (
          incr outlier_count ;
          Format.eprintf "outlier detected: %f@." qty) ;
        cond)
      workload_data
  in
  let total = List.length workload_data in
  Format.eprintf
    "Removed %d outliers out of %d elements.@."
    !outlier_count
    total ;
  valid

(* ------------------------------------------------------------------------- *)
(* Benchmarking *)

module Time = struct
  external get_time_ns : unit -> (int64[@unboxed])
    = "caml_clock_gettime_byte" "caml_clock_gettime"
    [@@noalloc]

  let measure f =
    let bef = get_time_ns () in
    let _ = f () in
    let aft = get_time_ns () in
    let dt = Int64.(to_float (sub aft bef)) in
    dt
    [@@inline always]

  let measure_and_return f =
    let bef = get_time_ns () in
    let x = f () in
    let aft = get_time_ns () in
    let dt = Int64.(to_float (sub aft bef)) in
    (dt, x)
    [@@inline always]
end

let make_sampler closure _rng_state = Time.measure closure

let compute_empirical_timing_distribution :
    closure:(unit -> 'a) -> nsamples:int -> float Emp.t =
 fun ~closure ~nsamples ->
  let sampler = make_sampler closure in
  let dummy_rng_state = Random.State.make [|1; 2; 3|] in
  Emp.of_generative ~nsamples sampler dummy_rng_state

let determinizer_from_options options =
  match options.determinizer with
  | Percentile i ->
      let perc = float_of_int i *. 0.01 in
      fun dist -> Emp.quantile (module Basic_structures.Std.Float) dist perc
  | Mean -> Emp.Float.empirical_mean

let seed_init_from_options (options : options) =
  match options.seed with
  | None -> Random.State.make_self_init ()
  | Some seed -> Random.State.make [|seed|]

let gc_init_from_options (options : options) =
  match options.minor_heap_size with
  | `words words -> Gc.set {(Gc.get ()) with minor_heap_size = words}

let set_gc_increment () =
  let stats = Gc.stat () in
  let words = stats.Gc.heap_words in
  let minimal_increment = 8 * 1024 * 1024 in
  let ratio = float minimal_increment /. float words in
  if ratio < 0.15 then Gc.set {(Gc.get ()) with major_heap_increment = 15}
  else Gc.set {(Gc.get ()) with major_heap_increment = minimal_increment}

let parse_config (type c t) ((module Bench) : (c, t) Benchmark.poly)
    (options : options) =
  match options.config_dir with
  | None ->
      Format.eprintf "Using default configuration for benchmark %s@." Bench.name ;
      let json =
        Data_encoding.Json.construct Bench.config_encoding Bench.default_config
      in
      Format.eprintf "%a@." Data_encoding.Json.pp json ;
      Bench.default_config
  | Some directory ->
      let config_file = Format.asprintf "%s.json" Bench.name in
      let path = Filename.concat directory config_file in
      let json = Benchmark_helpers.load_json path in
      let config =
        try Data_encoding.Json.destruct Bench.config_encoding json
        with Data_encoding.Json.Cannot_destruct (_, _) as exn ->
          Format.eprintf
            "Json deserialization error: %a@."
            (Data_encoding.Json.print_error ?print_unknown:None)
            exn ;
          exit 1
      in
      Format.eprintf
        "Loading configuration from %s for benchmark %s@."
        path
        Bench.name ;
      Format.eprintf "%a@." Data_encoding.Json.pp json ;
      config

let perform_benchmark (type c t) (options : options)
    (bench : (c, t) Benchmark.poly) : t workload_data =
  let (module Bench) = bench in
  let config = parse_config bench options in
  let rng_state = seed_init_from_options options in
  let benchmarks =
    Bench.create_benchmarks ~rng_state ~bench_num:options.bench_number config
  in
  let determinizer = determinizer_from_options options in
  gc_init_from_options options ;
  let progress =
    Benchmark_helpers.make_progress_printer
      Format.err_formatter
      (List.length benchmarks)
      "benchmarking"
  in
  let workload_data =
    List.fold_left
      (fun workload_data benchmark_fun ->
        progress () ;
        set_gc_increment () ;
        Gc.compact () ;
        match benchmark_fun () with
        | Generator.Plain {workload; closure} ->
            let qty_dist =
              compute_empirical_timing_distribution
                ~closure
                ~nsamples:options.nsamples
            in
            let qty = determinizer qty_dist in
            {workload; qty} :: workload_data
        | Generator.With_context {workload; closure; with_context} ->
            with_context (fun context ->
                let qty_dist =
                  compute_empirical_timing_distribution
                    ~closure:(fun () -> closure context)
                    ~nsamples:options.nsamples
                in
                let qty = determinizer qty_dist in
                {workload; qty} :: workload_data)
        | Generator.With_probe {workload; probe; closure} ->
            Tezos_stdlib.Utils.do_n_times options.nsamples (fun () ->
                closure probe) ;
            let aspects = probe.Generator.aspects () in
            List.fold_left
              (fun acc aspect ->
                let results = probe.Generator.get aspect in
                let qty_dist = Emp.of_raw_data (Array.of_list results) in
                let qty = determinizer qty_dist in
                let workload = workload aspect in
                {workload; qty} :: acc)
              workload_data
              aspects)
      []
      benchmarks
  in
  Format.eprintf "@." ;
  (* newline after progress printer terminates *)
  Format.eprintf
    "stats over all benchmarks: %a@."
    pp_stats
    (collect_stats workload_data) ;
  workload_data

(* ------------------------------------------------------------------------- *)
(* Helpers for creating basic probes *)

let make_timing_probe (type t) (module O : Compare.COMPARABLE with type t = t) =
  let table = Stdlib.Hashtbl.create 41 in
  let module Set = Set.Make (O) in
  {
    Generator.apply =
      (fun aspect closure ->
        let (dt, r) = Time.measure_and_return closure in
        Stdlib.Hashtbl.add table aspect dt ;
        r);
    aspects =
      (fun () -> Stdlib.Hashtbl.to_seq_keys table |> Set.of_seq |> Set.elements);
    get = (fun aspect -> Stdlib.Hashtbl.find_all table aspect);
  }
