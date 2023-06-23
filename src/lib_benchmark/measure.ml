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

type options = {
  seed : int option;
  nsamples : int;
  bench_number : int;
  minor_heap_size : [`words of int];
  config_file : string option;
}

type 'workload measured_workload = {
  workload : 'workload;
  measures : Maths.vector;
}

type 'workload workload_data = 'workload measured_workload list

type 'workload measurement = {
  bench_opts : options;
  workload_data : 'workload workload_data;
  date : Unix.tm;
}

type packed_measurement =
  | Measurement : (_, 't) Benchmark.poly * 't measurement -> packed_measurement

(* We can't deserialize the bytes before knowing the benchmark, which
   contains the workload encoding. *)
type serialized_workload = {
  bench_name : Namespace.t;
  measurement_bytes : Bytes.t;
}

type workloads_stats = {
  max : float;
  min : float;
  mean : float;
  variance : float;
}

(* ------------------------------------------------------------------------- *)

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
       (fun {seed; nsamples; bench_number; minor_heap_size; config_file} ->
         (seed, nsamples, bench_number, minor_heap_size, config_file))
       (fun (seed, nsamples, bench_number, minor_heap_size, config_file) ->
         {seed; nsamples; bench_number; minor_heap_size; config_file})
       (obj5
          (req "seed" (option Benchmark_helpers.int_encoding))
          (req "samples_per_bench" Benchmark_helpers.int_encoding)
          (req "bench_number" Benchmark_helpers.int_encoding)
          (req "minor_heap_size" heap_size_encoding)
          (req "config_file" (option string)))

let rfc3339_encoding =
  let of_tm tm =
    let time_s, _ = Unix.mktime tm in
    let ptime =
      WithExceptions.Option.get ~loc:__LOC__ @@ Ptime.of_float_s @@ time_s
    in
    Ptime.to_rfc3339 ~tz_offset_s:0 ptime
  in
  let to_tm rfc3339_string =
    Ptime.of_rfc3339 ~strict:true rfc3339_string
    |> Ptime.rfc3339_error_to_msg
    |> Result.map_error (function `Msg e -> e)
    |> Result.map (fun (utc, _, _) ->
           let seconds = Ptime.to_float_s utc in
           Unix.gmtime seconds)
  in
  (* let strip_msg =  Result.map_error (fun `Msg s -> s) in *)
  Data_encoding.conv_with_guard
    (fun tm -> of_tm tm)
    (fun str -> to_tm str)
    Data_encoding.string

let vec_encoding : Maths.vector Data_encoding.t =
  Data_encoding.(conv Maths.vector_to_array Maths.vector_of_array (array float))

let measured_workload_encoding workload_encoding =
  let open Data_encoding in
  conv
    (fun {workload; measures} -> (workload, measures))
    (fun (workload, measures) -> {workload; measures})
    (obj2 (req "workload" workload_encoding) (req "measures" vec_encoding))

let workload_data_encoding workload_encoding =
  Data_encoding.list (measured_workload_encoding workload_encoding)

let measurement_encoding workload_encoding =
  let open Data_encoding in
  def "measurement_encoding"
  @@ conv
       (fun {bench_opts; workload_data; date} ->
         (bench_opts, workload_data, date))
       (fun (bench_opts, workload_data, date) ->
         {bench_opts; workload_data; date})
       (obj3
          (req "benchmark_options" options_encoding)
          (req "workload_data" (workload_data_encoding workload_encoding))
          (req "date" rfc3339_encoding))

let serialized_workload_encoding =
  let open Data_encoding in
  def "serialized_workload"
  @@ conv
       (fun {bench_name; measurement_bytes} -> (bench_name, measurement_bytes))
       (fun (bench_name, measurement_bytes) -> {bench_name; measurement_bytes})
       (obj2
          (req "bench_name" Namespace.encoding)
          (req "measurement_bytes" bytes))

(* ------------------------------------------------------------------------- *)
(* Pp *)

let pp_options fmtr (options : options) =
  let seed =
    match options.seed with
    | None -> "self-init"
    | Some seed -> string_of_int seed
  in
  let nsamples = string_of_int options.nsamples in
  let config_file = Option.value options.config_file ~default:"None" in
  let bench_number = string_of_int options.bench_number in
  let minor_heap_size = match options.minor_heap_size with `words n -> n in
  Format.fprintf
    fmtr
    "@[<v 2>{ seed=%s;@,\
     bench #=%s;@,\
     nsamples/bench=%s;@,\
     minor_heap_size=%d words;@,\
     config directory=%s }@]"
    seed
    bench_number
    nsamples
    minor_heap_size
    config_file

let pp_stats : Format.formatter -> workloads_stats -> unit =
 fun fmtr {max; min; mean; variance} ->
  Format.fprintf
    fmtr
    "@[{ max = %f ; min = %f ; mean = %f ; sigma = %f }@]"
    max
    min
    mean
    (sqrt variance)

(* ------------------------------------------------------------------------- *)
(* Saving/loading workload data *)

let save :
    type c t.
    filename:string ->
    options:options ->
    bench:(c, t) Benchmark.poly ->
    workload_data:t workload_data ->
    packed_measurement =
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
          "Measure.save: encoding failed (%a); exiting@."
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
          "Measure.save: encoding failed (%a); exiting@."
          Data_encoding.Binary.pp_write_error
          err ;
        exit 1
    | Ok res -> res
  in
  let _nwritten =
    Lwt_main.run @@ Tezos_stdlib_unix.Lwt_utils_unix.create_file filename str
  in
  Measurement (bench, measurement)

let packed_measurement_save_json measurement_packed output_path =
  let (Measurement (bench, measurement)) = measurement_packed in
  let module Bench = (val bench) in
  let encoding_measurement = measurement_encoding Bench.workload_encoding in
  let encoding =
    Data_encoding.(
      obj2
        (req "benchmark_namespace" Namespace.encoding)
        (req "measurement_data" encoding_measurement))
  in
  let data = Data_encoding.Json.construct encoding (Bench.name, measurement) in
  let json = Data_encoding.Json.to_string data in
  match output_path with
  | Some output_path ->
      Out_channel.with_open_text output_path (fun oc ->
          Printf.fprintf oc "%s\n" json) ;
      Format.eprintf
        "Measure.packed_measurement_save_json: saved to %s@."
        output_path
  | None -> Printf.printf "%s\n" json

let load : filename:string -> packed_measurement =
 fun ~filename ->
  let cant_load err =
    Format.eprintf
      "Measure.load: can't load file (%a); exiting@."
      Data_encoding.Binary.pp_read_error
      err ;
    exit 1
  in
  let str =
    Lwt_main.run @@ Tezos_stdlib_unix.Lwt_utils_unix.read_file filename
  in
  Format.eprintf "Measure.load: loaded %s@." filename ;
  match Data_encoding.Binary.of_string serialized_workload_encoding str with
  | Ok {bench_name; measurement_bytes} -> (
      let bench = Registration.find_benchmark_exn bench_name in
      match Benchmark.ex_unpack bench with
      | Ex ((module Bench) as bench) -> (
          match
            Data_encoding.Binary.of_bytes
              (measurement_encoding Bench.workload_encoding)
              measurement_bytes
          with
          | Error err -> cant_load err
          | Ok m -> Measurement (bench, m)))
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
      (fun {workload; measures; _} ->
        (Bench.workload_to_vector workload, measures))
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
      (fun (vec, measures) ->
        let row =
          List.map
            (fun name -> string_of_float (Sparse_vec.String.get vec name))
            names
        in
        let measures =
          measures |> Maths.vector_to_seq |> Seq.map string_of_float
          |> List.of_seq
        in
        row @ measures)
      lines
  in
  let names = names @ ["timings"] in
  let csv = names :: rows in
  Csv.export ~filename csv

(* ------------------------------------------------------------------------- *)
(* Stats on measures *)

let fmin (x : float) (y : float) = if x < y then x else y

let fmax (x : float) (y : float) = if x > y then x else y

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
  let dist_data =
    List.rev_map
      (fun {measures; _} -> Array.of_seq (Maths.vector_to_seq measures))
      workload_data
    |> Array.concat
  in
  let min, max = farray_min_max dist_data in
  let dist = Emp.of_raw_data dist_data in
  let mean = Emp.Float.empirical_mean dist in
  let var = Emp.Float.empirical_variance dist in
  {max; min; mean; variance = var}

(* ------------------------------------------------------------------------- *)
(* Benchmarking *)

module Time = struct
  external get_time_ns : unit -> (int64[@unboxed])
    = "caml_clock_gettime_byte" "caml_clock_gettime"
    [@@noalloc]

  external clock_getres : unit -> (int64[@unboxed])
    = "caml_clock_getres_byte" "caml_clock_getres"
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

  let check_timer_resolution () =
    let ns = clock_getres () in
    if ns = 1L then ()
    else if ns = 0L then
      Stdlib.failwith "Snoop: cannot work without a proper clock"
    else
      Format.eprintf
        "WARNING: This machine's clock reslution is %Ldns, which is too large \
         for Snoop benchmarks!@."
        ns
end

let compute_empirical_timing_distribution :
    closure:(unit -> 'a) ->
    nsamples:int ->
    buffer:(float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t ->
    index:int ref ->
    int Linalg.Vec.Float.t =
 fun ~closure ~nsamples ~buffer ~index ->
  let start = !index in
  let stop = !index + nsamples - 1 in
  index := stop + 1 ;
  for i = start to stop do
    let dt = Time.measure closure in
    buffer.{i} <- dt
  done ;
  let shape = Linalg.Tensor.Int.rank_one nsamples in
  Linalg.Vec.Float.make shape (fun i -> buffer.{i + start})
 [@@ocaml.inline]

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

let perform_benchmark (type c t) (options : options)
    (bench : (c, t) Benchmark.poly) : t workload_data =
  Time.check_timer_resolution () ;
  let (module Bench) = bench in
  let config =
    Config.parse_config ~print:Stdlib.stderr bench options.config_file
  in
  let rng_state = seed_init_from_options options in
  let buffer =
    (* holds all samples; avoids allocating an array at each bench *)
    Bigarray.Array1.create
      Bigarray.float64
      Bigarray.c_layout
      (options.bench_number * options.nsamples)
  in
  let index = ref 0 in
  let benchmarks =
    Bench.create_benchmarks ~rng_state ~bench_num:options.bench_number config
  in
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
        let measure_plain_benchmark workload closure =
          let measures =
            compute_empirical_timing_distribution
              ~closure
              ~nsamples:options.nsamples
              ~buffer
              ~index
          in
          {workload; measures} :: workload_data
        in
        match benchmark_fun () with
        | Generator.Calculated {workload; measure} ->
            let measures = Array.init options.nsamples (fun _ -> measure ()) in
            let measures = Maths.vector_of_array measures in
            {workload; measures} :: workload_data
        | Generator.Plain {workload; closure} ->
            measure_plain_benchmark workload closure
        | Generator.With_context {workload; closure; with_context} ->
            with_context (fun context ->
                let measures =
                  compute_empirical_timing_distribution
                    ~closure:(fun () -> closure context)
                    ~nsamples:options.nsamples
                    ~buffer
                    ~index
                in
                {workload; measures} :: workload_data)
        | Generator.With_probe {workload; probe; closure} ->
            Tezos_stdlib.Utils.do_n_times options.nsamples (fun () ->
                closure probe) ;
            let aspects = probe.Generator.aspects () in
            List.fold_left
              (fun acc aspect ->
                let results = probe.Generator.get aspect in
                let measures = Maths.vector_of_array (Array.of_list results) in
                let workload = workload aspect in
                {workload; measures} :: acc)
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
        let dt, r = Time.measure_and_return closure in
        Stdlib.Hashtbl.add table aspect dt ;
        r);
    aspects =
      (fun () -> Stdlib.Hashtbl.to_seq_keys table |> Set.of_seq |> Set.elements);
    get = (fun aspect -> Stdlib.Hashtbl.find_all table aspect);
  }

let get_free_variable_set measurement =
  let (Measurement ((module Bench), m)) = measurement in
  let open Free_variable.Set in
  List.fold_left
    (fun acc (_local_model_name, model) ->
      let fvs =
        List.fold_left
          (fun acc {workload; _} ->
            let fvs = Model.get_free_variable_set_applied model workload in
            union fvs acc)
          empty
          m.workload_data
      in
      union acc fvs)
    empty
    Bench.models
