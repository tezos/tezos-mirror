(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

open Tezos_benchmark

let ns = Namespace.make Namespace.root "test_probe"

(* A silly scenario to illustrate With_probe benchmarks *)

module Aspect = struct
  type t = Hashing_Blake2b | Hashing_Sha256

  let compare (x : t) (y : t) =
    match (x, y) with
    | Hashing_Sha256, Hashing_Sha256 -> 0
    | Hashing_Blake2b, Hashing_Blake2b -> 0
    | Hashing_Blake2b, Hashing_Sha256 -> -1
    | Hashing_Sha256, Hashing_Blake2b -> 1
end

type workload = Blake2b of {nbytes : int} | Sha256 of {nbytes : int}

type config = {max_bytes : int}

module Probing_bench = struct
  let name = ns "Probing_test"

  let info = "Testing probing benchmarks"

  let tags = ["example"]

  (* We will measure hashing time on random bytes with length
     sampled uniformly between 1 and [max_bytes] length. *)
  type nonrec config = config

  let default_config = {max_bytes = 1 lsl 16}

  let module_filename = __FILE__

  let generated_code_destination = None

  (* The encoding is used by `tezos-snoop` to load the config from json
     files. *)
  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_bytes} -> max_bytes)
      (fun max_bytes -> {max_bytes})
      (obj1 (req "max_bytes" int31))

  (* The only relevant piece of information is the nbytes of the bytes being
     benchmarked. *)
  type nonrec workload = workload

  (* The workload encoding is used to save and load workloads in binary format. *)
  let workload_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"Blake2b"
          (Tag 0)
          int31
          (function Blake2b {nbytes} -> Some nbytes | _ -> None)
          (fun nbytes -> Blake2b {nbytes});
        case
          ~title:"Sha256"
          (Tag 1)
          int31
          (function Sha256 {nbytes} -> Some nbytes | _ -> None)
          (fun nbytes -> Sha256 {nbytes});
      ]

  (* How to interpret a workload as a vector; for automatic scatter-plotting. *)
  let workload_to_vector = function
    | Blake2b {nbytes} ->
        Sparse_vec.String.of_list [("blake2b_nbytes", float_of_int nbytes)]
    | Sha256 {nbytes} ->
        Sparse_vec.String.of_list [("sha256_nbytes", float_of_int nbytes)]

  (* Don't care about the models here. *)
  let models = []

  let probing_benchmark rng_state config () =
    let nbytes =
      Base_samplers.sample_in_interval
        rng_state
        ~range:{min = 1; max = config.max_bytes}
    in
    let arbitrary_data = Base_samplers.uniform_bytes rng_state ~nbytes in
    let closure (probe : 'a Generator.probe) =
      let _res_blake2b =
        probe.apply Aspect.Hashing_Blake2b (fun () ->
            Tezos_crypto.Blake2B.hash_bytes [arbitrary_data])
      in
      let _res_sha256 =
        probe.apply Hashing_Sha256 (fun () ->
            Tezos_crypto.Hacl.Hash.SHA256.digest arbitrary_data)
      in
      ()
    in
    let probe = Measure.make_timing_probe (module Aspect) in
    Generator.With_probe
      {
        workload =
          (function
          | Aspect.Hashing_Blake2b -> Blake2b {nbytes}
          | Hashing_Sha256 -> Sha256 {nbytes});
        probe;
        closure;
      }

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (probing_benchmark rng_state config)
end

let bench_opts =
  let open Measure in
  {
    seed = Some 1337;
    nsamples = 30;
    bench_number = 10;
    minor_heap_size = `words (256 * 1024);
    config_file = None;
  }

(* Perform timing measurements *)
let do_bench () =
  let bench : (config, workload) Benchmark.poly = (module Probing_bench) in
  let _workload_data = Measure.perform_benchmark bench_opts bench in
  true

let tests = [Test.tztest_assert "probing bench" `Quick do_bench]

let () = Alcotest_lwt.run "tezos-benchmark" [("probe", tests)] |> Lwt_main.run
