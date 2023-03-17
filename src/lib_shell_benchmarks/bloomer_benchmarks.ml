(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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
open Benchmarks_shell

let ns = Namespace.make Shell_namespace.ns "bloomer"

let fv s = Free_variable.of_namespace (ns s)

(* We use the same Bloom filter configuration as used in P2p_acl *)

let const_time_model ~const_name ~name =
  Model.make
    ~conv:(fun () -> ())
    ~model:(Model.unknown_const1 ~name ~const:(fv const_name))

let make_bench ~name ~info ~model ~generator ~make_bench :
    Tezos_benchmark.Benchmark.t =
  let module Bench : Benchmark.S = struct
    type config = unit

    let default_config = ()

    let module_filename = __FILE__

    let config_encoding = Data_encoding.unit

    type workload = unit

    let workload_encoding = Data_encoding.unit

    let workload_to_vector () = Sparse_vec.String.of_list [("encoding", 1.)]

    let name = ns name

    let info = info

    let tags = ["misc"]

    let create_benchmarks ~rng_state ~bench_num () =
      let generator () = generator rng_state in
      List.repeat bench_num (make_bench generator)

    let models = [("bloomer", model ~name)]
  end in
  (module Bench)

let make_bloomer () =
  Bloomer.create
    ~hash:(fun x -> Tezos_crypto.Blake2B.(to_bytes (hash_string [x])))
    ~hashes:5
    ~countdown_bits:4
    ~index_bits:(Bits.numbits (2 * 1024 * 8 * 1024 / 4))

let () =
  Registration.register
  @@ make_bench
       ~name:"bloomer_mem"
       ~info:"Benchmarking Bloomer.mem"
       ~model:(const_time_model ~const_name:"bloomer_mem_const")
       ~generator:(fun _rng_state ->
         let bloomer = make_bloomer () in
         let string = "test" in
         Bloomer.add bloomer string ;
         (bloomer, string))
       ~make_bench:(fun generator () ->
         let bloomer, string = generator () in
         let closure () = ignore (Bloomer.mem bloomer string) in
         Generator.Plain {workload = (); closure})

let () =
  Registration.register
  @@ make_bench
       ~name:"bloomer_add"
       ~info:"Benchmarking Bloomer.add"
       ~model:(const_time_model ~const_name:"bloomer_add_const")
       ~generator:(fun _rng_state -> make_bloomer ())
       ~make_bench:(fun generator () ->
         let bloomer = generator () in
         let closure () = ignore (Bloomer.add bloomer "test") in
         Generator.Plain {workload = (); closure})
