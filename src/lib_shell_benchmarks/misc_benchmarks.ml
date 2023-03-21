(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

let ns = Namespace.make Shell_namespace.ns "misc"

let fv s = Free_variable.of_namespace (ns s)

let lwt_variable = fv "lwt_main_run"

let lwt_model =
  Model.make
    ~conv:(fun () -> ())
    ~model:(Model.unknown_const1 ~name:(ns "lwt_model") ~const:lwt_variable)

module Lwt_main_run_bench : Benchmark.S = struct
  type config = unit

  let default_config = ()

  let config_encoding = Data_encoding.unit

  let name = ns "LWT_MAIN.RUN"

  let info = "Benchmark of Lwt_main.run"

  let module_filename = __FILE__

  let generated_code_destination = None

  let () = ignore generated_code_destination

  let tags = ["misc"]

  let models = [("*", lwt_model)]

  let workload_to_vector () = Sparse_vec.String.of_list [("lwt_main", 1.)]

  type workload = unit

  let workload_encoding = Data_encoding.unit

  let bench () =
    let closure () = Lwt_main.run Lwt.return_unit in
    let workload = () in
    Generator.Plain {workload; closure}

  let create_benchmarks ~rng_state ~bench_num () =
    ignore rng_state ;
    List.repeat bench_num bench
end

let () = Registration.register (module Lwt_main_run_bench)
