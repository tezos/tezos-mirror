(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol

let ns = Namespace.make Registration_helpers.ns "skip_list"

let fv s = Free_variable.of_namespace (ns s)

module Next : Benchmark.S = struct
  include Skip_list_repr.Make (struct
    let basis = 2
  end)

  let name = ns "next"

  let info = "Benchmark for Skip_list_repr.next"

  let tags = ["skip_list"]

  type config = {max_items : int}

  let default_config = {max_items = 10000}

  let config_encoding =
    let open Data_encoding in
    conv (fun {max_items} -> max_items) (fun max_items -> {max_items}) int31

  type workload = int

  let workload_encoding = Data_encoding.int31

  let workload_to_vector len =
    Sparse_vec.String.of_list [("len", float_of_int @@ len)]

  let next_model =
    let conv x = (x, ()) in
    Model.make ~conv ~model:(Model.logn ~coeff:(fv "len_coeff"))

  let models = [("skip_list_next", next_model)]

  let create_skip_list_of_len len =
    let rec go n cell =
      if n = 0 then cell
      else go (pred n) @@ next ~prev_cell:cell ~prev_cell_ptr:() ()
    in
    go len (genesis ())

  let create_benchmarks ~rng_state ~bench_num ({max_items} : config) =
    List.repeat bench_num @@ fun () ->
    let workload =
      Base_samplers.sample_in_interval
        rng_state
        ~range:{min = 0; max = max_items}
    in
    let prev_cell = create_skip_list_of_len workload in
    let prev_cell_ptr = () in
    let closure () = ignore (next ~prev_cell ~prev_cell_ptr ()) in
    Generator.Plain {workload; closure}
  let () =
    Registration.register_for_codegen
      (Namespace.basename name)
      (Model.For_codegen next_model)
end

let () = Registration_helpers.register (module Next)
