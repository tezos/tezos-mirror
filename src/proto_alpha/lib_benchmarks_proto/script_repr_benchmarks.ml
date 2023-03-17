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

open Protocol

let ns = Namespace.make Registration_helpers.ns "script_repr"

let fv s = Free_variable.of_namespace (ns s)

(** {2 [Script_repr] benchmarks} *)

module Script_repr_shared_config = struct
  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  type workload = {micheline_nodes : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun {micheline_nodes} -> micheline_nodes)
      (fun micheline_nodes -> {micheline_nodes})
      (obj1 (req "micheline_nodes" int31))

  let tags = [Tags.translator]

  let workload_to_vector {micheline_nodes} =
    Sparse_vec.String.of_list [("nodes", float_of_int micheline_nodes)]
end

module Sampler = Micheline_sampler.Make (struct
  type prim = Michelson_v1_primitives.prim

  (* The runtime of the functions in [Script_repr] do not depend on the primitives. *)
  let sample_prim : Michelson_v1_primitives.prim Base_samplers.sampler =
   fun _rng_state -> I_ADD

  let sample_annots : string list Base_samplers.sampler = fun _rng_state -> []

  let sample_string = Base_samplers.uniform_string ~nbytes:4

  let sample_bytes = Base_samplers.uniform_bytes ~nbytes:4

  let sample_z = Base_samplers.int ~size:{min = 1; max = 8}

  let width_function = Micheline_sampler.reasonable_width_function
end)

module Micheline_nodes_benchmark : Benchmark.S = struct
  include Script_repr_shared_config

  let name = ns "MICHELINE_NODES"

  let info =
    "Benchmarking the time it takes to compute the number of nodes of a \
     Micheline term"

  let module_filename = __FILE__

  let () = ignore module_filename

  let size_based_model =
    Model.make
      ~conv:(function {micheline_nodes} -> (micheline_nodes, ()))
      ~model:
        (Model.affine
           ~name
           ~intercept:
             (fv (Format.asprintf "%s_const" (Namespace.basename name)))
           ~coeff:
             (fv
                (Format.asprintf
                   "%s_ns_per_node_coeff"
                   (Namespace.basename name))))

  let models = [("size_translator_model", size_based_model)]

  let micheline_nodes_benchmark node =
    let nodes = Script_repr.micheline_nodes node in
    let workload = {micheline_nodes = nodes} in
    let closure () = ignore (Script_repr.micheline_nodes node) in
    Generator.Plain {workload; closure}

  let make_bench rng_state _cfg () =
    let term = Sampler.sample rng_state in
    micheline_nodes_benchmark term

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Micheline_nodes_benchmark)

module Script_repr_strip_annotations : Benchmark.S = struct
  include Script_repr_shared_config

  let name = ns "strip_annotations"

  let info = "Benchmarking Script_repr.strip_annotations"

  let module_filename = __FILE__

  let () = ignore module_filename

  let strip_annotations_model =
    Model.(
      make
        ~conv:(fun {micheline_nodes} -> (micheline_nodes, ()))
        ~model:(linear ~name ~coeff:(fv "nodes")))

  let models = [("strip_annotations_model", strip_annotations_model)]

  let create_benchmark rng_state () =
    let node = Sampler.sample rng_state in
    let closure () = ignore @@ Script_repr.strip_annotations node in
    let micheline_nodes = Script_repr.micheline_nodes node in
    Generator.Plain {workload = {micheline_nodes}; closure}

  let create_benchmarks ~rng_state ~bench_num _cfg =
    List.repeat bench_num (create_benchmark rng_state)
end

let () = Registration_helpers.register (module Script_repr_strip_annotations)
