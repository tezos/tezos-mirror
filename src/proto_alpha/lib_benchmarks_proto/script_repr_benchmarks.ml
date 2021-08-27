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

(* Eventually this should be merged in the protocol (and then lib-micheline) *)
let rec micheline_nodes node acc k =
  match node with
  | Micheline.Int (_, _) -> k (acc + 1)
  | Micheline.String (_, _) -> k (acc + 1)
  | Micheline.Bytes (_, _) -> k (acc + 1)
  | Micheline.Prim (_, _, subterms, _) ->
      micheline_nodes_list subterms (acc + 1) k
  | Micheline.Seq (_, subterms) -> micheline_nodes_list subterms (acc + 1) k

and micheline_nodes_list subterms acc k =
  match subterms with
  | [] -> k acc
  | n :: nodes ->
      micheline_nodes_list nodes acc (fun acc -> micheline_nodes n acc k)

let micheline_nodes node = micheline_nodes node 0 (fun x -> x)

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

  let name = "MICHELINE_NODES"

  let info =
    "Benchmarking the time it takes to compute the number of nodes of a \
     Micheline term"

  let size_based_model =
    Model.make
      ~conv:(function {micheline_nodes} -> (micheline_nodes, ()))
      ~model:
        (Model.affine
           ~intercept:
             (Free_variable.of_string (Format.asprintf "%s_const" name))
           ~coeff:
             (Free_variable.of_string
                (Format.asprintf "%s_ns_per_node_coeff" name)))

  let () =
    Registration_helpers.register_for_codegen
      name
      (Model.For_codegen size_based_model)

  let models = [("size_translator_model", size_based_model)]

  let micheline_nodes_benchmark node =
    let node = Micheline.root node in
    let nodes = micheline_nodes node in
    let workload = {micheline_nodes = nodes} in
    let closure () = ignore (Script_repr.micheline_nodes node) in
    Generator.Plain {workload; closure}

  let make_bench rng_state _cfg () =
    let term = Sampler.sample rng_state in
    micheline_nodes_benchmark (Micheline.strip_locations term)

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Micheline_nodes_benchmark)

module Node_size_benchmark : Benchmark.S = struct
  include Script_repr_shared_config

  let name = "NODE_SIZE"

  let info =
    "Benchmarking the time it takes to compute Script_typed_ir_size.node_size"

  let size_based_model =
    Model.make
      ~conv:(function {micheline_nodes} -> (micheline_nodes, ()))
      ~model:
        (Model.affine
           ~intercept:
             (Free_variable.of_string (Format.asprintf "%s_const" name))
           ~coeff:
             (Free_variable.of_string
                (Format.asprintf "%s_ns_per_node_coeff" name)))

  let () =
    Registration_helpers.register_for_codegen
      name
      (Model.For_codegen size_based_model)

  let models = [("size_translator_model", size_based_model)]

  let micheline_nodes_benchmark node =
    let open Cache_memory_helpers in
    let nodes = Nodes.to_int @@ fst @@ node_size node in
    let workload = {micheline_nodes = nodes} in
    let closure () = ignore (Script_typed_ir_size.node_size node) in
    Generator.Plain {workload; closure}

  let make_bench rng_state _cfg () =
    let term = Sampler.sample rng_state in
    micheline_nodes_benchmark term

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Node_size_benchmark)
