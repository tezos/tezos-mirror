(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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
open Benchmarks_shell
open Tezos_micheline

let ns = Namespace.make Shell_namespace.ns "micheline"

let fv s = Free_variable.of_namespace (ns s)

(* An ad-hoc sampler for Micheline values. Boltzmann sampling would do well
   here. *)

type width_function = depth:int -> int Base_samplers.sampler

type node = (int, unit) Micheline.node

type node_kind = Int_node | String_node | Bytes_node | Seq_node | Prim_node

(* We skew the distribution towards non-leaf nodes by repeating the
   relevant kinds ;) *)
let all_kinds = [|Int_node; String_node; Bytes_node; Seq_node; Prim_node|]

let sample_kind : node_kind Base_samplers.sampler =
 fun rng_state ->
  let i = Random.State.int rng_state (Array.length all_kinds) in
  all_kinds.(i)

let sample_string _ = ""

let sample_bytes _ = Bytes.empty

let sample_z _ = Z.zero

let sample (w : width_function) rng_state =
  let rec sample depth rng_state k =
    match sample_kind rng_state with
    | Int_node -> k (Micheline.Int (0, sample_z rng_state))
    | String_node -> k (Micheline.String (0, sample_string rng_state))
    | Bytes_node -> k (Micheline.Bytes (0, sample_bytes rng_state))
    | Seq_node ->
        let width = w ~depth rng_state in
        sample_list
          depth
          width
          []
          (fun terms -> k (Micheline.Seq (0, terms)))
          rng_state
    | Prim_node ->
        let width = w ~depth rng_state in
        sample_list
          depth
          width
          []
          (fun terms -> k (Micheline.Prim (0, (), terms, [])))
          rng_state
  and sample_list depth width acc k rng_state =
    if width < 0 then invalid_arg "sample_list: negative width"
    else if width = 0 then k (List.rev acc)
    else
      sample (depth + 1) rng_state (fun x ->
          sample_list depth (width - 1) (x :: acc) k rng_state)
  in
  sample 0 rng_state (fun x -> x)

let reasonable_width_function ~depth rng_state =
  (* Entirely ad-hoc *)
  Base_samplers.(
    sample_in_interval
      ~range:{min = 0; max = 20 / (Bits.numbits depth + 1)}
      rng_state)

let sample = sample reasonable_width_function

(* Computing the size of a micheline term *)

type size = {nodes : int; bytes : int}

let int z = {nodes = 1; bytes = (Z.numbits z + 7) / 8}

let string s = {nodes = 1; bytes = String.length s}

let bytes b = {nodes = 1; bytes = Bytes.length b}

let node = {nodes = 1; bytes = 0}

let ( @+ ) x y = {nodes = x.nodes + y.nodes; bytes = x.bytes + y.bytes}

let micheline_size (n : node) =
  let rec micheline_size n acc =
    let open Micheline in
    match n with
    | Int (_, i) -> acc @+ int i
    | String (_, s) -> acc @+ string s
    | Bytes (_, b) -> acc @+ bytes b
    | Seq (_, terms) ->
        List.fold_left
          (fun acc term -> micheline_size term acc)
          (acc @+ node)
          terms
    | Prim (_, _, terms, _) ->
        List.fold_left
          (fun acc term -> micheline_size term acc)
          (acc @+ node)
          terms
  in
  micheline_size n {nodes = 0; bytes = 0}

module Micheline_strip_locations : Benchmark.S = struct
  let name = ns "strip_locations_micheline"

  let info = "Benchmarking Micheline.strip_locations"

  let module_filename = __FILE__

  let purpose =
    Benchmark.Generate_code
      "src/proto_alpha/lib_protocol/script_repr_costs_generated.ml"

  let tags = ["micheline"]

  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  type workload = size

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun {nodes; bytes} -> (nodes, bytes))
      (fun (nodes, bytes) -> {nodes; bytes})
      (obj2 (req "nodes" int31) (req "bytes" int31))

  let workload_to_vector {nodes; bytes} =
    Sparse_vec.String.of_list
      [("nodes", float_of_int nodes); ("bytes", float_of_int bytes)]

  let model ~name =
    Model.(
      make
        ~conv:(fun {nodes; bytes = _} -> (nodes, ()))
        ~model:(linear ~name ~coeff:(fv "nodes")))

  let group = Benchmark.Standalone

  let create_benchmark ~rng_state () =
    let term = sample rng_state in
    let size = micheline_size term in
    let closure () = ignore (Micheline.strip_locations term) in
    Generator.Plain {workload = size; closure}
end

let () = Registration.register (module Micheline_strip_locations)
