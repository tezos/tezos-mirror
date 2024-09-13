(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023  Marigold <contact@marigold.dev>                       *)
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

(** This module provides benchmarks for skip list operations for basis = 4. *)

open Protocol
open Benchmarks_proto

module Skip_list = Skip_list.Make (struct
  (** The benchmarks must be run again if [basis] is changed. *)
  let basis = 4
end)

let ns = Namespace.make Registration.ns "skip_list"

let fv s = Free_variable.of_namespace (ns s)

(** Benchmark for the [Skip_list_repr.next] function. It is used for estimating
    the parameters for [Skip_list_cost_model.model_next]. *)
module Next : Benchmark.S = struct
  let purpose = Benchmark.Generate_code "skip_list"

  include Skip_list

  let name = ns "next"

  let info = "Benchmark for Skip_list_repr.next"

  let tags = ["skip_list"]

  type config = {max_items : int}

  let default_config = {max_items = 10000}

  let module_filename = __FILE__

  let group = Benchmark.Group "skip_list"

  let config_encoding =
    let open Data_encoding in
    conv (fun {max_items} -> max_items) (fun max_items -> {max_items}) int31

  type workload = int

  let workload_encoding = Data_encoding.int31

  let workload_to_vector len =
    Sparse_vec.String.of_list [("len", float_of_int @@ len)]

  let model =
    Model.make ~conv:(fun x -> (x, ())) Model.logn ~takes_saturation_reprs:true

  let create_skip_list_of_len len =
    let rec go n cell =
      if n = 0 then cell
      else go (pred n) @@ next ~prev_cell:cell ~prev_cell_ptr:() ()
    in
    go len (genesis ())

  let create_benchmark ~rng_state ({max_items} : config) =
    let workload =
      (* Since the model we want to infer is logarithmic in
         the length, we sample the logarithm of the length
         (and not the length itself) uniformly in an interval. *)
      let logmax = log (float_of_int max_items) in
      let loglen =
        Base_samplers.sample_float_in_interval ~min:0. ~max:logmax rng_state
      in
      int_of_float (exp loglen)
    in
    let prev_cell = create_skip_list_of_len workload in
    let prev_cell_ptr = () in
    let closure () = ignore (next ~prev_cell ~prev_cell_ptr ()) in
    Generator.Plain {workload; closure}
end

(** Benchmark for the [Sc_rollup_inbox_repr.hash_skip_list_cell]
   function. It is used for estimating the parameters for
   [Skip_list_cost_model.model_hash_cell]. The model estimates hashing
   a skip_list cell content and all its back pointers. *)
module Hash_cell : Benchmark.S = struct
  let purpose = Benchmark.Generate_code "skip_list"

  let name = ns "hash_cell"

  let info = "Estimating the costs of hashing a skip list cell"

  let tags = ["skip_list"]

  let module_filename = __FILE__

  let group = Benchmark.Group "skip_list"

  include Skip_list
  module Hash = Sc_rollup_inbox_repr.Hash

  let hash merkelized =
    let payload_hash = Skip_list.content merkelized in
    let back_pointers_hashes = Skip_list.back_pointers merkelized in
    Hash.to_bytes payload_hash :: List.map Hash.to_bytes back_pointers_hashes
    |> Hash.hash_bytes

  type config = {max_index : int}

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_index} -> max_index)
      (fun max_index -> {max_index})
      (obj1 (req "max_index" int31))

  let default_config = {max_index = 65536}

  type workload = {nb_backpointers : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun {nb_backpointers} -> nb_backpointers)
      (fun nb_backpointers -> {nb_backpointers})
      (obj1 (req "max_nb_backpointers" int31))

  let workload_to_vector {nb_backpointers} =
    Sparse_vec.String.of_list
      [("nb_backpointers", float_of_int nb_backpointers)]

  let model =
    Model.make
      ~takes_saturation_reprs:true
      ~conv:(fun {nb_backpointers} -> (nb_backpointers, ()))
      Model.affine

  let create_benchmark ~rng_state conf =
    (* Since the model we want to infer is logarithmic in
       the length, we sample the logarithm of the length
       (and not the length itself) uniformly in an interval. *)
    let skip_list_loglen =
      let logmax = log (float_of_int conf.max_index) in
      Base_samplers.sample_float_in_interval ~min:0. ~max:logmax rng_state
    in
    let skip_list_len = int_of_float (exp skip_list_loglen) in
    let random_hash () =
      Hash.hash_string
        [Base_samplers.string ~size:{min = 32; max = 32} rng_state]
    in
    let cell =
      let rec repeat n cell =
        if n = 0 then cell
        else
          let prev_cell = cell and prev_cell_ptr = hash cell in
          repeat
            (n - 1)
            (Skip_list.next ~prev_cell ~prev_cell_ptr (random_hash ()))
      in
      repeat skip_list_len (Skip_list.genesis (random_hash ()))
    in
    let nb_backpointers = List.length (Skip_list.back_pointers cell) in
    let workload = {nb_backpointers} in
    let closure () = ignore (hash cell) in
    Generator.Plain {workload; closure}
end

let () = Registration.register (module Next)

let () = Registration.register (module Hash_cell)
