(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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

let ns = Namespace.make Registration_helpers.ns "carbonated_map"

let fv s = Free_variable.of_namespace (ns s)

let make_context ~rng_state =
  match Lwt_main.run @@ Execution_context.make ~rng_state with
  | Ok (ctxt, _) -> ctxt
  | Error _ -> assert false

module Config_and_workload = struct
  type config = {size : int}

  let config_encoding =
    let open Data_encoding in
    conv (fun {size} -> size) (fun size -> {size}) (obj1 (req "size" int31))

  let default_config = {size = 100_000}

  type workload = config

  let module_filename = __FILE__

  let purpose = Benchmark.Other_purpose "No longer used to generate code"

  let tags = ["carbonated_map"]

  let workload_encoding = config_encoding

  let workload_to_vector {size} =
    Sparse_vec.String.of_list [("size", float_of_int size)]
end

module Alpha_context_gas = struct
  type context = Alpha_context.context

  let consume = Alpha_context.Gas.consume
end

(**
  Benchmarks the [fold] functions of [Carbonated_map].
  This benchmark does not depend on the size of the keys or types of elements.
*)
module Fold_benchmark : Benchmark.S = struct
  include Config_and_workload

  module Int = struct
    include Int

    (** Dummy value *)
    let compare_cost _ = Saturation_repr.safe_int 0
  end

  let name = ns "fold"

  let info = "Carbonated map to list"

  let fold_model =
    Model.make
      ~conv:(fun {size} -> (size, ()))
      (Model.affine
         ~name
         ~intercept:(fv "fold_const")
         ~coeff:(fv "fold_cost_per_item"))

  let models = [("carbonated_map", fold_model)]

  let benchmark rng_state config () =
    let module M = Carbonated_map.Make (Alpha_context_gas) (Int) in
    let _, list =
      let sampler rng_state =
        let key = Base_samplers.int rng_state ~size:{min = 1; max = 5} in
        (* Value should not be important *)
        let value = () in
        (Z.to_int key, value)
      in
      Structure_samplers.list
        rng_state
        ~range:{min = 1; max = config.size}
        ~sampler
    in
    let ctxt = make_context ~rng_state in
    let map =
      match
        M.of_list
          ctxt
          ~merge_overlap:(fun ctxt _left right -> Ok (right, ctxt))
          list
      with
      | Ok (map, _) -> map
      | _ -> assert false
    in
    let workload = {size = M.size map} in
    let closure () =
      ignore @@ M.fold_e ctxt (fun ctxt _ _ _ -> ok ((), ctxt)) () map
    in
    Generator.Plain {workload; closure}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (benchmark rng_state config)
end

(** Module type that consists of a comparable type along with a sampler
   function. *)
module type COMPARABLE_SAMPLER = sig
  include Compare.COMPARABLE

  val type_name : string

  val sampler : t Base_samplers.sampler
end

(**
  Functor for constructing a benchmark for the cost of comparing values. This
  functor can be used to generate [compare_cost] data for a particular
  key-type for [Carbonated_map] instances.
*)
module Make (CS : COMPARABLE_SAMPLER) = struct
  let compare_var type_name = fv (Printf.sprintf "compare_%s" type_name)

  module Compare = struct
    type config = unit

    let config_encoding = Data_encoding.unit

    let default_config = ()

    type workload = config

    let tags = ["carbonated_map"]

    let workload_encoding = config_encoding

    let workload_to_vector () = Sparse_vec.String.of_list []

    let name = ns @@ Printf.sprintf "compare_%s" CS.type_name

    let info =
      Printf.sprintf "Carbonated map compare cost for %s keys" CS.type_name

    let module_filename = __FILE__

    let purpose = Benchmark.Other_purpose "No longer used to generate code"

    let models =
      [
        ( "carbonated_map",
          Model.make
            ~conv:(fun () -> ())
            (Model.unknown_const1 ~name ~const:(compare_var CS.type_name)) );
      ]

    let benchmark rng_state _conf () =
      let key = CS.sampler rng_state in
      let workload = () in
      let closure () = ignore (CS.compare key key) in
      Generator.Plain {workload; closure}

    let create_benchmarks ~rng_state ~bench_num config =
      List.repeat bench_num (benchmark rng_state config)
  end

  module Find = struct
    include Config_and_workload

    module M =
      Carbonated_map.Make
        (Alpha_context_gas)
        (struct
          include CS

          (** Dummy cost*)
          let compare_cost _ = Saturation_repr.safe_int 0
        end)

    let name = ns "find"

    let info = Printf.sprintf "Carbonated find model"

    (**
       Given the cost of comparing keys, the model is used for deducing [intercept]
       and [traverse_overhead] from:

       [intercept + (log2 size * compare_cost) + (log2 size * traversal_overhead)]
     *)
    let find_model ~name ~intercept ~traverse_overhead =
      let module M = struct
        type arg_type = int * unit

        let name = name

        let takes_saturation_reprs = false

        module Def (L : Costlang.S) = struct
          type model_type = L.size -> L.size

          let arity = Model.arity_1

          let model =
            let open L in
            lam ~name:"size" @@ fun size ->
            let compare_cost =
              log2 size * free ~name:(compare_var CS.type_name)
            in
            let traversal_overhead = log2 size * free ~name:traverse_overhead in
            free ~name:intercept + compare_cost + traversal_overhead
        end
      end in
      (module M : Model.Model_impl with type arg_type = int * unit)

    let models =
      [
        ( "carbonated_map",
          Model.make
            ~conv:(fun {size} -> (size, ()))
            (find_model
               ~name
               ~intercept:(fv "intercept")
               ~traverse_overhead:(fv "traversal_overhead")) );
      ]

    let benchmark rng_state (config : config) () =
      let _, list =
        let sampler rng_state = (CS.sampler rng_state, ()) in
        Structure_samplers.list
          rng_state
          ~range:{min = 1; max = config.size}
          ~sampler
      in
      let ctxt = make_context ~rng_state in
      let map =
        match
          M.of_list ctxt ~merge_overlap:(fun ctxt _ _ -> Ok ((), ctxt)) list
        with
        | Ok (map, _) -> map
        | _ -> assert false
      in
      (* Pick the min binding from the map to guarantee longest path. *)
      let key =
        match M.to_list ctxt map with
        | Ok (kvs, _) -> (
            match List.hd kvs with Some (k, _) -> k | None -> assert false)
        | Error _ -> assert false
      in
      let workload = {size = M.size map} in
      let closure () = ignore @@ M.find ctxt key map in
      Generator.Plain {workload; closure}

    let create_benchmarks ~rng_state ~bench_num config =
      List.repeat bench_num (benchmark rng_state config)
  end

  module Find_intercept = struct
    type config = unit

    let config_encoding = Data_encoding.unit

    let default_config = ()

    type workload = config

    let tags = ["carbonated_map"]

    let workload_encoding = config_encoding

    let workload_to_vector () = Sparse_vec.String.of_list []

    module M =
      Carbonated_map.Make
        (Alpha_context_gas)
        (struct
          include CS

          (** Dummy cost*)
          let compare_cost _ = Saturation_repr.safe_int 0
        end)

    let name = ns "find_intercept"

    let info = Printf.sprintf "Carbonated find model (intercept case)"

    let module_filename = __FILE__

    let purpose = Benchmark.Other_purpose "No longer used to generate code"

    let models =
      [
        ( "carbonated_map",
          Model.make
            ~conv:(fun () -> ())
            (Model.unknown_const1 ~name ~const:(fv "intercept")) );
      ]

    let benchmark rng_state (_config : config) () =
      let ctxt = make_context ~rng_state in
      let map = M.empty in
      let key = CS.sampler rng_state in
      let workload = () in
      let closure () = ignore @@ M.find ctxt key map in
      Generator.Plain {workload; closure}

    let create_benchmarks ~rng_state ~bench_num config =
      List.repeat bench_num (benchmark rng_state config)
  end
end

(** A comparable and a sampler for [int] values. *)
module Int = struct
  type t = int

  let compare = Int.compare

  let type_name = "int"

  let sampler rng_state =
    Z.to_int @@ Base_samplers.int rng_state ~size:{min = 1; max = 6}
end

module Benchmarks_int = Make (Int)

let () =
  let open Registration_helpers in
  register (module Fold_benchmark) ;
  register (module Benchmarks_int.Compare) ;
  register (module Benchmarks_int.Find) ;
  register (module Benchmarks_int.Find_intercept)
