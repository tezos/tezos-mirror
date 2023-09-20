(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.com>                        *)
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
open Alpha_context

let ns = Namespace.make Registration_helpers.ns "tickets"

let fv s = Free_variable.of_namespace (ns s)

module Ticket_type_shared = struct
  type config = {max_size : int}

  let default_config = {max_size = Constants_repr.michelson_maximum_type_size}

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_size} -> max_size)
      (fun max_size -> {max_size})
      (obj1 (req "max_size" int31))

  type workload = {nodes : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (function {nodes} -> nodes)
      (fun nodes -> {nodes})
      (obj1 (req "nodes" int31))

  let workload_to_vector {nodes} =
    Sparse_vec.String.of_list [("nodes", float_of_int nodes)]

  let tags = ["tickets"]
end

exception
  Ticket_benchmark_error of {
    benchmark_name : Namespace.t;
    trace : Tezos_base.TzPervasives.tztrace;
  }

(** A benchmark for {!Ticket_costs.Constants.cost_compare_ticket_hash}. *)
module Compare_ticket_hash_benchmark : Benchmark.S = struct
  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  type workload = unit

  let tags = ["tickets"]

  let workload_encoding = Data_encoding.unit

  let workload_to_vector () = Sparse_vec.String.of_list []

  let name = ns "COMPARE_TICKET_HASH"

  let info = "Compare cost for Ticket_hash"

  let module_filename = __FILE__

  let purpose = Benchmark.Other_purpose "No longer used to generate code"

  let compare_model =
    Model.make
      ~conv:(fun () -> ())
      ~model:(Model.unknown_const1 ~name ~const:(fv "compare_ticket_hash"))

  let models = [("compare_tickets", compare_model)]

  let benchmark rng_state _conf () =
    let bytes = Base_samplers.bytes rng_state ~size:{min = 1; max = 64} in
    let hash =
      Ticket_hash.of_script_expr_hash @@ Script_expr_hash.hash_bytes [bytes]
    in
    let hash2 =
      Ticket_hash.of_script_expr_hash @@ Script_expr_hash.hash_bytes [bytes]
    in
    let workload = () in
    let closure () = ignore (Ticket_hash.compare hash hash2) in
    Generator.Plain {workload; closure}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (benchmark rng_state config)
end

let () = Registration_helpers.register (module Compare_ticket_hash_benchmark)

(** A benchmark for {!Ticket_costs.Constants.cost_compare_key_contract}.

    In this benchmark we only compare originated contracts; we never use
    implicit contracts. This is justified partly by the fact that
    currently the carbonated maps only use originated contracts as keys.
    In addition, while developing this benchmark the implicit contracts were
    also tested and gave almost identical timings. *)
module Compare_key_contract_benchmark : Benchmark.S = struct
  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  type workload = unit

  let workload_encoding = Data_encoding.unit

  let workload_to_vector () = Sparse_vec.String.of_list []

  let tags = ["tickets"]

  let name = ns "COMPARE_CONTRACT"

  let info = "Compare cost for Contracts"

  let module_filename = __FILE__

  let purpose = Benchmark.Other_purpose "No longer used to generate code"

  let compare_model =
    Model.make
      ~conv:(fun () -> ())
      ~model:(Model.unknown_const1 ~name ~const:(fv "compare_contract"))

  let models = [("compare_tickets", compare_model)]

  let benchmark rng_state _conf () =
    let bytes = Base_samplers.bytes rng_state ~size:{min = 32; max = 64} in
    let branch = Block_hash.hash_bytes [bytes] in
    let op_hash = Operation.hash_raw {shell = {branch}; proto = bytes} in
    let nonce = Origination_nonce.Internal_for_tests.initial op_hash in
    let contract = Contract.Internal_for_tests.originated_contract nonce in
    let contract2 = Contract.Internal_for_tests.originated_contract nonce in
    let workload = () in
    let closure () = ignore (Contract.compare contract contract2) in
    Generator.Plain {workload; closure}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (benchmark rng_state config)
end

let () = Registration_helpers.register (module Compare_key_contract_benchmark)

(* A simple ticket type for use in the benchmarks. *)
let ticket_ty =
  let open Script_typed_ir in
  WithExceptions.Result.get_ok ~loc:__LOC__ (ticket_t (-1) int_t)

(* A dummy type generator, sampling linear terms of a given size.
   The generator always returns types of the shape:

   [pair int_or_ticket (pair int_or_ticket (pair int_or_ticket ...))]

   This is a worst case type for [type_has_tickets], though nested
   ors, nested maps or nested lists would be just as bad. *)
let rec dummy_type_generator ~rng_state size =
  let open Script_typed_ir in
  let ticket_or_int =
    if Base_samplers.uniform_bool rng_state then Ex_ty ticket_ty
    else Ex_ty int_t
  in
  if size <= 1 then ticket_or_int
  else
    match (ticket_or_int, dummy_type_generator ~rng_state (size - 3)) with
    | Ex_ty l, Ex_ty r -> (
        match pair_t (-1) l r with
        | Error _ -> assert false
        | Ok (Ty_ex_c t) -> Ex_ty t)

(** A benchmark for {!Ticket_costs.Constants.cost_has_tickets_of_ty}. *)
module Has_tickets_type_benchmark : Benchmark.S = struct
  include Ticket_type_shared

  let name = ns "TYPE_HAS_TICKETS"

  let info = "Benchmarking type_has_tickets"

  let module_filename = __FILE__

  let purpose = Benchmark.Other_purpose "No longer used to generate code"

  let make_bench_helper rng_state config () =
    let open Result_syntax in
    let* ctxt, _ = Lwt_main.run (Execution_context.make ~rng_state) in
    let ctxt = Gas_helpers.set_limit ctxt in
    let size = Random.State.int rng_state config.max_size in
    let (Ex_ty ty) = dummy_type_generator ~rng_state size in
    let nodes =
      let size = Script_typed_ir.ty_size ty in
      Saturation_repr.to_int @@ Script_typed_ir.Type_size.to_int size
    in
    let workload = {nodes} in
    let closure () = ignore (Ticket_scanner.type_has_tickets ctxt ty) in
    ok (Generator.Plain {workload; closure})

  let make_bench rng_state config () =
    match make_bench_helper rng_state config () with
    | Ok closure -> closure
    | Error trace ->
        raise (Ticket_benchmark_error {benchmark_name = name; trace})

  let size_model =
    Model.make
      ~conv:(function {nodes} -> (nodes, ()))
      ~model:
        (Model.affine
           ~name
           ~intercept:
             (fv (Format.asprintf "%s_const" (Namespace.basename name)))
           ~coeff:(fv (Format.asprintf "%s_coeff" (Namespace.basename name))))

  let models = [("size_has_tickets_model", size_model)]

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Has_tickets_type_benchmark)

let ticket_sampler rng_state =
  let seed = Base_samplers.uniform_bytes ~nbytes:32 rng_state in
  let pkh, _, _ = Signature.generate_key ~algo:Signature.Ed25519 ~seed () in
  let ticketer = Alpha_context.Contract.Implicit pkh in
  Script_typed_ir.
    {ticketer; contents = Script_int.zero; amount = Ticket_amount.one}

(** A benchmark for {!Ticket_costs.Constants.cost_collect_tickets_step}. *)
module Collect_tickets_benchmark : Benchmark.S = struct
  include Ticket_type_shared

  let name = ns "COLLECT_TICKETS_STEP"

  let info = "Benchmarking tickets_of_value"

  let module_filename = __FILE__

  let purpose = Benchmark.Other_purpose "No longer used to generate code"

  let make_bench_helper rng_state config () =
    let open Script_typed_ir in
    let open Result_syntax in
    let* ctxt, _ = Lwt_main.run (Execution_context.make ~rng_state) in
    let ctxt = Gas_helpers.set_limit ctxt in
    let ty =
      match list_t (-1) ticket_ty with Error _ -> assert false | Ok t -> t
    in
    let _, elements =
      Structure_samplers.list
        ~range:{min = 0; max = config.max_size}
        ~sampler:ticket_sampler
        rng_state
    in
    let boxed_ticket_list = Script_list.of_list elements in
    Environment.wrap_tzresult
    @@ let* has_tickets, ctxt = Ticket_scanner.type_has_tickets ctxt ty in
       let workload = {nodes = Script_list.length boxed_ticket_list} in
       let closure () =
         ignore
           (Lwt_main.run
              (Ticket_scanner.tickets_of_value
                 ctxt
                 ~include_lazy:true
                 has_tickets
                 boxed_ticket_list))
       in
       ok (Generator.Plain {workload; closure})

  let make_bench rng_state config () =
    match make_bench_helper rng_state config () with
    | Ok closure -> closure
    | Error trace ->
        raise (Ticket_benchmark_error {benchmark_name = name; trace})

  let size_model =
    Model.make
      ~conv:(function {nodes} -> (nodes, ()))
      ~model:
        (Model.affine
           ~name
           ~intercept:
             (fv (Format.asprintf "%s_const" (Namespace.basename name)))
           ~coeff:(fv (Format.asprintf "%s_coeff" (Namespace.basename name))))

  let models = [("size_collect_tickets_step_model", size_model)]

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)
end

let () = Registration_helpers.register (module Collect_tickets_benchmark)
