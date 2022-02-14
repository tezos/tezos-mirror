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

(** A benchmark for {!Ticket_costs.Constants.cost_compare_ticket_hash}. *)
module Compare_ticket_hash_benchmark : Benchmark.S = struct
  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  type workload = unit

  let tags = ["tickets"]

  let workload_encoding = Data_encoding.unit

  let workload_to_vector () = Sparse_vec.String.of_list []

  let name = "COMPARE_TICKET_HASH"

  let info = "Compare cost for Ticket_hash"

  let compare_model =
    Model.make
      ~conv:(fun () -> ())
      ~model:
        (Model.unknown_const2
           ~const1:Builtin_benchmarks.timer_variable
           ~const2:(Free_variable.of_string "compare_ticket_hash"))

  let models = [("compare", compare_model)]

  let benchmark rng_state _conf () =
    let bytes = Base_samplers.bytes rng_state ~size:{min = 1; max = 64} in
    let hash =
      Ticket_hash.of_script_expr_hash @@ Script_expr_hash.hash_bytes [bytes]
    in
    let workload = () in
    let closure () = ignore (Ticket_hash.compare hash hash) in
    Generator.Plain {workload; closure}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (benchmark rng_state config)

  let () =
    Registration_helpers.register_for_codegen
      name
      (Model.For_codegen compare_model)
end

let () = Registration_helpers.register (module Compare_ticket_hash_benchmark)

(* A dummy type generator, sampling linear terms of a given size.
   The generator always returns types of the shape:

   [pair int_or_ticket (pair int_or_ticket (pair int_or_ticket ...))]

   This is a worst case type for [type_has_tickets], though nested
   unions, nested maps or nested lists would be just as bad.
 *)
let rec dummy_type_generator ~rng_state size =
  let open Script_ir_translator in
  let open Script_typed_ir in
  let ticket_or_int =
    if Base_samplers.uniform_bool rng_state then
      Ex_ty
        (match ticket_t (-1) int_key with Error _ -> assert false | Ok t -> t)
    else Ex_ty int_t
  in
  if size <= 1 then ticket_or_int
  else
    match (ticket_or_int, dummy_type_generator ~rng_state (size - 3)) with
    | (Ex_ty l, Ex_ty r) ->
        Ex_ty (match pair_t (-1) l r with Error _ -> assert false | Ok t -> t)

(** A benchmark for {!Ticket_costs.Constants.cost_has_tickets_of_ty}. *)
module Has_tickets_type_benchmark : Benchmark.S = struct
  include Translator_benchmarks.Parse_type_shared

  let name = "TYPE_HAS_TICKETS"

  let info = "Benchmarking type_has_tickets"

  let make_bench rng_state config () =
    let open Error_monad in
    ( Lwt_main.run (Execution_context.make ~rng_state) >>? fun (ctxt, _) ->
      let ctxt = Gas_helpers.set_limit ctxt in
      let size = Random.State.int rng_state config.max_size in
      let ty = dummy_type_generator ~rng_state size in
      match ty with
      | Ex_ty ty ->
          Environment.wrap_tzresult @@ Ticket_scanner.type_has_tickets ctxt ty
          >>? fun (_, ctxt') ->
          let consumed =
            Z.to_int
              (Gas_helpers.fp_to_z
                 (Alpha_context.Gas.consumed ~since:ctxt ~until:ctxt'))
          in
          let nodes =
            let size = Script_typed_ir.ty_size ty in
            Saturation_repr.to_int @@ Script_typed_ir.Type_size.to_int size
          in
          let workload = Type_workload {nodes; consumed} in
          let closure () = ignore (Ticket_scanner.type_has_tickets ctxt ty) in
          ok (Generator.Plain {workload; closure}) )
    |> function
    | Ok closure -> closure
    | Error err -> Translator_benchmarks.global_error name err

  let size_model =
    Model.make
      ~conv:(function Type_workload {nodes; consumed = _} -> (nodes, ()))
      ~model:
        (Model.affine
           ~intercept:
             (Free_variable.of_string (Format.asprintf "%s_const" name))
           ~coeff:(Free_variable.of_string (Format.asprintf "%s_coeff" name)))

  let models = [("size_has_tickets_model", size_model)]

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (make_bench rng_state config)

  let () =
    Registration_helpers.register_for_codegen
      name
      (Model.For_codegen size_model)
end

let () = Registration_helpers.register (module Has_tickets_type_benchmark)
