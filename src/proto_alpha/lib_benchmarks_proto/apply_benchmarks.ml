(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_benchmark
open Benchmarks_proto

let ns = Namespace.make Registration.ns "apply"

let fv s = Free_variable.of_namespace (ns s)

let initial_balance = 4_000_000_000_000L

let make_context ~rng_state =
  let open Lwt_result_syntax in
  let* block, (_, src, dst) =
    Context.init3
      ~rng_state
      ~bootstrap_balances:[initial_balance; initial_balance; initial_balance]
      ()
  in
  Context.get_constants (B block) >>=? fun csts ->
  let minimal_block_delay =
    Protocol.Alpha_context.Period.to_seconds csts.parametric.minimal_block_delay
  in
  Incremental.begin_construction
    ~timestamp:
      (Time.Protocol.add block.header.shell.timestamp minimal_block_delay)
    block
  >>=? fun vs ->
  let ctxt = Incremental.alpha_ctxt vs in
  let ctxt =
    (* Required for eg Create_contract *)
    Protocol.Alpha_context.Origination_nonce.init
      ctxt
      Tezos_crypto.Hashed.Operation_hash.zero
  in
  return (ctxt, src, dst)

(* This benchmark is introduced for !7761 and #4788. Its result is not used in the protocol
   therefore we do not need to perform it regularly.
*)
module Take_fees_benchmark = struct
  let name = ns "Take_fees"

  let info = "Benchmark for take_fees"

  let module_filename = __FILE__

  let generated_code_destination = None

  let group = Benchmark.Standalone

  let tags = ["apply"]

  type config = unit

  let config_encoding = Data_encoding.unit

  let default_config = ()

  type workload = {batch_length : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun {batch_length} -> batch_length)
      (fun batch_length -> {batch_length})
      (obj1 (req "batch_length" int31))

  let workload_to_vector {batch_length} =
    Sparse_vec.String.of_list [("batch_length", float_of_int batch_length)]

  let model =
    Model.make
      ~conv:(fun {batch_length} -> (batch_length, ()))
      ~model:Model.affine

  let create_benchmark ~rng_state _conf =
    let open Annotated_manager_operation in
    let open Alpha_context in
    let open Lwt_result_syntax in
    let batch_length =
      Base_samplers.sample_in_interval ~range:{min = 1; max = 100} rng_state
    in
    let workload = {batch_length} in
    let closure_result =
      Lwt_main.run
        (let* ctxt, src, dest = make_context ~rng_state in
         let* parameters = Client_proto_context.parse_arg_transfer None in
         let transaction =
           Transaction
             {
               amount = Tez.one;
               parameters;
               entrypoint = Entrypoint_repr.default;
               destination = dest;
             }
         in
         let pkh = match src with Implicit pkh -> pkh | _ -> assert false in
         let manager_info =
           Manager_info
             {
               source = Some pkh;
               fee = Limit.known Tez.one;
               gas_limit = Limit.known (Gas.Arith.integral_exn (Z.of_int 2000));
               storage_limit = Limit.known (Z.of_int 10);
               counter = Some (Manager_counter.Internal_for_tests.of_int 0);
               operation = transaction;
             }
         in
         let tr = Annotated_manager_operation manager_info in
         let transaction_list = List.repeat batch_length tr in
         let (Manager_list annotated_list) = manager_of_list transaction_list in
         let* batch = Lwt.return (manager_list_from_annotated annotated_list) in
         let closure () =
           Protocol.Apply.Internal_for_benchmark.take_fees ctxt batch
         in
         return closure)
    in
    let closure =
      match closure_result with
      | Ok c -> c
      | Error _ -> assert false (* TODO better error *)
    in
    Generator.Plain {workload; closure}
end

let () = Benchmarks_proto.Registration.register (module Take_fees_benchmark)
