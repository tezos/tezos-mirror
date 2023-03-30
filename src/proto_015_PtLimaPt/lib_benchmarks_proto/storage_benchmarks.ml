(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** {2 [Storage_functors] benchmarks}.

    This module registers a benchmark [List_key_values_benchmark].  Its result
    is used to fill in the corresponding value, [list_key_values_step]
    defined in [Storage_costs].
  *)

open Tezos_benchmark
open Storage_functors
open Protocol

let ns = Namespace.of_string

(** Creates a dummy raw-context value. *)
let default_raw_context () =
  let initial_accounts =
    Account.generate_accounts ~initial_balances:[100_000_000_000L] 1
  in
  let bootstrap_accounts =
    List.map
      (fun (Account.{pk; pkh; _}, amount, delegate_to) ->
        Default_parameters.make_bootstrap_account
          (pkh, pk, amount, delegate_to, None))
      initial_accounts
  in
  Block.prepare_initial_context_params initial_accounts
  >>=? fun (constants, _, _) ->
  let parameters =
    Default_parameters.parameters_of_constants
      ~bootstrap_accounts
      ~commitments:[]
      constants
  in
  let json = Default_parameters.json_of_parameters parameters in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  let protocol_param_key = ["protocol_parameters"] in
  Tezos_protocol_environment.Context.(
    let empty = Tezos_protocol_environment.Memory_context.empty in
    add empty ["version"] (Bytes.of_string "genesis") >>= fun ctxt ->
    add ctxt protocol_param_key proto_params)
  >>= fun context ->
  let typecheck ctxt script_repr = return ((script_repr, None), ctxt) in
  Init_storage.prepare_first_block
    Chain_id.zero
    context
    ~level:0l
    ~timestamp:(Time.Protocol.of_seconds 1643125688L)
    ~typecheck
  >>= fun e -> Lwt.return @@ Environment.wrap_tzresult e

module String = struct
  type t = string

  let encoding = Data_encoding.string
end

module Int32 = struct
  type t = int32

  let encoding = Data_encoding.int32

  module Index = struct
    type t = int

    let path_length = 1

    let to_path c l = string_of_int c :: l

    let of_path = function
      | [] | _ :: _ :: _ -> None
      | [c] -> int_of_string_opt c

    type 'a ipath = 'a * t

    let args =
      Storage_description.One
        {
          rpc_arg = Environment.RPC_arg.int;
          encoding = Data_encoding.int31;
          compare = Compare.Int.compare;
        }
  end
end

module Root_raw_context =
  Make_subcontext (Registered) (Raw_context)
    (struct
      let name = ["benchmark_storage_functors"]
    end)

module Indexed_context =
  Make_indexed_subcontext
    (Make_subcontext (Registered) (Root_raw_context)
       (struct
         let name = ["index"]
       end))
       (Int32.Index)

module Table =
  Make_indexed_carbonated_data_storage
    (Make_subcontext (Registered) (Raw_context)
       (struct
         let name = ["table_for_list_key_values"]
       end))
       (Int32.Index)
    (struct
      type t = string

      let encoding = Data_encoding.string
    end)

module List_key_values_benchmark_boilerplate = struct
  type config = {max_size : int}

  let name = "List_key_values"

  let info = "List key values"

  let module_filename = __FILE__

  let generated_code_destination = None

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_size} -> max_size)
      (fun max_size -> {max_size})
      (obj1 (req "max_size" int31))

  let default_config = {max_size = 100_000}

  type workload = {size : int}

  let tags = ["big_map"]

  let workload_encoding =
    let open Data_encoding in
    conv (fun {size} -> size) (fun size -> {size}) (obj1 (req "size" int31))

  let workload_to_vector {size} =
    Sparse_vec.String.of_list [("size", float_of_int size)]

  let models =
    [
      ( "list_key_values",
        Model.make
          ~conv:(fun {size} -> (size, ()))
          ~model:
            (Model.affine
               ~name:(ns name)
               ~intercept:(Free_variable.of_string "list_key_values_intercept")
               ~coeff:(Free_variable.of_string "list_key_values_step")) );
    ]

  let name = ns name
end

module List_key_values_benchmark = struct
  include List_key_values_benchmark_boilerplate

  let benchmark rng_state {max_size} () =
    let wrap m = m >|= Environment.wrap_tzresult in
    let size =
      Base_samplers.sample_in_interval
        ~range:{min = 1; max = max_size}
        rng_state
    in
    let ctxt =
      let fill_table =
        let open Lwt_result_syntax in
        let* ctxt = default_raw_context () in
        List.fold_left_es
          (fun ctxt (key, value) ->
            let* ctxt, _, _ = wrap @@ Table.add ctxt key value in
            return ctxt)
          ctxt
          (Stdlib.List.init size (fun key -> (key, string_of_int key)))
      in
      match Lwt_main.run fill_table with Ok ctxt -> ctxt | _ -> assert false
    in
    let workload = {size} in
    let closure () =
      (* We pass length [0] so that none of the steps of the fold over the
         key-value pairs load any values. That is isolate the cost of iterating
         over the tree without loading values. *)
      Table.list_key_values ~length:0 ctxt |> Lwt_main.run |> ignore
    in
    Generator.Plain {workload; closure}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (benchmark rng_state config)
end

module List_key_values_benchmark_intercept = struct
  include List_key_values_benchmark_boilerplate

  let name = Namespace.of_string @@ Namespace.to_string name ^ "_intercept"

  let benchmark _rng_state _config () =
    let ctxt =
      match Lwt_main.run (default_raw_context ()) with
      | Ok ctxt -> ctxt
      | _ -> assert false
    in
    let workload = {size = 0} in
    let closure () =
      (* We pass length [0] so that none of the steps of the fold over the
         key-value pairs load any values. That is isolate the cost of iterating
         over the tree without loading values. *)
      Table.list_key_values ~length:0 ctxt |> Lwt_main.run |> ignore
    in
    Generator.Plain {workload; closure}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (benchmark rng_state config)
end

let () = Registration_helpers.register (module List_key_values_benchmark)

let () =
  Registration_helpers.register (module List_key_values_benchmark_intercept)
