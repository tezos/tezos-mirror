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

(** A benchmark for estimating the gas cost of
    {!Sc_rollup_costs.Constants.cost_update_num_and_size_of_messages}. This
    value is used to consume the gas cost internally in
    [Sc_rollup_storage.add_messages], when computing the number of messages
    and their totla size in bytes to be added to an inbox.
*)

module Sc_rollup_update_num_and_size_of_messages_benchmark = struct
  let name = "Sc_rollup_update_num_and_size_of_messages"

  let info =
    "Estimating the cost of updating the number and total size of messages \
     when adding a message to a sc_rollup inbox"

  let tags = ["scoru"]

  type config = {
    max_num_messages : int;
    max_messages_size : int;
    max_new_message_size : int;
  }

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_num_messages; max_messages_size; max_new_message_size} ->
        (max_num_messages, max_messages_size, max_new_message_size))
      (fun (max_num_messages, max_messages_size, max_new_message_size) ->
        {max_num_messages; max_messages_size; max_new_message_size})
      (obj3
         (req "max_num_of_messages" int31)
         (req "max_messages_size" int31)
         (req "max_new_message_size" int31))

  let default_config =
    {
      max_num_messages = 100;
      max_messages_size = 1000;
      max_new_message_size = 100;
    }

  type workload = unit

  let workload_encoding = Data_encoding.unit

  let workload_to_vector () = Sparse_vec.String.of_list []

  let cost_update_num_and_size_ofmessages_model =
    Model.make
      ~conv:(fun () -> ())
      ~model:
        (Model.unknown_const2
           ~const1:Builtin_benchmarks.timer_variable
           ~const2:
             (Free_variable.of_string "cost_update_num_and_size_of_messages"))

  let models = [("scoru", cost_update_num_and_size_ofmessages_model)]

  let benchmark rng_state conf () =
    let num_messages =
      Base_samplers.sample_in_interval
        ~range:{min = 0; max = conf.max_num_messages}
        rng_state
    in
    let total_messages_size =
      Base_samplers.sample_in_interval
        ~range:{min = 0; max = conf.max_messages_size}
        rng_state
    in
    let new_message_size =
      Base_samplers.sample_in_interval
        ~range:{min = 0; max = conf.max_new_message_size}
        rng_state
    in
    let new_message =
      Base_samplers.uniform_string ~nbytes:new_message_size rng_state
    in
    let workload = () in
    let closure () =
      ignore
        (Sc_rollup_storage.Internal.update_num_and_size_of_messages
           ~num_messages
           ~total_messages_size
           new_message)
    in
    Generator.Plain {workload; closure}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (benchmark rng_state config)

  let () =
    Registration.register_for_codegen
      name
      (Model.For_codegen cost_update_num_and_size_ofmessages_model)
end

(** A benchmark for estimating the gas cost of {!Sc_rollup.Inbox.add_messages}.
    We assume that the cost (in gas) [cost(n, l)] of adding a message of size
    [n] bytes, at level [l] since the origination of the rollup, satisfies the
    equation [cost(n) = c_0 + c_1 * n + c_2 * log(l)], where [c_0], [c_1] and
    [c_2] are the values to be benchmarked. We also assume that the cost of
    adding messages [m_0, ..., m_k] to a rollup inbox is
    [\sum_{i=0}^{k} cost(|m_i|, l)]. Thus, it suffices to estimate the cost of 
    adding a single message to the inbox.
*)

module Sc_rollup_add_messages_benchmark = struct
  let name = "Sc_rollup_inbox_add_message"

  let info = "Estimating the costs of adding a single message to a rollup inbox"

  let tags = ["scoru"]

  type config = {max_length : int; max_level : int}

  let config_encoding =
    let open Data_encoding in
    conv
      (fun {max_length; max_level} -> (max_length, max_level))
      (fun (max_length, max_level) -> {max_length; max_level})
      (obj2 (req "max_bytes" int31) (req "max_level" int31))

  let default_config = {max_length = 1 lsl 16; max_level = 255}

  type workload = {message_length : int; level : int}

  let workload_encoding =
    let open Data_encoding in
    conv
      (fun {message_length; level} -> (message_length, level))
      (fun (message_length, level) -> {message_length; level})
      (obj2 (req "message_length" int31) (req "inbox_level" int31))

  let workload_to_vector {message_length; level} =
    Sparse_vec.String.of_list
      [
        ("message_length", float_of_int message_length);
        ("inbox_level", float_of_int level);
      ]

  let add_message_model =
    Model.make
      ~conv:(fun {message_length; level} -> (message_length, (level, ())))
      ~model:
        (Model.n_plus_logm
           ~intercept:(Free_variable.of_string "cost_add_message_intercept")
           ~linear_coeff:(Free_variable.of_string "cost_add_message_per_byte")
           ~log_coeff:(Free_variable.of_string "cost_add_message_per_level"))

  let models = [("scoru", add_message_model)]

  let benchmark rng_state conf () =
    let message =
      Base_samplers.string rng_state ~size:{min = 1; max = conf.max_length}
    in
    let last_level_int =
      Base_samplers.sample_in_interval
        ~range:{min = 1; max = conf.max_level}
        rng_state
    in
    let last_level =
      Raw_level_repr.of_int32_exn (Int32.of_int last_level_int)
    in
    let message_length = String.length message in

    let new_ctxt =
      let open Lwt_result_syntax in
      let* block, _ = Context.init1 () in
      let+ b = Incremental.begin_construction block in
      let state = Incremental.validation_state b in
      let ctxt = state.ctxt in
      (* Necessary to originate rollups. *)
      let ctxt =
        Alpha_context.Origination_nonce.init ctxt Operation_hash.zero
      in
      Alpha_context.Internal_for_tests.to_raw ctxt
    in

    let ctxt_with_rollup =
      let open Lwt_result_syntax in
      let* ctxt = new_ctxt in
      let+ rollup, _size, ctxt =
        Lwt.map Environment.wrap_tzresult
        @@ Sc_rollup_storage.originate ctxt ~kind:Example_arith ~boot_sector:""
      in
      (rollup, ctxt)
    in

    let add_message_and_increment_level ctxt rollup =
      let open Lwt_result_syntax in
      let+ inbox, _, ctxt =
        Lwt.map Environment.wrap_tzresult
        @@ Sc_rollup_storage.add_messages ctxt rollup ["CAFEBABE"]
      in
      let ctxt = Raw_context.Internal_for_tests.add_level ctxt 1 in
      (inbox, ctxt)
    in

    let prepare_benchmark_scenario () =
      let open Lwt_result_syntax in
      let rec add_messages_for_level ctxt inbox rollup =
        if Raw_level_repr.((Raw_context.current_level ctxt).level > last_level)
        then return (inbox, ctxt)
        else
          let* inbox, ctxt = add_message_and_increment_level ctxt rollup in
          add_messages_for_level ctxt inbox rollup
      in
      let* rollup, ctxt = ctxt_with_rollup in
      let inbox =
        Sc_rollup_inbox_repr.empty rollup (Raw_context.current_level ctxt).level
      in
      let* inbox, ctxt = add_messages_for_level ctxt inbox rollup in
      let+ messages, _ctxt =
        Lwt.return @@ Environment.wrap_tzresult
        @@ Raw_context.Sc_rollup_in_memory_inbox.current_messages ctxt rollup
      in
      (inbox, messages)
    in

    let inbox, current_messages =
      match Lwt_main.run @@ prepare_benchmark_scenario () with
      | Ok result -> result
      | Error _ -> assert false
    in

    let workload = {message_length; level = last_level_int} in
    let closure () =
      ignore
        (Sc_rollup_inbox_repr.add_messages_no_history
           inbox
           last_level
           [message]
           current_messages)
    in
    Generator.Plain {workload; closure}

  let create_benchmarks ~rng_state ~bench_num config =
    List.repeat bench_num (benchmark rng_state config)

  let () =
    Registration.register_for_codegen name (Model.For_codegen add_message_model)
end

let () = Registration_helpers.register (module Sc_rollup_add_messages_benchmark)

let () =
  Registration_helpers.register
    (module Sc_rollup_update_num_and_size_of_messages_benchmark)
