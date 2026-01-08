(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

let is_tracing_root_indexed_by_hash hash state =
  let open Lwt_result_syntax in
  let*! exists =
    Evm_state.exists
      state
      (Durable_storage_path.Trace.root_indexed_by_hash
         ~transaction_hash:(Some hash))
  in
  if exists then return @@ Some hash else return None

let read_value ?default state path =
  let open Lwt_result_syntax in
  let*! value = Evm_state.inspect state path in
  match value with
  | Some value -> return value
  | None -> (
      match default with
      | Some d -> return d
      | None -> tzfail Tracer_types.Trace_not_found)

let check_tracer_activation ~state ~version =
  let open Lwt_result_syntax in
  let* storage_version =
    let read key =
      let*! res = Evm_state.inspect state key in
      return res
    in
    Durable_storage.storage_version read
  in
  if storage_version < version then tzfail Tracer_types.Tracer_not_activated
  else return_unit

module StructLoggerRead = struct
  let read_logs_length state transaction_hash =
    let open Lwt_result_syntax in
    let* value =
      read_value
        ~default:(Bytes.of_string "\000")
        state
        (Durable_storage_path.Trace.logs_length ~transaction_hash)
    in
    return (Bytes.to_string value |> Z.of_bits |> Z.to_int)

  let read_opcode state transaction_hash opcode_index =
    read_value
      state
      (Durable_storage_path.Trace.opcode ~transaction_hash opcode_index)

  let read_logs state transaction_hash =
    let open Lwt_result_syntax in
    let* length = read_logs_length state transaction_hash in
    let*? opcodes =
      List.init
        ~when_negative_length:(TzTrace.make (error_of_fmt "Invalid length"))
        length
        Fun.id
    in
    List.map_es (read_opcode state transaction_hash) opcodes

  let read_output state transaction_hash =
    let open Lwt_result_syntax in
    let* gas =
      read_value state (Durable_storage_path.Trace.output_gas ~transaction_hash)
    in
    let* failed =
      read_value
        state
        (Durable_storage_path.Trace.output_failed ~transaction_hash)
    in
    let* return_value =
      read_value
      (* The key doesn't exist if there is no value returned by the transaction *)
        ~default:Bytes.empty
        state
        (Durable_storage_path.Trace.output_return_value ~transaction_hash)
    in
    let* struct_logs = read_logs state transaction_hash in
    Lwt.return
    @@ Tracer_types.StructLogger.output_binary_decoder
         ~gas
         ~failed
         ~return_value
         ~struct_logs
end

module CallTracerRead = struct
  (** Context used during the call trace rebuilding algorithm.

    The kernel stores the call trace sequentially: data for a call (excluding
  internal calls, but including its depth) is stored once a call is finished.
  Therefore, an internal call tree like the following:

  A: (0)
  = B:(1)
  == C:(2)
  = D:(1)

  is stored as 4 lines in the durable storage:

  /.../0/<C at depth 2>
  /.../1/<B at depth 1>
  /.../2/<D at depth 1>
  /.../3/<A at depth 0>

  If we read that list from the end, (A0,D1,B1,C2) it corresponds to a depth
  first search of the tree, with the children of a node read from last
  finished (in the execution order) to first finished.

    The algorithm used to rebuild the tree from the list of call read each line
    consecutively and acts depending on the line and its internal state:
        - call_list : a list of childrens at the same depth
        - depth : the depth of the last node read
        - stack : the context of the call (parts of the tree already
        reconstructed) at the top of the stack is the parent of the last node
        read, underneath is its parent, etc.

    The algorithm has three operations:
        - add at same depth: the node being read is at the same depth, so it's a
        child of the same node as the previous node read, so we add the current
        node in the `call_list`
        - push at next depth: the node being read is a children of the last node
        read. So we push the `call_list` on the stack and increase the depth
        counter
        - collapse : the node being read is at a lower depth, so we've seen all
        nodes at the last depth. Therefore we add all the nodes in
        `call_list` to their parent: its the first node in the list at the top
        of the stack.
        That operation might need to be repeated multiple times in a row: enough
        to find nodes which were seen at the same depth as the current node.

   *)
  type 'a cont = {call_list : 'a list; depth : int; stack : 'a list list}

  (* `value` is a node at the same depth *)
  let add_at_same_depth value {call_list; depth; stack} =
    {call_list = value :: call_list; depth; stack}

  (* `value` is a child of the last node seen. We push the `call_list` on the
     stack and start a new list of children *)
  let push_at_next_depth value {call_list; depth; stack} =
    {call_list = [value]; depth = depth + 1; stack = call_list :: stack}

  (* We have finished exploring a subtree, so we can add the list of calls to
     their parent.*)
  let collapse_one_level end_call {depth; stack; call_list} =
    match stack with
    | (current_call :: ended_calls) :: stack_rest ->
        let ended_current_call = end_call current_call call_list in
        Ok
          {
            call_list = ended_current_call :: ended_calls;
            depth = depth - 1;
            stack = stack_rest;
          }
    | _ -> Error (error_of_fmt "Tried to collapse too much")

  (* We can collapse the stack on `n` levels *)
  let rec collaspe_n_level end_call n cont =
    let open Result_syntax in
    if n = 0 then return cont
    else
      let* once = collapse_one_level end_call cont in
      collaspe_n_level end_call (n - 1) once

  (* The algorithm needs to process a new node `value` at depth `next_depth`.
     There are three valid possibilities:
         - next_depth = last_depth: the node being read is at the same depth.
         We `add`
         - next_depth = last_depth + 1: the node being read is a child of the
         last one. We `push`
         - next_depth < last_depth: we have finished reading a subtree.
         We `collaspe` a number of time (enough time to go back to next_depth)
  *)
  let process_value end_call next next_depth context =
    let open Result_syntax in
    let depth = context.depth in
    if next_depth = depth then return @@ add_at_same_depth next context
    else if next_depth = depth + 1 then
      return @@ push_at_next_depth next context
    else if next_depth < depth then
      let* new_context =
        collaspe_n_level end_call (depth - next_depth) context
      in
      return @@ add_at_same_depth next new_context
    else Error (error_of_fmt "missing levels")

  let build_calltraces end_call get_next =
    let open Lwt_result_syntax in
    let rec iter counter context =
      let depth = context.depth in
      let* storage_line = get_next counter in
      match storage_line with
      | Some (next, next_depth) -> (
          match process_value end_call next next_depth context with
          | Ok new_context -> iter (counter + 1) new_context
          | Error e -> tzfail e)
      | None -> (
          match collaspe_n_level end_call depth context with
          | Ok new_context -> return new_context
          | Error e -> tzfail e)
    in
    let empty_context = {call_list = []; depth = 0; stack = []} in
    let* {call_list; _} = iter 0 empty_context in
    return call_list

  let build_calltrace end_call get_next =
    let open Lwt_result_syntax in
    let* call_list = build_calltraces end_call get_next in
    match call_list with
    | [o] -> return o
    | _ -> tzfail (error_of_fmt "Multiple top call")

  let end_call_impl (node : Tracer_types.CallTracer.output) list =
    let open Tracer_types.CallTracer in
    {node with calls = list}

  (** [read_node evm_state i hash] reads line `i` of the trace of tx `hash` *)
  let read_node ~state i transaction_hash =
    let open Lwt_result_syntax in
    let* bytes =
      read_value
        state
        (Durable_storage_path.Trace.call_trace ~transaction_hash i)
    in
    let*! () =
      Tracer_event.read_line
        (Z.of_int i, Option.value transaction_hash ~default:"none")
    in
    Lwt.return @@ Tracer_types.CallTracer.decode_call bytes

  (** [call_trace_length evm_state hash] is used to findout how many lines
        there are, to be able to read them in reverse order. *)
  let call_trace_length state transaction_hash =
    let open Lwt_result_syntax in
    let* value =
      read_value
        ~default:(Bytes.of_string "\000")
        state
        (Durable_storage_path.Trace.call_trace_length ~transaction_hash)
    in
    return (Bytes.to_string value |> Z.of_bits |> Z.to_int)

  (** [read_outputs state] reads in the storage and interprets what
     the kernel stored. *)
  let read_outputs ?transaction_hash state =
    let open Lwt_result_syntax in
    let* length = call_trace_length state transaction_hash in
    (* there should at least be the top call *)
    if length = 0 then tzfail Tracer_types.Trace_not_found
    else
      let get_next counter =
        let index = length - 1 - counter in
        if index < 0 then return None
        else
          let* node = read_node ~state index transaction_hash in
          return (Some node)
      in
      build_calltraces end_call_impl get_next

  (** [read_output state hash] reads in the storage and interprets what
     the kernel stored. *)
  let read_output state transaction_hash =
    let open Lwt_result_syntax in
    let* outputs = read_outputs ?transaction_hash state in
    match outputs with
    | o :: _ -> return o
    | _ -> tzfail Tracer_types.Trace_not_found
end

let read_output config state root_indexed_hash =
  let open Tracer_types in
  let open Lwt_result_syntax in
  match config.tracer with
  | StructLogger ->
      let* output = StructLoggerRead.read_output state root_indexed_hash in
      return (StructLoggerOutput output)
  | CallTracer ->
      let* output = CallTracerRead.read_output state root_indexed_hash in
      return (CallTracerOutput output)

let read_outputs config state =
  let open Tracer_types in
  let open Lwt_result_syntax in
  match config.tracer with
  | CallTracer ->
      Lwt_result.bind
        (CallTracerRead.read_outputs state)
        (List.map_es (fun o -> return @@ CallTracerOutput o))
  | StructLogger -> tzfail @@ Tracer_types.Tracer_not_implemented "structLogger"

let trace_transaction (module Exe : Evm_execution.S) ~block_number
    ~transaction_hash ~config =
  let open Lwt_result_syntax in
  let*! () = Tracer_event.tracer_input (Tracer_types.config_to_string config) in
  let input = Tracer_types.input_rlp_encoder ~hash:transaction_hash config in
  let set_input state =
    let* () =
      check_tracer_activation
        ~state
        ~version:Tracer_types.(tracer_version_activation config.tracer)
    in
    let*! state =
      Evm_state.modify ~key:Durable_storage_path.Trace.input ~value:input state
    in
    return state
  in
  let* apply_result = Exe.replay ~alter_evm_state:set_input block_number in
  match apply_result with
  | Apply_failure -> tzfail Tracer_types.Trace_not_found
  | Apply_success {evm_state; _} ->
      let (Hash (Hex hash)) = transaction_hash in
      let* root_indexed_by_hash =
        is_tracing_root_indexed_by_hash hash evm_state
      in
      read_output config evm_state root_indexed_by_hash

let trace_block (module Exe : Evm_execution.S)
    (module Block_storage : Block_storage_sig.S) ~block_number ~config =
  let open Lwt_result_syntax in
  let*! () = Tracer_event.tracer_input (Tracer_types.config_to_string config) in
  (* only implemented for callTracer *)
  let* () =
    match config.Tracer_types.tracer with
    | CallTracer -> return_unit
    | StructLogger ->
        tzfail @@ Tracer_types.Tracer_not_implemented "structLogger"
  in
  (* first get the transaction hashes *)
  let* block =
    Block_storage.nth_block
      ~full_transaction_object:false
      (Ethereum_types.Qty.to_z block_number)
  in
  let hashes =
    match block.transactions with
    | TxHash hashes -> hashes
    | TxFull _ -> assert false
  in
  if List.is_empty hashes then return []
  else
    (* then get the traces *)
    let input = Tracer_types.input_rlp_encoder config in
    let set_input state =
      let* () =
        check_tracer_activation
          ~state
          ~version:Tracer_types.(tracer_version_activation config.tracer)
      in
      let*! state =
        Evm_state.modify
          ~key:Durable_storage_path.Trace.input
          ~value:input
          state
      in
      return state
    in
    let* apply_result = Exe.replay ~alter_evm_state:set_input block_number in
    let* traces =
      match apply_result with
      | Apply_failure -> tzfail Tracer_types.Trace_not_found
      | Apply_success {evm_state; _} -> read_outputs config evm_state
    in
    (* Now assemble the hash and traces *)
    List.combine
      ~when_different_lengths:
        [
          Tracer_types.Inconsistent_traces
            {
              block = block_number;
              nb_txs = List.length hashes;
              nb_traces = List.length traces;
            };
        ]
      hashes
      traces
    |> Lwt.return

let trace_call (module Exe : Evm_execution.S) ~call ~block ~config =
  let open Lwt_result_syntax in
  let config_rlp = Tracer_types.input_rlp_encoder config in
  let set_config state =
    let* () =
      check_tracer_activation
        ~state
        ~version:Tracer_types.(tracer_version_activation config.tracer)
    in
    let*! state =
      Evm_state.modify
        ~key:Durable_storage_path.Trace.input
        ~value:config_rlp
        state
    in
    return state
  in
  let*? messages = Simulation.(encode (V1 {call; with_da_fees = false})) in
  let simulation_input =
    Simulation.Encodings.
      {
        messages;
        reveal_pages = None;
        insight_requests = [];
        log_kernel_debug_file = Some "traceCall";
      }
  in
  let* evm_state =
    Exe.execute ~alter_evm_state:set_config simulation_input block
  in
  read_output config evm_state None
