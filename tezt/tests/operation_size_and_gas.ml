(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Requirement:  For dal tests: ./scripts/install_dal_trusted_setup.sh
    Component:    Operation size and gas
    Invocation:   dune exec tezt/tests/main.exe -- --file operation_size_and_gas.ml
    Subject:      Tests size and gas consumption of manager operations.
*)

let team = Tag.layer1

(** Tags shared by all tests in this file. *)
let operation_size_and_gas_tags =
  [team; "operation"; "size"; "gas"; "estimation"; "manager"]

let print_gas_consumed gas_consumed =
  let gas_limit = Float.(to_int (ceil gas_consumed)) in
  Log.info
    ~color:Log.Color.FG.gray
    "Estimated gas consumption is: %f. The gas_limit must be at least of %d \
     gas units for the operation to succeed."
    gas_consumed
    gas_limit

let get_consumed_milligas operation_result =
  Log.info
    ~color:Log.Color.FG.gray
    "operation_result: %s"
    (JSON.encode operation_result) ;
  JSON.(operation_result |> get "consumed_milligas" |> as_int)

(* Different options to compute gas consumption by operation *)
(* Option 1. [operation_process] is the output of [Client.spawn_*] *)
let estimated_gas_consumption operation_process =
  let* stdout = Process.check_and_read_stdout operation_process in
  Log.info ~color:Log.Color.FG.blue "stdout = %s" stdout ;
  let gas_consumed =
    let re = Re.Str.regexp "\\(.\\|[ \\\n]\\)*Consumed gas: \\([0-9.]+\\).*" in
    if Re.Str.string_match re stdout 0 then
      float_of_string (Re.Str.matched_group 2 stdout)
    else
      Test.fail
        "Failed to parse the consumed gas in the following output of the dry \
         run:\n\
         %s"
        stdout
  in
  let () = print_gas_consumed gas_consumed in
  return gas_consumed

(* Option 2. Requires the hash of an operation that has been baked
   into a block. *)
(* Example:
   let* (`OpHash op_hash) = Operation.inject op client in
   let* () = Client.bake_for_and_wait ~node client in
   let* op_gas = operation_gas_hash op_hash client in
*)
let operation_gas_hash operation_hash client =
  let* receipt =
    Operation_receipt.get_result_for ~check_previous:1 operation_hash client
  in
  assert (List.length receipt = 1) ;
  let operation_result = List.hd receipt in
  let milligas_consumed = get_consumed_milligas operation_result in
  let gas_consumed = Float.of_int milligas_consumed /. 1000. in
  let () = print_gas_consumed gas_consumed in
  return gas_consumed

(* Option 3. Uses the run_operation RPC *)
let operation_gas ~node operation client =
  let* op_json = Operation.make_run_operation_input operation client in
  (* Log.info "%s" (Ezjsonm.value_to_string ~minify:false op_json) ; *)
  let* output =
    Node.RPC.(
      call node (post_chain_block_helpers_scripts_run_operation (Data op_json)))
  in
  let operation_result =
    JSON.(output |-> "contents" |=> 0 |-> "metadata" |-> "operation_result")
  in
  let milligas_consumed = get_consumed_milligas operation_result in
  let gas_consumed = Float.of_int milligas_consumed /. 1000. in
  let () = print_gas_consumed gas_consumed in
  return gas_consumed

let operation_size operation client =
  let* size = Operation.byte_size operation client in
  Log.info ~color:Log.Color.FG.gray "Operation size in bytes is %d." size ;
  return size

let operation_kind operation client =
  let* op_json_u = Operation.make_run_operation_input operation client in
  let op_json = JSON.annotate ~origin:__LOC__ op_json_u in
  (*   Log.info ~color:Log.Color.FG.red "operation kind: %s" (JSON.encode op_json) ; *)
  let op_kind =
    JSON.(op_json |-> "operation" |-> "contents" |=> 0 |-> "kind" |> as_string)
  in
  return op_kind

let print_op_size_and_gas_in_file ~name ?op_size ~op_gas () =
  let op_size = Option.value ~default:0 op_size in
  let str = sf "%s, %d, %f" name op_size op_gas in
  Regression.capture ~eol:true str ;
  Log.info ~color:Log.Color.FG.red "%s" str

let operation_size_and_gas ?name ?gas_limit ?storage_limit ~node ~source
    operation_payload client =
  let module M = Operation.Manager in
  let op_manager =
    M.make ?gas_limit ?storage_limit ~source @@ operation_payload
  in
  let* op = M.operation [op_manager] client in
  let* op_kind = operation_kind op client in
  let* op_size = operation_size op client in
  let* op_gas = operation_gas ~node op client in
  let name =
    match name with Some name -> op_kind ^ "; " ^ name | None -> op_kind
  in
  print_op_size_and_gas_in_file ~name ~op_size ~op_gas () ;
  unit

let register ~protocols:_ = ()
