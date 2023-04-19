(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Testing
    -------
    Component:    Rollup layer 1 logic
    Invocation:   dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                  -- --file test_sc_rollup_wasm.ml
    Subject:      Unit test for the Wasm PVM
*)

open Protocol
open Tezos_micheline.Micheline
open Michelson_v1_primitives
open Tezos_webassembly_interpreter
module Context = Tezos_context_memory.Context_binary
open Wasm_utils

module Proof_encoding =
  Tezos_context_merkle_proof_encoding.Merkle_proof_encoding

module Wasm_context = struct
  module Tree = struct
    include Context.Tree

    type tree = Context.tree

    type t = Context.t

    type key = string list

    type value = bytes
  end

  type tree = Context.tree

  type proof = Context.Proof.tree Context.Proof.t

  let verify_proof p f =
    Lwt.map Result.to_option (Context.verify_tree_proof p f)

  let produce_proof context tree step =
    let open Lwt_syntax in
    let* context = Context.add_tree context [] tree in
    let* (_hash : Context_hash.t) =
      Context.commit ~time:Time.Protocol.epoch context
    in
    let index = Context.index context in
    match Context.Tree.kinded_key tree with
    | Some k ->
        let* p = Context.produce_tree_proof index k step in
        return (Some p)
    | None -> return None

  let kinded_hash_to_state_hash = function
    | `Value hash | `Node hash ->
        Sc_rollup_repr.State_hash.context_hash_to_state_hash hash

  let proof_before proof = kinded_hash_to_state_hash proof.Context.Proof.before

  let proof_after proof = kinded_hash_to_state_hash proof.Context.Proof.after

  let proof_encoding = Proof_encoding.V2.Tree2.tree_proof_encoding
end

module Full_Wasm =
  Sc_rollup_wasm.V2_0_0.Make (Environment.Wasm_2_0_0.Make) (Wasm_context)

let test_initial_state_hash_wasm_pvm () =
  let open Alpha_context in
  let open Lwt_result_syntax in
  let empty = Sc_rollup_helpers.make_empty_tree () in
  let*! state = Sc_rollup_helpers.Wasm_pvm.initial_state ~empty in
  let*! hash = Sc_rollup_helpers.Wasm_pvm.state_hash state in
  let expected = Sc_rollup.Wasm_2_0_0PVM.reference_initial_state_hash in
  if Sc_rollup.State_hash.(hash = expected) then return_unit
  else
    failwith
      "incorrect hash, expected %a, got %a"
      Sc_rollup.State_hash.pp
      expected
      Sc_rollup.State_hash.pp
      hash

let test_metadata_size () =
  let address = Sc_rollup_repr.Address.of_bytes_exn (Bytes.make 20 '\000') in
  let metadata =
    Sc_rollup_metadata_repr.{address; origination_level = Raw_level_repr.root}
  in
  let bytes =
    Data_encoding.Binary.to_bytes_exn Sc_rollup_metadata_repr.encoding metadata
  in
  assert (
    Bytes.length bytes
    = Tezos_scoru_wasm.Host_funcs.Internal_for_tests.metadata_size) ;
  Lwt_result_syntax.return_unit

let test_l1_input_kind () =
  let open Lwt_result_syntax in
  let open Sc_rollup_inbox_message_repr in
  let open Tezos_scoru_wasm in
  let check_msg msg expected =
    let*? msg = Environment.wrap_tzresult @@ serialize msg in
    let msg = unsafe_to_string msg |> Pvm_input_kind.from_raw_input in
    assert (msg = expected) ;
    return_unit
  in
  let* () = check_msg (Internal Start_of_level) (Internal Start_of_level) in
  let* () = check_msg (Internal End_of_level) (Internal End_of_level) in
  let* () = check_msg (External "payload") External in

  return_unit

let make_transaction value text contract =
  let entrypoint = Entrypoint_repr.default in
  let destination : Contract_hash.t =
    Contract_hash.of_bytes_exn @@ Bytes.of_string contract
  in
  let unparsed_parameters =
    strip_locations
    @@ Prim
         ( 0,
           I_TICKET,
           [Prim (0, I_PAIR, [Int (0, Z.of_int32 value); String (1, text)], [])],
           [] )
  in
  Sc_rollup_outbox_message_repr.{unparsed_parameters; entrypoint; destination}

let make_transactions () =
  let l =
    QCheck2.Gen.(
      generate1
      @@ list_size
           (return 3)
           (triple (string_size @@ return 20) int32 (small_string ~gen:char)))
  in
  List.map (fun (contract, i, s) -> make_transaction i s contract) l

(* This is simple "echo kernel" it spits out the first four inputs (SOL,
   Info_per_level, input, EOL) it receives. It uses the [write_output] host
   function and so it is used to test this function. *)
let test_output () =
  let open Lwt_result_syntax in
  let level_offset = 20 in
  let dst = 60 in
  let max_bytes = 3600 in
  let dst_without_header = dst + 2 in
  let modul =
    Format.sprintf
      {|
        (module
          (type $t0 (func (param i32 i32) (result i32)))
          (type $t3 (func (param i32 i32 i32) (result i32)))
          (import "smart_rollup_core" "read_input" (func $read_input (type $t3)))
          (import "smart_rollup_core" "write_output" (func $write_output (type $t0)))
          (memory 1)
          (export "memory" (memory 0))
          (func (export "kernel_run")
            (local $size i32)
            (local.set $size (call $read_input
                                   (i32.const %d)
                                   (i32.const %d)
                                   (i32.const %d)))
            (call $write_output (i32.const %d)
                                (i32.sub (local.get $size) (i32.const 2)))
            (local.set $size (call $read_input
                                   (i32.const %d)
                                   (i32.const %d)
                                   (i32.const %d)))
            (call $write_output (i32.const %d)
                                (i32.sub (local.get $size) (i32.const 2)))
            (local.set $size (call $read_input
                                   (i32.const %d)
                                   (i32.const %d)
                                   (i32.const %d)))
            (call $write_output (i32.const %d)
                                (i32.sub (local.get $size) (i32.const 2)))
            (local.set $size (call $read_input
                                   (i32.const %d)
                                   (i32.const %d)
                                   (i32.const %d)))
            (call $write_output (i32.const %d)
                                (local.get $size))
            drop)
          )

    |}
      level_offset
      dst
      max_bytes
      dst_without_header
      level_offset
      dst
      max_bytes
      dst_without_header
      level_offset
      dst
      max_bytes
      dst_without_header
      level_offset
      dst
      max_bytes
      dst_without_header
  in

  let*! dummy = Context.init "/tmp" in
  let dummy_context = Context.empty dummy in
  let (empty_tree : Wasm.tree) = Context.Tree.empty dummy_context in
  let parsed = Parse.string_to_module modul in
  let parsed =
    match parsed.it with Script.Textual m -> m | _ -> assert false
  in
  let*! boot_sector = Encode.encode parsed in
  let*! tree =
    Wasm.initial_state Sc_rollup_wasm.V2_0_0.current_version empty_tree
  in
  let*! tree =
    Wasm.install_boot_sector
      ~ticks_per_snapshot:Sc_rollup_wasm.V2_0_0.ticks_per_snapshot
      ~outbox_validity_period:Sc_rollup_wasm.V2_0_0.outbox_validity_period
      ~outbox_message_limit:Sc_rollup_wasm.V2_0_0.outbox_message_limit
      boot_sector
      tree
  in
  let*! tree =
    Wasm.Internal_for_tests.set_max_nb_ticks (Z.of_int64 50_000_000L) tree
  in
  let transactions = make_transactions () in
  let out =
    Sc_rollup_outbox_message_repr.(Atomic_transaction_batch {transactions})
  in
  let string_input_message =
    Data_encoding.Binary.to_string_exn
      Sc_rollup_outbox_message_repr.encoding
      out
  in
  let*! tree = eval_until_input_requested tree in
  let*! tree = set_full_input_step [string_input_message] 0l tree in
  let*! final_tree = eval_until_input_requested tree in
  let*! output = Wasm.Internal_for_tests.get_output_buffer final_tree in
  let* last_outbox_level =
    match output.Tezos_webassembly_interpreter.Output_buffer.last_level with
    | Some level -> return level
    | None -> failwith "The PVM output buffer does not contain any outbox."
  in
  let*! last_outbox =
    Tezos_webassembly_interpreter.Output_buffer.Internal_for_tests.get_outbox
      output
      last_outbox_level
  in
  let* end_of_level_message_index =
    match
      Output_buffer.Internal_for_tests.get_outbox_last_message_index last_outbox
    with
    | Some index -> return index
    | None -> failwith "The PVM output buffer does not contain any outbox."
  in
  (* The last message in the outbox corresponds to EOL, due to the nature of the
     kernel. As such we must take the one preceding it. *)
  let message_index = Z.pred end_of_level_message_index in

  let*! bytes_output_message =
    Tezos_webassembly_interpreter.Output_buffer.(
      get_message output {outbox_level = last_outbox_level; message_index})
  in
  assert (string_input_message = Bytes.to_string bytes_output_message) ;
  let message =
    Data_encoding.Binary.of_bytes_exn
      Sc_rollup_outbox_message_repr.encoding
      bytes_output_message
  in
  assert (message = out) ;
  let*? outbox_level =
    Environment.wrap_tzresult @@ Raw_level_repr.of_int32 last_outbox_level
  in
  let output = Sc_rollup_PVM_sig.{outbox_level; message_index; message} in

  let*! pf = Full_Wasm.produce_output_proof dummy_context final_tree output in

  match pf with
  | Ok proof ->
      let*! valid = Full_Wasm.verify_output_proof proof in
      fail_unless valid (Exn (Failure "An output proof is not valid."))
  | Error _ -> failwith "Error during proof generation"

(* When snapshoting a new protocol, to fix this test, the following
   action should be done.

     - In [src/lib_scoru_wasm/constants.ml], add a new variable before
       [proto_alpha_name] using the name of the new protocol, and whose
       value is [Raw_context.version_value].
     - Update [src/lib_scoru_wasm/pvm_input_kind.ml] to add a new case
       to the type [protocol], and update the functions
       [protocol_from_raw] and [Internal_for_tests.proto_to_binary]
       accordingly (by copy/pasting the [Proto_alpha] case and doing
       the necessary renaming.
     - Update [src/lib_scoru_wasm/wasm_vm.ml], more precisely the
       [version_for_protocol] function, to take into account the new
       protocol. The expected result is the same as for
       [Proto_alpha]. *)
let test_protocol_names () =
  let open Alpha_context.Sc_rollup.Inbox_message in
  let protocol_migration_message_str =
    Data_encoding.Binary.to_string_exn
      encoding
      (Internal protocol_migration_internal_message)
  in
  let kind =
    Tezos_scoru_wasm.Pvm_input_kind.from_raw_input
      protocol_migration_message_str
  in
  assert (kind = Internal (Protocol_migration Proto_alpha)) ;
  assert (
    protocol_migration_internal_message
    = Protocol_migration Tezos_scoru_wasm.Constants.proto_alpha_name) ;
  Lwt_result_syntax.return_unit

let tests =
  [
    Tztest.tztest
      "initial state hash for Wasm"
      `Quick
      test_initial_state_hash_wasm_pvm;
    Tztest.tztest "size of a rollup metadata" `Quick test_metadata_size;
    Tztest.tztest "l1 input kind" `Quick test_l1_input_kind;
    Tztest.tztest "output proofs" `Quick test_output;
    Tztest.tztest "protocol names consistency" `Quick test_protocol_names;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ Protocol.name [("sc rollup wasm", tests)]
  |> Lwt_main.run
