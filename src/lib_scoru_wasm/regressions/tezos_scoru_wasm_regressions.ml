(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Invocation: dune exec -- tezt/tests/main.exe --file tezos_scoru_wasm_regressions.ml *)

open Tezos_scoru_wasm_helpers
open Tezos_scoru_wasm_helpers.Wasm_utils

(* Helpers *)
module Context_binary = Tezos_context_memory.Context_binary

module Prover = struct
  open Tezos_protocol_alpha
  open Tezos_protocol_alpha.Protocol

  module Tree :
    Environment.Context.TREE
      with type t = Context_binary.t
       and type tree = Context_binary.tree
       and type key = string list
       and type value = bytes = struct
    type t = Context_binary.t

    type tree = Context_binary.tree

    type key = Context_binary.key

    type value = Context_binary.value

    include Context_binary.Tree
  end

  module WASM_P :
    Alpha_context.Sc_rollup.Wasm_2_0_0PVM.P
      with type Tree.t = Context_binary.t
       and type Tree.tree = Context_binary.tree
       and type Tree.key = string list
       and type Tree.value = bytes
       and type proof = Context_binary.Proof.tree Context_binary.Proof.t =
  struct
    open Alpha_context
    module Tree = Tree

    type tree = Tree.tree

    type proof = Context_binary.Proof.tree Context_binary.Proof.t

    let proof_encoding =
      Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V2.Tree2
      .tree_proof_encoding

    let kinded_hash_to_state_hash :
        Context_binary.Proof.kinded_hash -> Sc_rollup.State_hash.t = function
      | `Value hash | `Node hash ->
          Sc_rollup.State_hash.context_hash_to_state_hash hash

    let proof_before proof =
      kinded_hash_to_state_hash proof.Context_binary.Proof.before

    let proof_after proof =
      kinded_hash_to_state_hash proof.Context_binary.Proof.after

    let produce_proof context tree step =
      let open Lwt_syntax in
      let* context = Context_binary.add_tree context [] tree in
      let* (_hash : Context_hash.t) =
        Context_binary.commit ~time:Time.Protocol.epoch context
      in
      let index = Context_binary.index context in
      match Context_binary.Tree.kinded_key tree with
      | Some k ->
          let* p = Context_binary.produce_tree_proof index k step in
          return (Some p)
      | None ->
          Stdlib.failwith
            "produce_proof: internal error, [kinded_key] returned [None]"

    let verify_proof proof step =
      let open Lwt_syntax in
      let* result = Context_binary.verify_tree_proof proof step in
      match result with
      | Ok v -> return (Some v)
      | Error _ ->
          (* We skip the error analysis here since proof verification is not a
             job for the rollup node. *)
          return None
  end

  include
    Alpha_context.Sc_rollup.Wasm_2_0_0PVM.Make
      (Environment.Wasm_2_0_0.Make)
      (WASM_P)
end

module Verifier =
  Tezos_protocol_alpha.Protocol.Alpha_context.Sc_rollup.Wasm_2_0_0PVM
  .Protocol_implementation

let version_name = function Wasm_pvm_state.V0 -> "v0" | V1 -> "v1"

let capture_hash_of tree =
  Regression.capture @@ Context_hash.to_b58check
  @@ Encodings_util.Tree.hash tree

let rec eval_and_capture_many ?(fail_on_stuck = true) ~bunk
    ?(max_steps = Int64.max_int) tree =
  capture_hash_of tree ;
  let* info = Wasm_fast.get_info tree in
  match info.input_request with
  | No_input_required when max_steps > 0L ->
      let steps = Int64.min bunk max_steps in
      let* tree, steps = Wasm_fast.compute_step_many ~max_steps:steps tree in
      let max_steps = Int64.sub max_steps steps in
      eval_and_capture_many ~bunk ~max_steps tree
  | _ ->
      let* is_stuck = Wasm_fast.Internal_for_tests.is_stuck tree in
      if fail_on_stuck && Option.is_some is_stuck then
        Test.fail ~__LOC__ "WASM PVM is stuck" ;
      return tree

let echo_kernel =
  read_file
  @@ project_root // "src" // "proto_alpha" // "lib_protocol" // "test"
     // "integration" // "wasm_kernel" // "echo.wast"

let tx_no_verify_kernel =
  read_file
  @@ project_root // "src" // "lib_scoru_wasm" // "test" // "wasm_kernels"
     // "tx-kernel-no-verif.wasm"

let tx_no_verify_inputs =
  let base =
    project_root // "src" // "lib_scoru_wasm" // "test" // "messages"
  in
  [read_file (base // "deposit.out"); read_file (base // "withdrawal.out")]

let link_kernel import_name import_params import_results =
  Format.sprintf
    {|
(module
 (import "smart_rollup_core" "%s"
         (func $%s (param %s) (result %s)))
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_run")
    (nop)))
  |}
    import_name
    import_name
    (String.concat " " import_params)
    (String.concat " " import_results)

let check_proof_size ~current_tick ~proof_size_limit context input_opt s =
  let open Lwt_syntax in
  let* proof = Prover.produce_proof context input_opt s in
  match proof with
  | Error _ ->
      Test.fail "Could not compute proof for tick %a" Z.pp_print current_tick
  | Ok proof ->
      let bytes =
        Data_encoding.Binary.to_bytes_exn Prover.proof_encoding proof
      in
      let len = Bytes.length bytes in
      if proof_size_limit < Bytes.length bytes then
        Test.fail
          "Found a proof too large (%d bytes) at tick %a"
          len
          Z.pp_print
          current_tick ;
      Regression.capture Format.(asprintf "%a, %d" Z.pp_print current_tick len) ;
      unit

let checked_eval ~proof_size_limit context s =
  let open Lwt_syntax in
  let* info = Wasm_fast.get_info s in
  let* () =
    check_proof_size
      ~current_tick:info.current_tick
      ~proof_size_limit
      context
      None
      s
  in
  unit

let context ~name () =
  let open Lwt_syntax in
  let* index = Context_binary.init name in
  return (Context_binary.empty index)

let register_gen ~from_binary ~fail_on_stuck ?ticks_per_snapshot ~tag ~inputs
    ~skip_ticks ~ticks_to_check ~name ~versions k kernel =
  let eval context s =
    let rec eval checked_ticks s =
      let* info = Wasm_fast.get_info s in
      match info.input_request with
      | No_input_required when checked_ticks < ticks_to_check ->
          let* () = k context s in
          let* s = Wasm_fast.compute_step s in
          (eval [@tailcall]) Int64.(succ checked_ticks) s
      | No_input_required -> (skip [@tailcall]) s
      | _ -> return s
    and skip s =
      let* info = Wasm_fast.get_info s in
      match info.input_request with
      | No_input_required when 0L < skip_ticks ->
          let* s, _ = Wasm_fast.compute_step_many ~max_steps:skip_ticks s in
          (eval [@tailcall]) 0L s
      | No_input_required -> (eval [@tailcall]) 0L s
      | _ -> return s
    in
    eval 0L s
  in

  List.iter
    (fun version ->
      Regression.register
        ~__FILE__
        ~title:
          Format.(
            sprintf "kernel %s run (%s, %s)" name tag (version_name version))
        ~tags:["wasm_2_0_0"; name; tag; version_name version]
        (fun () ->
          let* context = context ~name () in
          let* tree =
            initial_tree ~from_binary ~version ?ticks_per_snapshot kernel
          in
          let* tree = set_full_input_step inputs 0l tree in
          let* tree = eval context tree in
          let* is_stuck = Wasm_fast.Internal_for_tests.is_stuck tree in
          if Option.is_some is_stuck && fail_on_stuck then
            Test.fail "Evaluation reached a Stuck state" ;
          unit))
    versions

let register ?(from_binary = false) ?(fail_on_stuck = true) ?ticks_per_snapshot
    ?(inputs = []) ?(proof_size_limit = 16 * 1024) ?hash_frequency
    ?proof_frequency ~name ~versions kernel =
  (match proof_frequency with
  | Some proof_frequency ->
      register_gen
        ~tag:"proof"
        ~from_binary
        ~fail_on_stuck
        ?ticks_per_snapshot
        ~inputs
        ~versions
        ~name
        ~skip_ticks:(snd proof_frequency)
        ~ticks_to_check:(fst proof_frequency)
        (fun context s -> checked_eval ~proof_size_limit context s)
        kernel
  | None -> ()) ;
  match hash_frequency with
  | Some hash_frequency ->
      register_gen
        ~tag:"hash"
        ~from_binary
        ~fail_on_stuck
        ?ticks_per_snapshot
        ~inputs
        ~versions
        ~name
        ~skip_ticks:hash_frequency
        ~ticks_to_check:1L
        (fun _ s ->
          capture_hash_of s ;
          unit)
        kernel
  | None -> ()

let register () =
  register
    ~name:"echo"
    ~from_binary:false
    ~ticks_per_snapshot:5_000L
    ~inputs:[]
    ~versions:[V0; V1]
    ~hash_frequency:137L
    ~proof_frequency:(11L, 23L)
    echo_kernel ;
  register
    ~name:"tx_no_verify"
    ~from_binary:true
    ~ticks_per_snapshot:6_000_000L
    ~inputs:tx_no_verify_inputs
    ~versions:[V0; V1]
    ~hash_frequency:10_037L
    ~proof_frequency:(3L, 30_893L)
    tx_no_verify_kernel ;
  register
    ~name:"link_store_create"
    ~fail_on_stuck:false
    ~from_binary:false
    ~ticks_per_snapshot:5_000L
    ~inputs:tx_no_verify_inputs
    ~versions:[V0; V1]
    ~hash_frequency:0L
    ~proof_frequency:(1L, 0L)
    (link_kernel "store_create" ["i32"; "i32"; "i32"] ["i32"]) ;
  register
    ~name:"link_store_delete_value"
    ~fail_on_stuck:false
    ~from_binary:false
    ~ticks_per_snapshot:5_000L
    ~inputs:tx_no_verify_inputs
    ~versions:[V0; V1]
    ~hash_frequency:0L
    ~proof_frequency:(1L, 0L)
    (link_kernel "store_delete_value" ["i32"; "i32"] ["i32"]) ;
  register
    ~name:"link_store_get_hash"
    ~fail_on_stuck:false
    ~from_binary:false
    ~ticks_per_snapshot:5_000L
    ~inputs:tx_no_verify_inputs
    ~versions:[V0; V1]
    ~hash_frequency:0L
    ~proof_frequency:(1L, 0L)
    (link_kernel
       "__internal_store_get_hash"
       ["i32"; "i32"; "i32"; "i32"]
       ["i32"])
