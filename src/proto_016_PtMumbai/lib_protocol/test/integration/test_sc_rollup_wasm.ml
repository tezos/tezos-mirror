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
    Component:    sc rollup wasm
    Invocation:   dune exec \
                  src/proto_alpha/lib_protocol/test/integration/main.exe \
                  -- test "^sc rollup wasm$"
    Subject:      Test the WASM 2.0 PVM.
*)

open Protocol
open Alpha_context
module Context_binary = Tezos_context_memory.Context_binary

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
  Protocol.Alpha_context.Sc_rollup.Wasm_2_0_0PVM.P
    with type Tree.t = Context_binary.t
     and type Tree.tree = Context_binary.tree
     and type Tree.key = string list
     and type Tree.value = bytes
     and type proof = Context_binary.Proof.tree Context_binary.Proof.t = struct
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

module Verifier = Alpha_context.Sc_rollup.Wasm_2_0_0PVM.Protocol_implementation

module Prover =
  Alpha_context.Sc_rollup.Wasm_2_0_0PVM.Make
    (Environment.Wasm_2_0_0.Make)
    (WASM_P)
(* Helpers *)

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/2198
   SCORU system should expose a constant upper bound for proof size.
   One suggestion for this constant is 16KB. Unfortunately, the proof
   that are generated in “realistic” scenarios are still too big.

   If #2198 is addressed, and a constant is added to the protocol to
   limit the maximum size of a valid SCORU proof, then the value here
   should reflect that. *)
let proof_size_limit = 16 * 1024

let check_proof_size ~loc context input_opt s =
  let open Lwt_result_syntax in
  let*! proof = Prover.produce_proof context input_opt s in
  match proof with
  | Error _ -> Stdlib.failwith "missing proof"
  | Ok proof ->
      let bytes =
        Data_encoding.Binary.to_bytes_exn Prover.proof_encoding proof
      in
      Assert.leq_int ~loc (Bytes.length bytes) proof_size_limit

(* Like [eval] but also checks the proof size. *)
let checked_eval ~loc context s =
  let open Lwt_result_syntax in
  let* () = check_proof_size ~loc context None s in
  let*! s = Prover.eval s in
  return s

(* Like [set_input] but also checks the proof size. *)
let checked_set_input ~loc context input s =
  let open Lwt_result_syntax in
  let* () = check_proof_size ~loc context (Some input) s in
  let*! s = Prover.set_input input s in
  return s

let find tree key encoding =
  let open Lwt.Syntax in
  Format.printf "f %s\n" (String.concat "/" key) ;
  let+ value = Context_binary.Tree.find tree key in
  match value with
  | Some bytes ->
      Format.printf "v %S\n" (Bytes.to_string bytes) ;
      Some (Data_encoding.Binary.of_bytes_exn encoding bytes)
  | None -> None

let operator = Account.new_account

let arbitrary_input i payload =
  match Sc_rollup.Inbox_message.serialize (External payload) with
  | Ok payload ->
      Sc_rollup.Inbox_message
        {
          inbox_level = Raw_level.of_int32_exn 0l;
          message_counter = Z.of_int i;
          payload;
        }
  | Error err ->
      Format.printf "%a@," Environment.Error_monad.pp_trace err ;
      assert false

(* Read the chosen `wasm_kernel` into memory. *)
let read_kernel name =
  let open Tezt.Base in
  let kernel_file =
    project_root // Filename.dirname __FILE__ // "wasm_kernel"
    // (name ^ ".wasm")
  in
  read_file kernel_file

(* Kernel with allocation & simple computation only.
   9863 bytes long - will be split into 3 chunks. *)
let computation_kernel () = read_kernel "computation"

let rec eval_until_set_input context s =
  let open Lwt_result_syntax in
  let*! info = Prover.get_status s in
  match info with
  | Computing ->
      let* s = checked_eval ~loc:__LOC__ context s in
      eval_until_set_input context s
  | Waiting_for_reveal _ | Waiting_for_input_message -> return s

let should_boot_computation_kernel () =
  let open Lwt_result_syntax in
  let boot_sector = computation_kernel () in
  let*! index = Context_binary.init "/tmp" in
  let context = Context_binary.empty index in
  let empty = Context_binary.Tree.empty context in
  let*! s = Prover.initial_state ~empty in
  (* sets a reasonable nb-of-tick limit to limit test running time *)
  let*! s =
    Tree.add
      s
      ["pvm"; "max_nb_ticks"]
      (Data_encoding.Binary.to_bytes_exn Data_encoding.n (Z.of_int 50_000))
  in
  let*! s = Prover.install_boot_sector s boot_sector in
  (* Feeding it with one input *)
  let* s =
    checked_set_input ~loc:__LOC__ context (arbitrary_input 0 "test") s
  in
  (* running until waiting for input *)
  let* (_s : Prover.state) = eval_until_set_input context s in
  return_unit

let tests =
  [
    Tztest.tztest
      "should boot a valid kernel until reading inputs"
      `Quick
      should_boot_computation_kernel;
  ]
