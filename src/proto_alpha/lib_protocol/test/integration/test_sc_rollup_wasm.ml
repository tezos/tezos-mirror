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
    let _hash = Context_binary.commit ~time:Time.Protocol.epoch context in
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

module Prover = Alpha_context.Sc_rollup.Wasm_2_0_0PVM.Make (WASM_P)
(* Helpers *)

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/2198
   SCORU system should expose a constant upper bound for proof size.
   One suggestion for this constant is 16KB. Unfortunately, the proof
   that are generated in “realistic” scenarios are still too big.

   If #2198 is addressed, and a constant is added to the protocol to
   limit the maximum size of a valid SCORU proof, then the value here
   should reflect that.

   The size set here is the minimum size that makes the current tests
   pass. It should be updated to [16 * 1024] once the small ticks
   milestone is completed.
*)
let proof_size_limit = 21_550

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

let complete_boot_sector sector :
    Tezos_scoru_wasm.Gather_floppies.origination_message =
  Complete_kernel sector

let incomplete_boot_sector sector Account.{pk; _} :
    Tezos_scoru_wasm.Gather_floppies.origination_message =
  Incomplete_kernel (Bytes.of_string sector, pk)

let find tree key encoding =
  let open Lwt.Syntax in
  Format.printf "f %s\n" (String.concat "/" key) ;
  let+ value = Context_binary.Tree.find tree key in
  match value with
  | Some bytes ->
      Format.printf "v %S\n" (Bytes.to_string bytes) ;
      Some (Data_encoding.Binary.of_bytes_exn encoding bytes)
  | None -> None

let find_status tree =
  find
    tree
    ["gather-floppies"; "status"]
    Tezos_scoru_wasm.Gather_floppies.internal_status_encoding

let get_chunks_count tree =
  let open Lwt.Syntax in
  let+ len =
    find
      tree
      ["durable"; "kernel"; "boot.wasm"; "_"; "length"]
      Data_encoding.int64
  in
  Option.fold ~none:0 ~some:Int64.to_int len

let check_status tree expected =
  let open Lwt.Syntax in
  let* status = find_status tree in
  match (status, expected) with
  | Some status, Some expected ->
      assert (status = expected) ;
      Lwt.return ()
  | None, None -> Lwt.return ()
  | _, _ -> assert false

let check_chunks_count tree expected =
  let open Lwt.Syntax in
  let* count = get_chunks_count tree in
  if count = expected then Lwt_result.return ()
  else failwith "wrong chunks counter, expected %d, got %d" expected count

let operator () =
  match Account.generate_accounts 1 with
  | [(account, _, _)] -> account
  | _ -> assert false

let should_boot_complete_boot_sector boot_sector () =
  let open Tezos_scoru_wasm.Gather_floppies in
  let open Lwt_result_syntax in
  let*! index = Context_binary.init "/tmp" in
  let context = Context_binary.empty index in
  (* The number of chunks necessary to store the kernel. *)
  let boot_sector_len =
    match boot_sector with
    | Complete_kernel bytes | Incomplete_kernel (bytes, _) -> Bytes.length bytes
  in
  let boot_sector =
    Data_encoding.Binary.to_string_exn origination_message_encoding boot_sector
  in
  (* We create a new PVM, and install the boot sector. *)
  let*! s = Prover.initial_state context in
  let*! s = Prover.install_boot_sector s boot_sector in
  (* After this first step, the PVM has just loaded the boot sector in
     "/boot-sector", and nothing more.  As a consequence, most of the
     step of the [Gather_floppies] instrumentation is not set. *)
  let*! () = check_status s None in
  let* () = check_chunks_count s 0 in
  (* At this step, the [eval] function of the PVM will interpret the
     origination message encoded in [boot_sector]. *)
  let* s = checked_eval ~loc:__LOC__ context s in
  (* We expect that the WASM does not expect more floppies, and that
     the kernel as been correctly splitted into several chunks. *)
  let*! () = check_status s (Some Not_gathering_floppies) in
  let* () = check_chunks_count s boot_sector_len in
  return_unit

let arbitrary_input i payload =
  match Sc_rollup.Inbox_message.serialize (External payload) with
  | Ok payload ->
      Sc_rollup.
        {
          inbox_level = Raw_level.of_int32_exn 0l;
          message_counter = Z.of_int i;
          payload;
        }
  | Error err ->
      Format.printf "%a@," Environment.Error_monad.pp_trace err ;
      assert false

let floppy_input i operator chunk =
  let signature = Signature.sign operator.Account.sk chunk in
  let floppy = Tezos_scoru_wasm.Gather_floppies.{chunk; signature} in
  arbitrary_input
    i
    (Data_encoding.Binary.to_string_exn
       Tezos_scoru_wasm.Gather_floppies.floppy_encoding
       floppy)

let should_interpret_empty_chunk () =
  let open Lwt_result_syntax in
  let op = operator () in
  let chunk_size = Tezos_scoru_wasm.Gather_floppies.chunk_size in
  let origination_message =
    Data_encoding.Binary.to_string_exn
      Tezos_scoru_wasm__Gather_floppies.origination_message_encoding
    @@ incomplete_boot_sector (String.make chunk_size 'a') op
  in
  let chunk = Bytes.empty in
  let correct_input = floppy_input 0 op chunk in

  (* Init the PVM *)
  let*! index = Context_binary.init "/tmp" in
  let context = Context_binary.empty index in
  let*! s = Prover.initial_state context in
  let*! s = Prover.install_boot_sector s origination_message in
  (* Intererptation of the origination message *)
  let* s = checked_eval ~loc:__LOC__ context s in
  let*! () = check_status s (Some (Gathering_floppies op.pk)) in
  let* () = check_chunks_count s chunk_size in
  (* Try to interpret the empty input (correctly signed) *)
  let*! s = Prover.set_input correct_input s in
  let*! () = check_status s (Some Not_gathering_floppies) in
  (* We still have 1 chunk. *)
  let* () = check_chunks_count s chunk_size in
  return_unit

let should_refuse_chunks_with_incorrect_signature () =
  let open Lwt_result_syntax in
  let good_op = operator () in
  let bad_op = operator () in
  let chunk_size = Tezos_scoru_wasm.Gather_floppies.chunk_size in
  let origination_message =
    Data_encoding.Binary.to_string_exn
      Tezos_scoru_wasm__Gather_floppies.origination_message_encoding
    @@ incomplete_boot_sector (String.make chunk_size 'a') good_op
  in
  let chunk = Bytes.make chunk_size 'b' in
  let incorrect_input = floppy_input 0 bad_op chunk in
  let correct_input = floppy_input 0 good_op chunk in

  (* Init the PVM *)
  let*! index = Context_binary.init "/tmp" in
  let context = Context_binary.empty index in
  let*! s = Prover.initial_state context in
  let*! s = Prover.install_boot_sector s origination_message in
  (* Intererptation of the origination message *)
  let* s = checked_eval ~loc:__LOC__ context s in
  let*! () = check_status s (Some (Gathering_floppies good_op.pk)) in
  let* () = check_chunks_count s chunk_size in
  (* Try to interpret the incorrect input (badly signed) *)
  let* s = checked_set_input ~loc:__LOC__ context incorrect_input s in
  let*! () = check_status s (Some (Gathering_floppies good_op.pk)) in
  (* We still have 1 chunk. *)
  let* () = check_chunks_count s chunk_size in
  (* Try to interpret the correct input (correctly signed) *)
  let* s = checked_set_input ~loc:__LOC__ context correct_input s in
  let*! () = check_status s (Some (Gathering_floppies good_op.pk)) in
  (* We now have 2 chunks. *)
  let* () = check_chunks_count s (2 * chunk_size) in
  return_unit

let should_boot_incomplete_boot_sector kernel () =
  let open Lwt_result_syntax in
  let operator = operator () in
  let chunk_size = Tezos_scoru_wasm.Gather_floppies.chunk_size in
  let initial_chunk, rem_chunks =
    let split_chunk s =
      let len = String.length s in
      let size = min len chunk_size in
      let chunk = String.sub s 0 size in
      let rest =
        if len > chunk_size then Some (String.sub s size @@ (len - size))
        else None
      in
      (chunk, rest)
    in
    let rec do_chunks chunks left =
      match left with
      | None -> chunks
      | Some left ->
          let chunk, rest = split_chunk left in
          (do_chunks [@tailcall]) (chunk :: chunks) rest
    in
    let initial, rest = split_chunk kernel in
    (initial, List.rev @@ do_chunks [] rest)
  in
  let initial_chunk =
    Data_encoding.Binary.to_string_exn
      Tezos_scoru_wasm__Gather_floppies.origination_message_encoding
    @@ incomplete_boot_sector initial_chunk operator
  in
  let chunks =
    rem_chunks
    |> List.take_n (List.length rem_chunks - 1)
    |> List.map Bytes.of_string
  in
  let final_chunk = Bytes.of_string @@ List.last "" rem_chunks in
  let final_chunk_size = Bytes.length final_chunk in

  let*! index = Context_binary.init "/tmp" in
  let context = Context_binary.empty index in
  let*! s = Prover.initial_state context in
  let*! s = Prover.install_boot_sector s initial_chunk in
  let* () = check_proof_size ~loc:__LOC__ context None s in
  let*! () = check_status s None in
  let* () = check_chunks_count s 0 in
  (* First tick, to interpret the boot sector. One chunk have been
     provided, and the PVM expects more chunk to come. *)
  let* s = checked_eval ~loc:__LOC__ context s in
  let*! () = check_status s (Some (Gathering_floppies operator.pk)) in
  let* () = check_chunks_count s chunk_size in
  (* Then, installing the additional chunks. *)
  let* s =
    List.fold_left_i_es
      (fun i s chunk ->
        (* We are installing the [i+2]th chunk ([i] starts at 0, and
           the first chunk is not part of the list). *)
        let input = floppy_input i operator chunk in
        let* s = checked_set_input ~loc:__LOC__ context input s in
        (* We have [i+2] chunks. *)
        let* () = check_chunks_count s ((i + 2) * chunk_size) in
        return s)
      s
      chunks
  in
  (* Up until the very last one, where the status of the PVM change. *)
  let len = List.length chunks in
  let input = floppy_input len operator final_chunk in
  let* s = checked_set_input ~loc:__LOC__ context input s in
  let*! () = check_status s (Some Not_gathering_floppies) in
  let* () =
    check_chunks_count s (((len + 1) * chunk_size) + final_chunk_size)
  in
  return_unit

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
  | Waiting_for_input_message -> return s

let should_boot_computation_kernel () =
  let open Lwt_result_syntax in
  let boot_sector =
    Data_encoding.Binary.to_string_exn
      Tezos_scoru_wasm.Gather_floppies.origination_message_encoding
      (complete_boot_sector (String.to_bytes (computation_kernel ())))
  in
  let*! index = Context_binary.init "/tmp" in
  let context = Context_binary.empty index in
  let*! s = Prover.initial_state context in
  let*! s = Prover.install_boot_sector s boot_sector in
  (* installing the boot kernel *)
  let* s = checked_eval ~loc:__LOC__ context s in
  (* Make the first ticks of the WASM PVM (parsing of origination
     message, parsing and init of the kernel), to switch it to
     “waiting for input” mode. *)
  let* s = eval_until_set_input context s in
  (* Feeding it with one input *)
  let* s =
    checked_set_input ~loc:__LOC__ context (arbitrary_input 0 "test") s
  in
  (* running until waiting for input *)
  let* _s = eval_until_set_input context s in
  return_unit

let tests =
  [
    Tztest.tztest "should boot a complete boot sector" `Quick
    @@ should_boot_complete_boot_sector
         (complete_boot_sector (Bytes.of_string @@ computation_kernel ()));
    ( Tztest.tztest "should boot an incomplete but too small boot sector" `Quick
    @@ fun () ->
      let operator = operator () in
      should_boot_complete_boot_sector
        (incomplete_boot_sector "\x00asm\x01\x00\x00\x00" operator)
        () );
    Tztest.tztest
      "should boot an incomplete boot sector with floppies"
      `Quick
      (should_boot_incomplete_boot_sector @@ computation_kernel ());
    Tztest.tztest
      "should interpret an empty chunk as EOF"
      `Quick
      should_interpret_empty_chunk;
    Tztest.tztest
      "should refuse chunks with an incorrect signature"
      `Quick
      should_refuse_chunks_with_incorrect_signature;
    Tztest.tztest
      "should boot a valid kernel until reading inputs"
      `Quick
      should_boot_computation_kernel;
  ]
