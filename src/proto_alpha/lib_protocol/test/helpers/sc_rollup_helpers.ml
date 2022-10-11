(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let originated_rollup op =
  let nonce =
    Origination_nonce.Internal_for_tests.initial (Operation.hash_packed op)
  in
  Contract.Internal_for_tests.originated_contract nonce

module In_memory_context = struct
  open Tezos_context_memory

  module Tree = struct
    include Context.Tree

    type tree = Context.tree

    type t = Context.t

    type key = string list

    type value = bytes
  end

  type tree = Tree.tree

  type proof = Context.Proof.tree Context.Proof.t

  let hash_tree _ = assert false

  let verify_proof p f =
    Lwt.map Result.to_option (Context.verify_tree_proof p f)

  let produce_proof context state step =
    let open Lwt_syntax in
    let* context = Context.add_tree context [] state in
    let* h = Context.commit ~time:Time.Protocol.epoch context in
    let index = Context.index context in
    let* context = Context.checkout_exn index h in
    match Tree.kinded_key state with
    | Some k ->
        let index = Context.index context in
        let* p = Context.produce_tree_proof index k step in
        return (Some p)
    | None -> return None

  let kinded_hash_to_state_hash = function
    | `Value hash | `Node hash ->
        Sc_rollup.State_hash.context_hash_to_state_hash hash

  let proof_before proof = kinded_hash_to_state_hash proof.Context.Proof.before

  let proof_after proof = kinded_hash_to_state_hash proof.Context.Proof.after

  let proof_encoding =
    Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V2.Tree32
    .tree_proof_encoding
end

module Arith_pvm :
  Sc_rollup.PVM.S
    with type context = In_memory_context.Tree.t
     and type state = In_memory_context.tree
     and type proof =
      Tezos_context_memory.Context.Proof.tree
      Tezos_context_memory.Context.Proof.t =
  Sc_rollup.ArithPVM.Make (In_memory_context)

module Wasm_pvm :
  Sc_rollup.PVM.S
    with type context = In_memory_context.Tree.t
     and type state = In_memory_context.tree
     and type proof =
      Tezos_context_memory.Context.Proof.tree
      Tezos_context_memory.Context.Proof.t =
  Sc_rollup.Wasm_2_0_0PVM.Make (Environment.Wasm_2_0_0.Make) (In_memory_context)

let origination_proof ~boot_sector = function
  | Sc_rollup.Kind.Example_arith ->
      let open Lwt_syntax in
      let context = Tezos_context_memory.make_empty_context () in
      let* proof = Arith_pvm.produce_origination_proof context boot_sector in
      let proof = WithExceptions.Result.get_ok ~loc:__LOC__ proof in
      return
        (Sc_rollup.Arith_pvm_with_proof
           (module struct
             include Arith_pvm

             let proof = proof
           end))
  | Sc_rollup.Kind.Wasm_2_0_0 ->
      let open Lwt_syntax in
      let context = Tezos_context_memory.make_empty_context () in
      let* proof = Wasm_pvm.produce_origination_proof context boot_sector in
      let proof = WithExceptions.Result.get_ok ~loc:__LOC__ proof in
      return
        (Sc_rollup.Wasm_2_0_0_pvm_with_proof
           (module struct
             include Wasm_pvm

             let proof = proof
           end))

let wrap_origination_proof ~kind ~boot_sector proof_string_opt :
    Sc_rollup.wrapped_proof tzresult Lwt.t =
  let open Lwt_result_syntax in
  match proof_string_opt with
  | None ->
      let*! origination_proof = origination_proof ~boot_sector kind in
      return origination_proof
  | Some proof_string ->
      Lwt.map Environment.wrap_tzresult
      @@ Sc_rollup_operations.Internal_for_tests.origination_proof_of_string
           proof_string
           kind

let genesis_commitment ~boot_sector ~origination_level = function
  | Sc_rollup.Kind.Example_arith ->
      let open Lwt_syntax in
      let context = Tezos_context_memory.make_empty_context () in
      let* proof = Arith_pvm.produce_origination_proof context boot_sector in
      let proof = WithExceptions.Result.get_ok ~loc:__LOC__ proof in
      let genesis_state_hash = Arith_pvm.proof_stop_state proof in
      return
        Sc_rollup.Commitment.(
          genesis_commitment ~origination_level ~genesis_state_hash)
  | Sc_rollup.Kind.Wasm_2_0_0 ->
      let open Lwt_syntax in
      let context = Tezos_context_memory.make_empty_context () in
      let* proof = Wasm_pvm.produce_origination_proof context boot_sector in
      let proof = WithExceptions.Result.get_ok ~loc:__LOC__ proof in
      let genesis_state_hash = Wasm_pvm.proof_stop_state proof in
      return
        Sc_rollup.Commitment.(
          genesis_commitment ~origination_level ~genesis_state_hash)

let genesis_commitment_raw ~boot_sector ~origination_level kind =
  let open Lwt_syntax in
  let origination_level =
    Raw_level_repr.to_int32 origination_level
    |> Alpha_context.Raw_level.of_int32_exn
  in
  let kind =
    match kind with
    | Sc_rollups.Kind.Example_arith -> Sc_rollup.Kind.Example_arith
    | Sc_rollups.Kind.Wasm_2_0_0 -> Sc_rollup.Kind.Wasm_2_0_0
  in
  let* res = genesis_commitment ~boot_sector ~origination_level kind in
  let res =
    Data_encoding.Binary.to_bytes_exn Sc_rollup.Commitment.encoding res
    |> Data_encoding.Binary.of_bytes_exn Sc_rollup_commitment_repr.encoding
  in
  return res

(** {2. Inbox message helpers.} *)

(** {1. Above [Alpha_context].} *)

let message_serialize msg =
  WithExceptions.Result.get_ok
    ~loc:__LOC__
    Sc_rollup.Inbox_message.(serialize msg)

let make_external_inbox_message str = message_serialize (External str)

let make_internal_inbox_message internal_msg =
  message_serialize (Internal internal_msg)

let make_input ?(inbox_level = Raw_level.root) ?(message_counter = Z.zero)
    payload =
  Sc_rollup.Inbox_message {inbox_level; message_counter; payload}

let make_external_input ?inbox_level ?message_counter str =
  let payload = make_external_inbox_message str in
  make_input ?inbox_level ?message_counter payload

let make_sol ~inbox_level =
  let payload = make_internal_inbox_message Start_of_level in
  make_input ~inbox_level ~message_counter:Z.zero payload

let make_eol ~inbox_level ~message_counter =
  let payload = make_internal_inbox_message End_of_level in
  make_input ~inbox_level ~message_counter payload

(** Message is the combination of a [message] and its associated [input].

    [message] is used to:
    - Construct the protocol inbox, when [message] is [`Message]. The protocol
      adds [`SOL] and [`EOL] itself.
    - Construct the players' inboxes.

    [input] is used to evaluate the players' inboxes.

*)
type message = {
  input : Sc_rollup.input;
  message : [`SOL | `Message of string | `EOL];
}

let pp_input fmt (input : Sc_rollup.input) =
  match input with
  | Reveal _ -> assert false
  | Inbox_message {inbox_level; message_counter; _} ->
      Format.fprintf
        fmt
        "(%a, %s)"
        Raw_level.pp
        inbox_level
        (Z.to_string message_counter)

let pp_message fmt {input; message} =
  Format.fprintf
    fmt
    "{ input = %a; message = %S }"
    pp_input
    input
    (match message with `SOL -> "SOL" | `Message msg -> msg | `EOL -> "EOL")

(** An empty inbox level is a SOL and EOL. *)
let make_empty_level inbox_level =
  let sol = {input = make_sol ~inbox_level; message = `SOL} in
  let eol =
    {input = make_eol ~inbox_level ~message_counter:Z.one; message = `EOL}
  in
  (inbox_level, [sol; eol])

(** Creates inputs based on string messages. *)
let strs_to_inputs inbox_level messages =
  List.fold_left
    (fun (acc, message_counter) message ->
      let input = make_external_input ~inbox_level ~message_counter message in
      ({input; message = `Message message} :: acc, Z.succ message_counter))
    ([], Z.one)
    messages

(** Transform messages into inputs and wrap them between SOL and EOL. *)
let wrap_messages inbox_level strs =
  let sol = {input = make_sol ~inbox_level; message = `SOL} in
  let rev_inputs, message_counter = strs_to_inputs inbox_level strs in
  let inputs = List.rev rev_inputs in
  let eol = {input = make_eol ~inbox_level ~message_counter; message = `EOL} in
  (sol :: inputs) @ [eol]

let gen_messages_for_levels ~start_level ~max_level gen_message =
  let open QCheck2.Gen in
  let rec aux acc n =
    match n with
    | n when n < 0 ->
        (* Prevent [Stack_overflow]. *)
        assert false
    | 0 -> return acc
    | n ->
        let inbox_level =
          Raw_level.of_int32_exn (Int32.of_int (start_level + n - 1))
        in
        let* empty_level = bool in
        let* level_messages =
          if empty_level then return (make_empty_level inbox_level)
          else
            let* messages =
              let* input = gen_message in
              let* inputs = small_list gen_message in
              return (input :: inputs)
            in
            return (inbox_level, wrap_messages inbox_level messages)
        in
        aux (level_messages :: acc) (n - 1)
  in
  aux [] (max_level - start_level)

(** {1. Below [Alpha_context].} *)

let message_serialize_repr msg =
  WithExceptions.Result.get_ok
    ~loc:__LOC__
    Sc_rollup_inbox_message_repr.(serialize msg)

let make_external_inbox_message_repr str = message_serialize_repr (External str)

let make_internal_inbox_message_repr internal_msg =
  message_serialize_repr (Internal internal_msg)

let make_input_repr ?(inbox_level = Raw_level_repr.root)
    ?(message_counter = Z.zero) payload =
  Sc_rollup_PVM_sig.Inbox_message {inbox_level; message_counter; payload}

let make_external_input_repr ?inbox_level ?message_counter str =
  let payload = make_external_inbox_message_repr str in
  make_input_repr ?inbox_level ?message_counter payload

let make_sol_repr ~inbox_level =
  let payload = make_internal_inbox_message_repr Start_of_level in
  make_input_repr ~inbox_level ~message_counter:Z.zero payload

let make_eol_repr ~inbox_level ~message_counter =
  let payload = make_internal_inbox_message_repr End_of_level in
  make_input_repr ~inbox_level ~message_counter payload

(** Message is the combination of a [message] and its associated [input].

    [message] is used to:
    - Construct the protocol inbox, when [message] is [`Message]. The protocol
      adds [`SOL] and [`EOL] itself.
    - Construct the players' inboxes.

    [input] is used to evaluate the players' inboxes.

*)
type message_repr = {
  input_repr : Sc_rollup_PVM_sig.input;
  message_repr : [`SOL | `Message of string | `EOL];
}

let pp_input_repr fmt (input_repr : Sc_rollup_PVM_sig.input) =
  match input_repr with
  | Reveal _ -> assert false
  | Inbox_message {inbox_level; message_counter; _} ->
      Format.fprintf
        fmt
        "(%a, %s)"
        Raw_level_repr.pp
        inbox_level
        (Z.to_string message_counter)

let pp_message_repr fmt {input_repr; message_repr} =
  Format.fprintf
    fmt
    "{ input_repr = %a; message_repr = %S }"
    pp_input_repr
    input_repr
    (match message_repr with
    | `SOL -> "SOL"
    | `Message msg -> msg
    | `EOL -> "EOL")

(** An empty inbox level is a SOL and EOL. *)
let make_empty_level_repr inbox_level =
  let sol = {input_repr = make_sol_repr ~inbox_level; message_repr = `SOL} in
  let eol =
    {
      input_repr = make_eol_repr ~inbox_level ~message_counter:Z.one;
      message_repr = `EOL;
    }
  in
  (inbox_level, [sol; eol])

(** Creates input_reprs based on string message_reprs. *)
let strs_to_input_reprs_repr inbox_level message_reprs =
  List.fold_left
    (fun (acc, message_counter) message_repr ->
      let input_repr =
        make_external_input_repr ~inbox_level ~message_counter message_repr
      in
      ( {input_repr; message_repr = `Message message_repr} :: acc,
        Z.succ message_counter ))
    ([], Z.one)
    message_reprs

(** Transform message_reprs into input_reprs and wrap them between SOL and EOL. *)
let wrap_message_reprs_repr inbox_level strs =
  let sol = {input_repr = make_sol_repr ~inbox_level; message_repr = `SOL} in
  let rev_input_reprs, message_counter =
    strs_to_input_reprs_repr inbox_level strs
  in
  let input_reprs = List.rev rev_input_reprs in
  let eol =
    {
      input_repr = make_eol_repr ~inbox_level ~message_counter;
      message_repr = `EOL;
    }
  in
  (sol :: input_reprs) @ [eol]

let gen_message_reprs_for_levels_repr ~start_level ~max_level gen_message_repr =
  let open QCheck2.Gen in
  let rec aux acc n =
    match n with
    | 0 -> return acc
    | n when n > 0 ->
        let inbox_level =
          Raw_level_repr.of_int32_exn (Int32.of_int (start_level + n - 1))
        in
        let* empty_level = bool in
        let* level_message_reprs =
          if empty_level then return (make_empty_level_repr inbox_level)
          else
            let* message_reprs =
              let* input_repr = gen_message_repr in
              let* input_reprs = small_list gen_message_repr in
              return (input_repr :: input_reprs)
            in
            return
              (inbox_level, wrap_message_reprs_repr inbox_level message_reprs)
        in
        aux (level_message_reprs :: acc) (n - 1)
    | _ ->
        (* Prevent [Stack_overflow]. *)
        assert false
  in
  aux [] (max_level - start_level)
