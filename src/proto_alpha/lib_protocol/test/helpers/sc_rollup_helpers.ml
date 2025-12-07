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

module Arith_pvm = Pvm_in_memory.Arith
module Wasm_pvm = Pvm_in_memory.Wasm

module Make_in_memory_context (Context : sig
  type tree

  include
    Tezos_context_sigs.Context.TEZOS_CONTEXT
      with type memory_context_tree := tree
       and type tree := tree
       and type value_key = Context_hash.t
       and type node_key = Context_hash.t
end) =
struct
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
        return_some p
    | None -> return_none

  let kinded_hash_to_state_hash = function
    | `Value hash | `Node hash ->
        Sc_rollup.State_hash.context_hash_to_state_hash hash

  let proof_before proof = kinded_hash_to_state_hash proof.Context.Proof.before

  let proof_after proof = kinded_hash_to_state_hash proof.Context.Proof.after

  let cast_read_only proof = Context.Proof.{proof with after = proof.before}

  let proof_encoding =
    Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V2.Tree2
    .tree_proof_encoding
end

module Wrong_in_memory_context =
  Make_in_memory_context (Tezos_context_memory.Context)

module Wrong_arith_pvm :
  Sc_rollup.PVM.S
    with type context = Wrong_in_memory_context.Tree.t
     and type state = Wrong_in_memory_context.tree
     and type proof =
      Tezos_context_memory.Context.Proof.tree
      Tezos_context_memory.Context.Proof.t =
  Sc_rollup.ArithPVM.Make (Wrong_in_memory_context)

let genesis_commitment ~boot_sector ~origination_level kind =
  let open Lwt_syntax in
  let+ genesis_state_hash = Sc_rollup.genesis_state_hash_of kind ~boot_sector in
  Sc_rollup.Commitment.genesis_commitment ~origination_level ~genesis_state_hash

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
    | Sc_rollups.Kind.Riscv -> Sc_rollup.Kind.Riscv
  in
  let* res = genesis_commitment ~boot_sector ~origination_level kind in
  let res =
    Data_encoding.Binary.to_bytes_exn Sc_rollup.Commitment.encoding res
    |> Data_encoding.Binary.of_bytes_exn Sc_rollup_commitment_repr.encoding
  in
  return res

(** {2 Inbox message helpers.} *)

(** {1 Above [Alpha_context].} *)

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

let make_info_per_level ~inbox_level ~predecessor_timestamp ~predecessor =
  let payload =
    make_internal_inbox_message
      (Info_per_level {predecessor_timestamp; predecessor})
  in
  make_input ~inbox_level ~message_counter:Z.one payload

let make_protocol_migration ~inbox_level =
  let payload =
    make_internal_inbox_message
      Sc_rollup.Inbox_message.protocol_migration_internal_message
  in
  make_input ~inbox_level ~message_counter:Z.(succ one) payload

(** Message is the combination of a [message] and its associated [input].

    [message] is used to:
    - Construct the protocol inbox, when [message] is [`Message]. The protocol
      adds [`SOL] and [`EOL] itself.
    - Construct the players' inboxes.

    [input] is used to evaluate the players' inboxes.

*)
type message = {
  input : Sc_rollup.input;
  message :
    [ `SOL
    | `Info_per_level of Timestamp.t * Block_hash.t
    | `Message of string
    | `EOL ];
}

(** Put as much information as possible in this record so it can be used
    in different setups:
    1. Creating an inbox on the protocol-side, requires [messages] only.
    2. Re-construct an inbox, requires [payloads], [timestamp], [predecessor].
    3. Evaluate inputs in a PVM, requires [inputs]

    [level] is useful for (1) (2) (3).
 *)
type payloads_per_level = {
  messages : string list;  (** List of external messages. *)
  payloads : Sc_rollup.Inbox_message.serialized list;
      (** List of external serialized messages. *)
  predecessor_timestamp : Time.Protocol.t;
      (** predecessor timestamp of the [Info_per_level]. *)
  predecessor : Block_hash.t;  (** Predecessor of the [Info_per_level]. *)
  level : Raw_level.t;
  inputs : Sc_rollup.input list;
      (** List of all inputs for the level, to be read by a PVM. *)
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
    (match message with
    | `SOL -> "SOL"
    | `Info_per_level (predecessor_timestamp, block_hash) ->
        Format.asprintf
          "Info_per_level (%s, %a)"
          (Timestamp.to_notation predecessor_timestamp)
          Block_hash.pp
          block_hash
    | `Message msg -> msg
    | `EOL -> "EOL")

(** Creates inputs based on string messages. *)
let strs_to_inputs inbox_level messages =
  List.fold_left
    (fun (acc, message_counter) message ->
      let input = make_external_input ~inbox_level ~message_counter message in
      ({input; message = `Message message} :: acc, Z.succ message_counter))
    ([], Z.of_int 2)
    messages

(** Transform the list of all inputs the PVM should read. *)
let make_inputs ~first_block predecessor_timestamp predecessor messages
    inbox_level =
  (* SOL is at index 0. *)
  let sol = make_sol ~inbox_level in
  (* Info_per_level is at index 1. *)
  let info_per_level =
    make_info_per_level ~inbox_level ~predecessor_timestamp ~predecessor
  in
  let mig =
    if first_block then [make_protocol_migration ~inbox_level] else []
  in
  (* External inputs start at index 2. *)
  let external_inputs =
    List.mapi
      (fun i message ->
        make_external_input
          ~inbox_level
          ~message_counter:(Z.of_int (2 + List.length mig + i))
          message)
      messages
  in
  (* EOL is after SOL/Info_per_level and all external inputs, therefore,
     at index [2 + List.length messages]. *)
  let eol =
    let message_counter =
      Z.of_int (2 + List.length mig + List.length messages)
    in
    make_eol ~inbox_level ~message_counter
  in
  [sol; info_per_level] @ mig @ external_inputs @ [eol]

let predecessor_timestamp_and_hash_from_level level =
  let level_int64 = Int64.of_int32 @@ Raw_level.to_int32 level in
  let predecessor_timestamp = Time.Protocol.of_seconds level_int64 in
  let hash = Block_hash.hash_string [Int64.to_string level_int64] in
  (predecessor_timestamp, hash)

(** Wrap messages, predecessor_timestamp and predecessor of a level into a
    [payloads_per_level] .*)
let wrap_messages level
    ?(pred_info = predecessor_timestamp_and_hash_from_level level) messages :
    payloads_per_level =
  let predecessor_timestamp, predecessor = pred_info in
  let payloads = List.map make_external_inbox_message messages in
  let inputs =
    make_inputs
      ~first_block:(level = Raw_level.root || level = Raw_level.(succ root))
      predecessor_timestamp
      predecessor
      messages
      level
  in
  {payloads; predecessor_timestamp; predecessor; messages; level; inputs}

(** An empty inbox level is a SOL,IPL and EOL. *)
let make_empty_level ?pred_info inbox_level =
  wrap_messages ?pred_info inbox_level []

let gen_messages ?pred_info inbox_level gen_message =
  let open QCheck2.Gen in
  let* input = gen_message in
  let* inputs = small_list gen_message in
  return (wrap_messages ?pred_info inbox_level (input :: inputs))

let gen_payloads_for_levels ~start_level ~max_level gen_message =
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
          else gen_messages inbox_level gen_message
        in
        aux (level_messages :: acc) (n - 1)
  in
  aux [] (max_level - start_level)

(** {1 Below [Alpha_context].} *)

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

(** An empty inbox level is a SOL,IPL and EOL. *)
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

module Payloads_histories =
  Map.Make (Sc_rollup.Inbox_merkelized_payload_hashes.Hash)

type payloads_histories =
  Sc_rollup.Inbox_merkelized_payload_hashes.History.t Payloads_histories.t

let get_payloads_history payloads_histories witness =
  Payloads_histories.find witness payloads_histories
  |> WithExceptions.Option.get ~loc:__LOC__
  |> Lwt.return

let get_history history i = Sc_rollup.Inbox.History.find i history |> Lwt.return

let inbox_message_of_input input =
  match input with Sc_rollup.Inbox_message x -> Some x | _ -> None

let payloads_from_messages =
  List.map (fun {input; _} ->
      match input with
      | Inbox_message {payload; _} -> payload
      | Reveal _ -> assert false)

let first_after payloads_per_levels level message_counter =
  let payloads_at_level level =
    List.find
      (fun {level = payloads_level; _} -> level = payloads_level)
      payloads_per_levels
  in
  let payloads_per_level =
    WithExceptions.Option.get ~loc:__LOC__ @@ payloads_at_level level
  in
  match List.nth payloads_per_level.inputs (Z.to_int message_counter) with
  | Some input -> inbox_message_of_input input
  | None -> (
      (* If no input at (l, n), the next input is (l+1, 0). *)
      let next_level = Raw_level.succ level in
      match payloads_at_level next_level with
      | None -> None
      | Some payloads_per_level ->
          let input = Stdlib.List.hd payloads_per_level.inputs in
          inbox_message_of_input input)

let list_of_inputs_from_list_of_messages
    (payloads_per_levels : message list list) =
  List.map
    (fun inputs ->
      let payloads = List.map (fun {input; _} -> input) inputs in
      payloads)
    payloads_per_levels

let dumb_init level =
  Sc_rollup.Inbox.genesis
    ~predecessor_timestamp:Time.Protocol.epoch
    ~predecessor:Block_hash.zero
    level

let dumb_init_repr level =
  Sc_rollup_inbox_repr.genesis
    ~protocol_migration_message:
      Raw_context.protocol_migration_serialized_message
    ~predecessor_timestamp:Time.Protocol.epoch
    ~predecessor:Block_hash.zero
    level

let origination_op ?force_reveal ?counter ?fee ?gas_limit ?storage_limit
    ?(boot_sector = "") ?(parameters_ty = "unit") ?whitelist ctxt src kind =
  Op.sc_rollup_origination
    ?force_reveal
    ?counter
    ?fee
    ?gas_limit
    ?storage_limit
    ?whitelist
    ctxt
    src
    kind
    ~boot_sector
    ~parameters_ty:(Script.lazy_expr @@ Expr.from_string parameters_ty)

let latest_level_proof inbox =
  Sc_rollup.Inbox.Internal_for_tests.level_proof_of_history_proof
  @@ Sc_rollup.Inbox.old_levels_messages inbox

let latest_level_proof_hash inbox = (latest_level_proof inbox).hash

module Node_inbox = struct
  type t = {
    inbox : Sc_rollup.Inbox.t;
    history : Sc_rollup.Inbox.History.t;
    payloads_histories : payloads_histories;
  }

  let new_inbox ?(genesis_predecessor_timestamp = Time.Protocol.epoch)
      ?(genesis_predecessor = Block_hash.zero)
      ?(inbox_creation_level = Raw_level.root) () =
    let open Result_syntax in
    let inbox =
      Sc_rollup.Inbox.genesis
        ~predecessor_timestamp:genesis_predecessor_timestamp
        ~predecessor:genesis_predecessor
        inbox_creation_level
    in
    let history = Sc_rollup.Inbox.History.empty ~capacity:10000L in
    let payloads_histories = Payloads_histories.empty in
    return {inbox; history; payloads_histories}

  let fill_inbox ~inbox_creation_level node_inbox payloads_per_levels =
    let open Result_wrap_syntax in
    let rec aux {inbox; history; payloads_histories} = function
      | [] -> return {inbox; history; payloads_histories}
      | ({
           payloads = _;
           predecessor_timestamp;
           predecessor;
           messages;
           level;
           inputs = _;
         } :
          payloads_per_level)
        :: rst ->
          let messages =
            List.map
              (fun message -> Sc_rollup.Inbox_message.External message)
              messages
          in
          let*@ payloads_history, history, inbox, witness, _messages =
            Sc_rollup.Inbox.add_all_messages
              ~first_block:Raw_level.(equal (succ inbox_creation_level) level)
              ~dal_attested_slots_messages:[]
              ~predecessor_timestamp
              ~predecessor
              history
              inbox
              messages
          in
          (* Store in the history this archived level. *)
          let witness_hash =
            Sc_rollup.Inbox_merkelized_payload_hashes.hash witness
          in
          let payloads_histories =
            Payloads_histories.add
              witness_hash
              payloads_history
              payloads_histories
          in
          aux {inbox; history; payloads_histories} rst
    in
    aux node_inbox payloads_per_levels

  let construct_inbox ?(inbox_creation_level = Raw_level.root)
      ?genesis_predecessor_timestamp ?genesis_predecessor payloads_per_levels =
    let open Result_syntax in
    let* node_inbox =
      new_inbox
        ?genesis_predecessor_timestamp
        ?genesis_predecessor
        ~inbox_creation_level
        ()
    in
    fill_inbox ~inbox_creation_level node_inbox payloads_per_levels

  let get_history history hash =
    Lwt.return @@ Sc_rollup.Inbox.History.find hash history

  let produce_proof {payloads_histories; history; _} inbox_snapshot
      (level, message_counter) =
    let open Lwt_result_wrap_syntax in
    wrap
    @@ Sc_rollup.Inbox.produce_proof
         ~get_payloads_history:(get_payloads_history payloads_histories)
         ~get_history:(get_history history)
         inbox_snapshot
         (level, message_counter)

  let produce_and_expose_proof node_inbox node_inbox_snapshot
      (level, message_counter) =
    let open Lwt_result_syntax in
    let* proof, input =
      produce_proof node_inbox node_inbox_snapshot (level, message_counter)
    in
    let exposed_proof = Sc_rollup.Inbox.Internal_for_tests.expose_proof proof in
    return (exposed_proof, input)

  let produce_payloads_proof {payloads_histories; _}
      (head_cell_hash : Sc_rollup.Inbox_merkelized_payload_hashes.Hash.t)
      message_counter =
    let open Lwt_result_wrap_syntax in
    wrap
    @@ Sc_rollup.Inbox.Internal_for_tests.produce_payloads_proof
         (get_payloads_history payloads_histories)
         head_cell_hash
         ~index:message_counter

  let produce_inclusion_proof {history; _} inbox_snapshot level =
    let open Lwt_result_wrap_syntax in
    wrap
    @@ Sc_rollup.Inbox.Internal_for_tests.produce_inclusion_proof
         (get_history history)
         inbox_snapshot
         level
end

module Protocol_inbox = struct
  let new_inbox ?(genesis_predecessor_timestamp = Time.Protocol.epoch)
      ?(genesis_predecessor = Block_hash.zero)
      ?(inbox_creation_level = Raw_level.root) () =
    Sc_rollup.Inbox.genesis
      ~predecessor_timestamp:genesis_predecessor_timestamp
      ~predecessor:genesis_predecessor
      inbox_creation_level

  let fill_inbox ~inbox_creation_level inbox payloads_per_levels =
    let open Result_wrap_syntax in
    let rec aux inbox = function
      | [] -> return inbox
      | ({
           payloads = _;
           predecessor_timestamp;
           predecessor;
           messages;
           level;
           inputs = _;
         } :
          payloads_per_level)
        :: rst ->
          let payloads =
            List.map
              (fun message -> Sc_rollup.Inbox_message.(External message))
              messages
          in
          let*@ _, _, inbox, _, _ =
            Sc_rollup.Inbox.add_all_messages
              ~first_block:Raw_level.(equal (succ inbox_creation_level) level)
              ~dal_attested_slots_messages:[]
              ~predecessor_timestamp
              ~predecessor
              (Sc_rollup.Inbox.History.empty ~capacity:1000L)
              inbox
              payloads
          in
          aux inbox rst
    in
    aux inbox payloads_per_levels

  let add_new_level ?pred_info inbox messages =
    let next_level = Raw_level.succ @@ Sc_rollup.Inbox.inbox_level inbox in
    let messages_per_level = wrap_messages ?pred_info next_level messages in
    fill_inbox inbox [messages_per_level]

  let add_new_empty_level ?pred_info inbox =
    let next_level = Raw_level.succ @@ Sc_rollup.Inbox.inbox_level inbox in
    let empty_level = [make_empty_level ?pred_info next_level] in
    fill_inbox inbox empty_level

  let construct_inbox ?(inbox_creation_level = Raw_level.root)
      ?genesis_predecessor_timestamp ?genesis_predecessor payloads_per_levels =
    let inbox =
      new_inbox
        ?genesis_predecessor_timestamp
        ?genesis_predecessor
        ~inbox_creation_level
        ()
    in
    fill_inbox ~inbox_creation_level inbox payloads_per_levels
end

let construct_node_and_protocol_inbox ?inbox_creation_level
    ?genesis_predecessor_timestamp ?genesis_predecessor payloads_per_levels =
  let open Result_syntax in
  let* node_inbox =
    Node_inbox.construct_inbox
      ?inbox_creation_level
      ?genesis_predecessor_timestamp
      ?genesis_predecessor
      payloads_per_levels
  in
  let* protocol_inbox =
    Protocol_inbox.construct_inbox
      ?inbox_creation_level
      ?genesis_predecessor_timestamp
      ?genesis_predecessor
      payloads_per_levels
  in
  return (node_inbox, protocol_inbox)

module Protocol_inbox_with_ctxt = struct
  let fill_inbox block list_of_messages contract =
    let open Lwt_result_syntax in
    let* block, list_of_messages =
      List.fold_left_map_es
        (fun (block : Block.t) ({messages; _} as messages_per_level) ->
          let predecessor = block.hash in
          let predecessor_timestamp = block.header.shell.timestamp in

          let* block =
            match messages with
            | [] ->
                let* block = Block.bake block in
                return block
            | messages ->
                let* operation_add_message =
                  Op.sc_rollup_add_messages (B block) contract messages
                in
                let* block =
                  Block.bake ~operation:operation_add_message block
                in
                return block
          in

          return
            (block, {messages_per_level with predecessor; predecessor_timestamp}))
        block
        list_of_messages
    in
    return (block, list_of_messages)
end

let is_reveal_enabled_default =
  Sc_rollup.is_reveal_enabled_predicate
    Default_parameters.constants_mainnet.sc_rollup.reveal_activation_level

let tick_of_int_exn ?(__LOC__ = __LOC__) n =
  WithExceptions.Option.get ~loc:__LOC__ (Sc_rollup.Tick.of_int n)

module type PVM_eval = sig
  include Sc_rollup.PVM.S

  val make_empty_context : unit -> context

  val initial_hash : hash Lwt.t

  val eval_until_input :
    fuel:int option ->
    our_states:(int * hash) trace ->
    int ->
    state ->
    (state * int option * int * (int * hash) trace) Lwt.t

  val eval_inputs_from_initial_state :
    metadata:Sc_rollup.Metadata.t ->
    ?fuel:int ->
    ?bootsector:string ->
    Sc_rollup.input trace trace ->
    (state * Sc_rollup.Tick.t * (Sc_rollup.Tick.t * hash) trace, 'a) result
    Lwt.t
end

module Make_PVM_eval (PVM : sig
  include Sc_rollup.PVM.S

  val make_empty_state : unit -> state

  val make_empty_context : unit -> context
end) : PVM_eval with type context = PVM.context and type state = PVM.state =
struct
  include PVM

  let bootsector_state ~bootsector =
    let open Lwt_syntax in
    let empty = make_empty_state () in
    let* state = initial_state ~empty in
    let* state = install_boot_sector state bootsector in
    return state

  let initial_hash =
    let open Lwt_syntax in
    let empty = make_empty_state () in
    let* state = initial_state ~empty in
    state_hash state

  let consume_fuel = Option.map pred

  let continue_with_fuel ~our_states ~(tick : int) fuel state f =
    let open Lwt_syntax in
    match fuel with
    | Some 0 -> return (state, fuel, tick, our_states)
    | _ -> f tick our_states (consume_fuel fuel) state

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3498

     the following is almost the same code as in the rollup node, except that it
     creates the association list (tick, state_hash). *)
  let eval_until_input ~fuel ~our_states start_tick state =
    let open Lwt_syntax in
    let rec go ~our_states fuel (tick : int) state =
      let* input_request =
        is_input_state ~is_reveal_enabled:is_reveal_enabled_default state
      in
      match fuel with
      | Some 0 -> return (state, fuel, tick, our_states)
      | None | Some _ -> (
          match input_request with
          | No_input_required ->
              let* state = eval state in
              let* state_hash = state_hash state in
              let our_states = (tick, state_hash) :: our_states in
              go ~our_states (consume_fuel fuel) (tick + 1) state
          | Needs_reveal (Request_dal_page _pid) ->
              (* TODO/DAL: https://gitlab.com/tezos/tezos/-/issues/4160
                 We assume that there are no confirmed Dal slots.
                 We'll reuse the infra to provide Dal pages in the future. *)
              let input = Sc_rollup.(Reveal (Dal_page None)) in
              let* state = set_input input state in
              let* state_hash = state_hash state in
              let our_states = (tick, state_hash) :: our_states in
              go ~our_states (consume_fuel fuel) (tick + 1) state
          | Needs_reveal (Reveal_raw_data _)
          | Needs_reveal Reveal_metadata
          | Needs_reveal Reveal_dal_parameters
          | Initial | First_after _ ->
              return (state, fuel, tick, our_states)
          | Needs_reveal (Request_adal_page _) ->
              (* ADAL/FIXME: https://gitlab.com/tezos/tezos/-/milestones/410

                 to be implemented when needed. *)
              assert false)
    in

    go ~our_states fuel start_tick state

  let eval_metadata ~fuel ~our_states tick state ~metadata =
    let open Lwt_syntax in
    continue_with_fuel ~our_states ~tick fuel state
    @@ fun tick our_states fuel state ->
    let input = Sc_rollup.(Reveal (Metadata metadata)) in
    let* state = set_input input state in
    let* state_hash = state_hash state in
    let our_states = (tick, state_hash) :: our_states in
    let tick = succ tick in
    return (state, fuel, tick, our_states)

  let feed_input ~fuel ~our_states ~tick state input =
    let open Lwt_syntax in
    let* state, fuel, tick, our_states =
      eval_until_input ~fuel ~our_states tick state
    in
    continue_with_fuel ~our_states ~tick fuel state
    @@ fun tick our_states fuel state ->
    let* state = set_input input state in
    let* state_hash = state_hash state in
    let our_states = (tick, state_hash) :: our_states in
    let tick = tick + 1 in
    let* state, fuel, tick, our_states =
      eval_until_input ~fuel ~our_states tick state
    in
    return (state, fuel, tick, our_states)

  let eval_inbox ?fuel ~inputs ~tick state =
    let open Lwt_result_syntax in
    List.fold_left_es
      (fun (state, fuel, tick, our_states) input ->
        let*! state, fuel, tick, our_states =
          feed_input ~fuel ~our_states ~tick state input
        in
        return (state, fuel, tick, our_states))
      (state, fuel, tick, [])
      inputs

  let eval_inputs ~fuel ~tick ?(our_states = []) ~inputs_per_levels state =
    let open Lwt_result_syntax in
    List.fold_left_es
      (fun (state, fuel, tick, our_states) inputs ->
        let* state, fuel, tick, our_states' =
          eval_inbox ?fuel ~inputs ~tick state
        in
        return (state, fuel, tick, our_states @ our_states'))
      (state, fuel, tick, our_states)
      inputs_per_levels

  let eval_inputs_from_initial_state ~metadata ?fuel ?(bootsector = "")
      inputs_per_levels =
    let open Lwt_result_syntax in
    let*! state = bootsector_state ~bootsector in
    let*! state_hash = state_hash state in
    let tick = 0 in
    let our_states = [(tick, state_hash)] in
    let tick = succ tick in
    (* 1. We evaluate the boot sector. *)
    let*! state, fuel, tick, our_states =
      eval_until_input ~fuel ~our_states tick state
    in
    (* 2. We evaluate the metadata. *)
    let*! state, fuel, tick, our_states =
      eval_metadata ~fuel ~our_states tick state ~metadata
    in
    (* 3. We evaluate the inbox. *)
    let* state, _fuel, tick, our_states =
      eval_inputs state ~fuel ~tick ~our_states ~inputs_per_levels
    in
    let our_states =
      List.sort (fun (x, _) (y, _) -> Compare.Int.compare x y) our_states
    in
    let our_states =
      List.map
        (fun (tick_int, state) -> (tick_of_int_exn tick_int, state))
        our_states
    in
    let tick = tick_of_int_exn tick in
    return (state, tick, our_states)
end

module Arith_pvm_eval = Make_PVM_eval (Arith_pvm)
module Wasm_pvm_eval = Make_PVM_eval (Wasm_pvm)

let make_pvm_with_context_and_state (type context state)
    (module PVM : Sc_rollup.PVM.S
      with type context = context
       and type state = state) ~state ~context ~reveal ~inbox () :
    (module Sc_rollup.Proof.PVM_with_context_and_state) =
  let Node_inbox.{payloads_histories; history; inbox} = inbox in
  (module struct
    include PVM

    let context : context = context

    let state = state

    let reveal = reveal

    module Inbox_with_history = struct
      let inbox = Sc_rollup.Inbox.old_levels_messages inbox

      let get_history inbox =
        Sc_rollup.Inbox.History.find inbox history |> Lwt.return

      let get_payloads_history witness_hash =
        Payloads_histories.find witness_hash payloads_histories
        |> WithExceptions.Option.get ~loc:__LOC__
        |> Lwt.return
    end

    (* FIXME/DAL-REFUTATION: https://gitlab.com/tezos/tezos/-/issues/3992
       Extend refutation game to handle Dal refutation case. *)
    module Dal_with_history = struct
      let confirmed_slots_history = Dal.Slots_history.genesis

      let get_history _hash = Lwt.return_none

      let page_info = None

      let dal_activation_level =
        let constants = Default_parameters.constants_test in
        if constants.dal.feature_enable then
          Some constants.sc_rollup.reveal_activation_level.dal_parameters
        else None

      let dal_attested_slots_validity_lag =
        let constants = Default_parameters.constants_test in
        constants.sc_rollup.reveal_activation_level
          .dal_attested_slots_validity_lag
    end
  end)
