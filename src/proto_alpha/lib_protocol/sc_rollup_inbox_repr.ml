(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(**

   A Merkelized inbox represents a list of messages. This list
   is decomposed into sublists of messages, one for each Tezos level greater
   than the level where SCORU is activated.

   This module is designed to:

   1. provide a space-efficient representation for proofs of inbox
      inclusions (only for inboxes obtained at the end of block
      validation) ;

   2. offer an efficient function to add a new batch of messages in the
      inbox at the current level.

   To solve (1), we use a proof tree H which is implemented by a merkelized skip
   list allowing for compact inclusion proofs (See {!skip_list_repr.ml}).

   To solve (2), we maintain a separate proof tree C witnessing the contents of
   messages of the current level also implemented by a merkelized skip list for
   the same reason.

   The protocol maintains the hashes of the head of H and C.

   The rollup node needs to maintain a full representation for C and a
   partial representation for H back to the level of the LCC.

*)
type error += Inbox_proof_error of string

type error += Tried_to_add_zero_messages

type error += Inbox_level_reached_messages_limit

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"internal.smart_rollup_inbox_proof_error"
    ~title:
      "Internal error: error occurred during proof production or validation"
    ~description:"An inbox proof error."
    ~pp:(fun ppf e -> Format.fprintf ppf "Inbox proof error: %s" e)
    (obj1 (req "error" (string Plain)))
    (function Inbox_proof_error e -> Some e | _ -> None)
    (fun e -> Inbox_proof_error e) ;

  register_error_kind
    `Permanent
    ~id:"internal.smart_rollup_add_zero_messages"
    ~title:"Internal error: trying to add zero messages"
    ~description:
      "Message adding functions must be called with a positive number of \
       messages"
    ~pp:(fun ppf _ -> Format.fprintf ppf "Tried to add zero messages")
    empty
    (function Tried_to_add_zero_messages -> Some () | _ -> None)
    (fun () -> Tried_to_add_zero_messages) ;

  let description =
    Format.sprintf
      "There can be only %s messages in an inbox level, the limit has been \
       reached."
      (Z.to_string Constants_repr.sc_rollup_max_number_of_messages_per_level)
  in
  register_error_kind
    `Permanent
    ~id:"smart_rollup_inbox_level_reached_message_limit"
    ~title:"Inbox level reached messages limit"
    ~description
    ~pp:(fun ppf _ -> Format.pp_print_string ppf description)
    empty
    (function Inbox_level_reached_messages_limit -> Some () | _ -> None)
    (fun () -> Inbox_level_reached_messages_limit)

module Int64_map = Map.Make (Int64)
module Hash = Smart_rollup.Inbox_hash

module Skip_list_parameters = struct
  let basis = 4
end

module Skip_list = Skip_list_repr.Make (Skip_list_parameters)

module V1 = struct
  type level_proof = {
    hash : Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.t;
    level : Raw_level_repr.t;
  }

  let level_proof_encoding =
    let open Data_encoding in
    conv
      (fun {hash; level} -> (hash, level))
      (fun (hash, level) -> {hash; level})
      (obj2
         (req
            "hash"
            Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.encoding)
         (req "level" Raw_level_repr.encoding))

  let equal_level_proof {hash; level} level_proof_2 =
    Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.equal
      hash
      level_proof_2.hash
    && Raw_level_repr.equal level level_proof_2.level

  type history_proof = (level_proof, Hash.t) Skip_list.cell

  let hash_history_proof cell =
    let {hash; level} = Skip_list.content cell in
    let back_pointers_hashes = Skip_list.back_pointers cell in
    Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.to_bytes hash
    :: (Raw_level_repr.to_int32 level |> Int32.to_string |> Bytes.of_string)
    :: List.map Hash.to_bytes back_pointers_hashes
    |> Hash.hash_bytes

  let equal_history_proof = Skip_list.equal Hash.equal equal_level_proof

  let history_proof_encoding : history_proof Data_encoding.t =
    Skip_list.encoding Hash.encoding level_proof_encoding

  let pp_level_proof fmt {hash; level} =
    Format.fprintf
      fmt
      "hash: %a@,level: %a"
      Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.pp
      hash
      Raw_level_repr.pp
      level

  let pp_history_proof fmt history_proof =
    (Skip_list.pp ~pp_content:pp_level_proof ~pp_ptr:Hash.pp) fmt history_proof

  (** Construct an inbox [history] with a given [capacity]. If you
      are running a rollup node, [capacity] needs to be large enough to
      remember any levels for which you may need to produce proofs. *)
  module History =
    Bounded_history_repr.Make
      (struct
        let name = "Smart_rollup_inbox_history"
      end)
      (Hash)
      (struct
        type t = history_proof

        let pp = pp_history_proof

        let equal = equal_history_proof

        let encoding = history_proof_encoding
      end)

  (* An inbox is composed of a metadata of type {!t}, and a [level witness]
     representing the messages of the current level (held by the
     [Raw_context.t] in the protocol).

     The metadata contains :
     - [level] : the inbox level ;
     - [old_levels_messages] : a witness of the inbox history.
  *)
  type t = {level : Raw_level_repr.t; old_levels_messages : history_proof}

  let equal inbox1 inbox2 =
    (* To be robust to addition of fields in [t]. *)
    let {level; old_levels_messages} = inbox1 in
    Raw_level_repr.equal level inbox2.level
    && equal_history_proof old_levels_messages inbox2.old_levels_messages

  let pp fmt {level; old_levels_messages} =
    Format.fprintf
      fmt
      "@[<hov 2>{ level = %a@;old_levels_messages = %a@;}@]"
      Raw_level_repr.pp
      level
      pp_history_proof
      old_levels_messages

  let hash inbox = hash_history_proof inbox.old_levels_messages

  let inbox_level inbox = inbox.level

  let old_levels_messages inbox = inbox.old_levels_messages

  let current_witness inbox =
    let {hash; _} = Skip_list.content inbox.old_levels_messages in
    hash

  let encoding =
    Data_encoding.(
      conv
        (fun {level; old_levels_messages} -> (level, old_levels_messages))
        (fun (level, old_levels_messages) -> {level; old_levels_messages})
        (obj2
           (req "level" Raw_level_repr.encoding)
           (req "old_levels_messages" history_proof_encoding)))
end

type versioned = V1 of V1.t

let versioned_encoding =
  let open Data_encoding in
  union
    [
      case
        ~title:"V1"
        (Tag 0)
        V1.encoding
        (function V1 inbox -> Some inbox)
        (fun inbox -> V1 inbox);
    ]

include V1

let of_versioned = function V1 inbox -> inbox [@@inline]

let to_versioned inbox = V1 inbox [@@inline]

type serialized_proof = string

let serialized_proof_encoding = Data_encoding.(string Hex)

type payloads_proof = {
  proof : Sc_rollup_inbox_merkelized_payload_hashes_repr.proof;
  payload : Sc_rollup_inbox_message_repr.serialized option;
}

let payloads_proof_encoding =
  let open Data_encoding in
  conv
    (fun {proof; payload} -> (proof, (payload :> string option)))
    (fun (proof, payload) ->
      {
        proof;
        payload =
          Option.map Sc_rollup_inbox_message_repr.unsafe_of_string payload;
      })
    (obj2
       (req
          "proof"
          Sc_rollup_inbox_merkelized_payload_hashes_repr.proof_encoding)
       (opt "payload" (string Hex)))

let add_protocol_internal_message payload payloads_history witness =
  Sc_rollup_inbox_merkelized_payload_hashes_repr.add_payload
    payloads_history
    witness
    payload

let add_protocol_internal_message_no_history payload witness =
  Sc_rollup_inbox_merkelized_payload_hashes_repr.add_payload_no_history
    witness
    payload

let add_message payload payloads_history witness =
  let open Result_syntax in
  let message_counter =
    Sc_rollup_inbox_merkelized_payload_hashes_repr.get_index witness
  in
  let* () =
    let max_number_of_messages_per_level =
      Constants_repr.sc_rollup_max_number_of_messages_per_level
    in
    error_unless
      Compare.Z.(message_counter <= max_number_of_messages_per_level)
      Inbox_level_reached_messages_limit
  in
  Sc_rollup_inbox_merkelized_payload_hashes_repr.add_payload
    payloads_history
    witness
    payload

let take_snapshot inbox = inbox.old_levels_messages

(** [archive history inbox witness] archives the current inbox level depending
    on the [history] parameter's [capacity]. Updates the
    [inbox.current_level] and [inbox.old_levels_messages]. *)
let archive history inbox witness =
  let open Result_syntax in
  (* [form_history_proof history inbox] adds the current inbox level to the
     history and creates new [inbox.old_levels_messages] including
     the current level. *)
  let form_history_proof history inbox =
    let prev_cell = inbox.old_levels_messages in
    let prev_cell_ptr = hash_history_proof prev_cell in
    let* history = History.remember prev_cell_ptr prev_cell history in
    let current_level_proof =
      let hash = Sc_rollup_inbox_merkelized_payload_hashes_repr.hash witness in
      {hash; level = inbox.level}
    in
    let cell = Skip_list.next ~prev_cell ~prev_cell_ptr current_level_proof in
    return (history, cell)
  in
  let* history, old_levels_messages = form_history_proof history inbox in
  let inbox = {inbox with old_levels_messages} in
  return (history, inbox)

(** [archive_no_history inbox witness] archives the current inbox level. Updates
    the [inbox.current_level] and [inbox.old_levels_messages]. *)
let archive_no_history inbox witness =
  let old_levels_messages =
    let prev_cell = inbox.old_levels_messages in
    let prev_cell_ptr = hash_history_proof prev_cell in
    let current_level_proof =
      let hash = Sc_rollup_inbox_merkelized_payload_hashes_repr.hash witness in
      {hash; level = inbox.level}
    in
    Skip_list.next ~prev_cell ~prev_cell_ptr current_level_proof
  in
  {inbox with old_levels_messages}

let add_messages payloads_history payloads witness =
  let open Result_syntax in
  let* () =
    error_when
      (match payloads with [] -> true | _ -> false)
      Tried_to_add_zero_messages
  in
  let* payloads_history, witness =
    List.fold_left_e
      (fun (payloads_history, witness) payload ->
        add_message payload payloads_history witness)
      (payloads_history, witness)
      payloads
  in
  return (payloads_history, witness)

let add_messages_no_history payloads witness =
  let open Result_syntax in
  let+ _, witness =
    add_messages
      Sc_rollup_inbox_merkelized_payload_hashes_repr.History.no_history
      payloads
      witness
  in
  witness

(* An [inclusion_proof] is a path in the Merkelized skip list
   showing that a given inbox history is a prefix of another one.
   This path has a size logarithmic in the difference between the
   levels of the two inboxes. *)
type inclusion_proof = history_proof list

let inclusion_proof_encoding =
  let open Data_encoding in
  list history_proof_encoding

let pp_inclusion_proof = Format.pp_print_list pp_history_proof

let pp_payloads_proof fmt {proof; payload} =
  Format.fprintf
    fmt
    "payload: %a@,@[<v 2>proof:@,%a@]"
    Format.(
      pp_print_option
        ~none:(fun fmt () -> pp_print_string fmt "None")
        (fun fmt payload ->
          pp_print_string
            fmt
            (Sc_rollup_inbox_message_repr.unsafe_to_string payload)))
    payload
    Sc_rollup_inbox_merkelized_payload_hashes_repr.pp_proof
    proof

(* See the main docstring for this type (in the mli file) for
   definitions of the three proof parameters [starting_point],
   [message] and [snapshot]. In the below we deconstruct
   [starting_point] into [(l, n)] where [l] is a level and [n] is a
   message index.

   In a proof, [inclusion_proof] is an inclusion proof of [history_proof] into
   [snapshot] where [history_proof] is the skip list cell for the level [l],
   and [message_proof] is a tree proof showing that

   [exists witness .
   (hash witness = history_proof.content.hash)
   AND (get_messages_payload n witness = (_, message))]

   Note: in the case that [message] is [None] this shows that there's no
   value at the index [n]; in this case we also must check that
   [history_proof] equals [snapshot]. *)
type proof = {inclusion_proof : inclusion_proof; message_proof : payloads_proof}

let pp_proof fmt {inclusion_proof; message_proof} =
  Format.fprintf
    fmt
    "@[<v>@[<v 2>inclusion proof:@,%a@]@,@[<v 2>payloads proof:@,%a@]@]"
    pp_inclusion_proof
    inclusion_proof
    pp_payloads_proof
    message_proof

let proof_encoding =
  let open Data_encoding in
  conv
    (fun {inclusion_proof; message_proof} -> (inclusion_proof, message_proof))
    (fun (inclusion_proof, message_proof) -> {inclusion_proof; message_proof})
    (obj2
       (req "inclusion_proof" inclusion_proof_encoding)
       (req "message_proof" payloads_proof_encoding))

let of_serialized_proof = Data_encoding.Binary.of_string_opt proof_encoding

let to_serialized_proof = Data_encoding.Binary.to_string_exn proof_encoding

(** [verify_payloads_proof {proof; payload} head_cell_hash n label] handles
    all the verification needed for a particular message proof at a particular
    level.

    First it checks that [proof] is a valid inclusion of [payload_cell] in
    [head_cell] and that [head_cell] hash is [head_cell_hash].

    Then there is two cases,

    - either [n] is superior to the index of [head_cell] then the provided
    [payload] must be empty (and [payload_cell = head_cell]);

    - or [0 < n < max_index head_cell] then the provided payload must exist and
    the payload hash must equal the content of the [payload_cell].
*)
let verify_payloads_proof {proof; payload} head_cell_hash n =
  let open Result_syntax in
  let* payload_cell, head_cell =
    Sc_rollup_inbox_merkelized_payload_hashes_repr.verify_proof proof
  in
  (* Checks that [proof] is a valid inclusion of [payload_cell] in
     [head_cell] and that [head_cell] hash is [head_cell_hash]. *)
  let* () =
    error_unless
      (Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.equal
         head_cell_hash
         (Sc_rollup_inbox_merkelized_payload_hashes_repr.hash head_cell))
      (Inbox_proof_error (Format.sprintf "message_proof does not match history"))
  in
  let max_index =
    Sc_rollup_inbox_merkelized_payload_hashes_repr.get_index head_cell
  in
  if Compare.Z.(n = Z.succ max_index) then
    (* [n] is equal to the index of [head_cell] then the provided [payload] must
       be init (,and [payload_cell = head_cell]) *)
    let* () =
      error_unless
        (Option.is_none payload)
        (Inbox_proof_error "Payload provided but none expected")
    in
    let* () =
      error_unless
        (Sc_rollup_inbox_merkelized_payload_hashes_repr.equal
           payload_cell
           head_cell)
        (Inbox_proof_error "Provided proof is about a unexpected payload")
    in
    return_none
  else if Compare.Z.(n <= max_index) then
    (* [0 < n < max_index head_cell] then the provided [payload] must exists and
       [payload_hash] must equal the content of the [payload_cell]. *)
    let* payload =
      match payload with
      | Some payload -> return payload
      | None ->
          tzfail
            (Inbox_proof_error
               "Expected a payload but none provided in the proof")
    in
    let payload_hash =
      Sc_rollup_inbox_message_repr.hash_serialized_message payload
    in
    let proven_payload_hash =
      Sc_rollup_inbox_merkelized_payload_hashes_repr.get_payload_hash
        payload_cell
    in
    let* () =
      error_unless
        (Sc_rollup_inbox_message_repr.Hash.equal
           payload_hash
           proven_payload_hash)
        (Inbox_proof_error
           "the payload provided does not match the payload's hash found in \
            the message proof")
    in
    let payload_index =
      Sc_rollup_inbox_merkelized_payload_hashes_repr.get_index payload_cell
    in
    let* () =
      error_unless
        (Compare.Z.equal n payload_index)
        (Inbox_proof_error
           (Format.sprintf "found index in message_proof is incorrect"))
    in
    return_some payload
  else
    tzfail
      (Inbox_proof_error
         "Provided message counter is out of the valid range [0 -- (max_index \
          + 1)]")

(** [produce_payloads_proof get_payloads_history head_cell_hash ~index]

    [get_payloads_history cell_hash] is a function that returns an
    {!Sc_rollup_inbox_merkelized_payload_hashes_repr.History.t}. The returned
    history must contains the cell with hash [cell_hash], all its ancestor cell
    and their associated payload.

    [head_cell] the latest cell of the [witness] we want to produce a proof on
    with hash [head_cell_hash].

    This function produce either:

    - if [index <= head_cell_max_index], a proof that [payload_cell] with
    [index] is an ancestor to [head_cell] where [head_cell] is the cell with
    hash [head_cell_hash]. It returns the proof and the payload associated to
    [payload_cell];

   - else a proof that [index] is out of bound for [head_cell]. It returns the
   proof and no payload.
*)
let produce_payloads_proof get_payloads_history head_cell_hash ~index =
  let open Lwt_result_syntax in
  (* We first retrieve the history of cells for this level. *)
  let*! payloads_history = get_payloads_history head_cell_hash in
  (* We then fetch the actual head cell in the history. *)
  let*? head_cell =
    match
      Sc_rollup_inbox_merkelized_payload_hashes_repr.History.find
        head_cell_hash
        payloads_history
    with
    | Some {merkelized = head_cell; payload = _} -> ok head_cell
    | None ->
        error
          (Inbox_proof_error "could not find head_cell in the payloads_history")
  in
  let head_cell_max_index =
    Sc_rollup_inbox_merkelized_payload_hashes_repr.get_index head_cell
  in
  (* if [index <= head_cell_max_index] then the index belongs to this level, we
     prove its existence. Else the index is out of bounds, we prove its
     non-existence. *)
  let target_index = Compare.Z.(min index head_cell_max_index) in
  (* We look for the cell at `target_index` starting from `head_cell`. If it
     exists, we return the payload held in this cell. Otherwise, we prove that
     [index] does not exist in this level. *)
  let proof =
    Sc_rollup_inbox_merkelized_payload_hashes_repr.produce_proof
      payloads_history
      head_cell
      ~index:target_index
  in
  match proof with
  | Some ({payload; merkelized = _}, proof) ->
      if Compare.Z.(target_index = index) then
        return {proof; payload = Some payload}
      else return {proof; payload = None}
  | None -> tzfail (Inbox_proof_error "could not produce a valid proof.")

let verify_inclusion_proof inclusion_proof snapshot_history_proof =
  let open Result_syntax in
  let rec aux (hash_map, ptr_list) = function
    | [] -> tzfail (Inbox_proof_error "inclusion proof is empty")
    | [target] ->
        let target_ptr = hash_history_proof target in
        let hash_map = Hash.Map.add target_ptr target hash_map in
        let ptr_list = target_ptr :: ptr_list in
        ok (hash_map, List.rev ptr_list, target, target_ptr)
    | history_proof :: tail ->
        let ptr = hash_history_proof history_proof in
        aux (Hash.Map.add ptr history_proof hash_map, ptr :: ptr_list) tail
  in
  let* hash_map, ptr_list, target, target_ptr =
    aux (Hash.Map.empty, []) inclusion_proof
  in
  let deref ptr = Hash.Map.find ptr hash_map in
  let cell_ptr = hash_history_proof snapshot_history_proof in
  let* () =
    error_unless
      (Skip_list.valid_back_path
         ~equal_ptr:Hash.equal
         ~deref
         ~cell_ptr
         ~target_ptr
         ptr_list)
      (Inbox_proof_error "invalid inclusion proof")
  in
  return target

let produce_inclusion_proof deref inbox_snapshot l =
  let open Lwt_result_syntax in
  let compare {hash = _; level} = Raw_level_repr.compare level l in
  let*! result = Skip_list.Lwt.search ~deref ~compare ~cell:inbox_snapshot in
  match result with
  | Skip_list.{rev_path; last_cell = Found history_proof} ->
      return (List.rev rev_path, history_proof)
  | {last_cell = Nearest _; _}
  | {last_cell = No_exact_or_lower_ptr; _}
  | {last_cell = Deref_returned_none; _} ->
      (* We are only interested in the result where [search] returns a path to
         the cell we were looking for. All the other cases should be
         considered as an error. *)
      tzfail
      @@ Inbox_proof_error
           (Format.asprintf
              "Skip_list.search failed to find a valid path: %a"
              (Skip_list.pp_search_result ~pp_cell:pp_history_proof)
              result)

let verify_proof (l, n) inbox_snapshot {inclusion_proof; message_proof} =
  assert (Z.(geq n zero)) ;
  let open Result_syntax in
  let* history_proof = verify_inclusion_proof inclusion_proof inbox_snapshot in
  let level_proof = Skip_list.content history_proof in
  let* payload_opt = verify_payloads_proof message_proof level_proof.hash n in
  match payload_opt with
  | Some payload ->
      return_some
        Sc_rollup_PVM_sig.{inbox_level = l; message_counter = n; payload}
  | None ->
      if equal_history_proof inbox_snapshot history_proof then return_none
      else
        let* payload =
          Sc_rollup_inbox_message_repr.(serialize (Internal Start_of_level))
        in
        let inbox_level = Raw_level_repr.succ l in
        let message_counter = Z.zero in
        return_some Sc_rollup_PVM_sig.{inbox_level; message_counter; payload}

let produce_proof ~get_payloads_history ~get_history inbox_snapshot (l, n) =
  let open Lwt_result_syntax in
  let* inclusion_proof, history_proof =
    produce_inclusion_proof get_history inbox_snapshot l
  in
  let level_proof = Skip_list.content history_proof in
  let* ({payload; proof = _} as message_proof) =
    produce_payloads_proof get_payloads_history level_proof.hash ~index:n
  in
  let proof = {inclusion_proof; message_proof} in
  let*? input =
    let open Result_syntax in
    match payload with
    | Some payload ->
        return_some
          Sc_rollup_PVM_sig.{inbox_level = l; message_counter = n; payload}
    | None ->
        (* No payload means that there is no more message to read at the level of
           [history_proof]. *)
        if equal_history_proof inbox_snapshot history_proof then
          (* if [history_proof] is equal to the snapshot then it means that there
             is no more message to read. *)
          return_none
        else
          (* Else we must read the [sol] of the next level. *)
          let inbox_level = Raw_level_repr.succ l in
          let message_counter = Z.zero in
          let* payload =
            Sc_rollup_inbox_message_repr.(serialize (Internal Start_of_level))
          in
          return_some Sc_rollup_PVM_sig.{inbox_level; message_counter; payload}
  in
  return (proof, input)

let init_witness payloads_history =
  let open Result_syntax in
  let sol = Sc_rollup_inbox_message_repr.start_of_level_serialized in
  let* payloads_history, witness =
    Sc_rollup_inbox_merkelized_payload_hashes_repr.genesis payloads_history sol
  in
  return (payloads_history, witness)

let init_witness_no_history =
  let sol = Sc_rollup_inbox_message_repr.start_of_level_serialized in
  Sc_rollup_inbox_merkelized_payload_hashes_repr.genesis_no_history sol

let add_info_per_level ~predecessor_timestamp ~predecessor payloads_history
    witness =
  let info_per_level =
    Sc_rollup_inbox_message_repr.info_per_level_serialized
      ~predecessor_timestamp
      ~predecessor
  in
  add_protocol_internal_message info_per_level payloads_history witness

let add_info_per_level_no_history ~predecessor_timestamp ~predecessor witness =
  let info_per_level =
    Sc_rollup_inbox_message_repr.info_per_level_serialized
      ~predecessor_timestamp
      ~predecessor
  in
  add_protocol_internal_message_no_history info_per_level witness

let finalize_inbox_level payloads_history history inbox witness =
  let open Result_syntax in
  let inbox = {inbox with level = Raw_level_repr.succ inbox.level} in
  let eol = Sc_rollup_inbox_message_repr.end_of_level_serialized in
  let* payloads_history, witness =
    add_protocol_internal_message eol payloads_history witness
  in
  let* history, inbox = archive history inbox witness in
  return (payloads_history, history, witness, inbox)

let finalize_inbox_level_no_history inbox witness =
  let inbox = {inbox with level = Raw_level_repr.succ inbox.level} in
  let eol = Sc_rollup_inbox_message_repr.end_of_level_serialized in
  let witness = add_protocol_internal_message_no_history eol witness in
  archive_no_history inbox witness

let add_all_messages ~protocol_migration_message ~predecessor_timestamp
    ~predecessor history inbox messages =
  let open Result_syntax in
  let* payloads = List.map_e Sc_rollup_inbox_message_repr.serialize messages in
  let is_first_block = Option.is_some protocol_migration_message in
  let payloads_history =
    (* Must remember every [payloads] and internal messages pushed by the
       protocol: SOL/Info_per_level/EOL. *)
    let capacity =
      (List.length payloads + 3 + if is_first_block then 1 else 0)
      |> Int64.of_int
    in
    Sc_rollup_inbox_merkelized_payload_hashes_repr.History.empty ~capacity
  in
  (* Add [SOL], [Info_per_level], and possibly [Protocol_migration]. *)
  let* payloads_history, witness = init_witness payloads_history in

  let* payloads_history, witness =
    add_info_per_level
      ~predecessor_timestamp
      ~predecessor
      payloads_history
      witness
  in

  let* payloads_history, witness =
    match protocol_migration_message with
    | Some protocol_migration_message ->
        let* message =
          Sc_rollup_inbox_message_repr.serialize
            (Internal protocol_migration_message)
        in
        add_message message payloads_history witness
    | None -> return (payloads_history, witness)
  in

  let* payloads_history, witness =
    match payloads with
    | [] -> return (payloads_history, witness)
    | payloads -> add_messages payloads_history payloads witness
  in
  let* payloads_history, history, witness, inbox =
    finalize_inbox_level payloads_history history inbox witness
  in

  (* Wrap the messages so the caller can execute every actual messages
     for this inbox. *)
  let messages =
    let open Sc_rollup_inbox_message_repr in
    let sol = Internal Start_of_level in
    let info_per_level =
      Internal (Info_per_level {predecessor_timestamp; predecessor})
    in
    let migration =
      Option.fold
        ~none:[]
        ~some:(fun x -> [Internal x])
        protocol_migration_message
    in
    let eol = Internal End_of_level in
    [sol] @ [info_per_level] @ migration @ messages @ [eol]
  in

  return (payloads_history, history, inbox, witness, messages)

let genesis ~protocol_migration_message ~predecessor_timestamp ~predecessor
    level =
  (* 1. Add [SOL], [Info_per_level] and [Protocol_migration]. *)
  let witness = init_witness_no_history in
  let witness =
    add_info_per_level_no_history ~predecessor_timestamp ~predecessor witness
  in
  let witness =
    add_protocol_internal_message_no_history protocol_migration_message witness
  in

  (* 2. Add [EOL]. *)
  let eol = Sc_rollup_inbox_message_repr.end_of_level_serialized in
  let witness = add_protocol_internal_message_no_history eol witness in

  let level_proof =
    let hash = Sc_rollup_inbox_merkelized_payload_hashes_repr.hash witness in
    {hash; level}
  in

  {level; old_levels_messages = Skip_list.genesis level_proof}

module Internal_for_tests = struct
  type nonrec inclusion_proof = inclusion_proof

  let pp_inclusion_proof = pp_inclusion_proof

  let produce_inclusion_proof = produce_inclusion_proof

  let verify_inclusion_proof = verify_inclusion_proof

  let serialized_proof_of_string x = x

  let get_level_of_history_proof (history_proof : history_proof) =
    let ({level; _} : level_proof) = Skip_list.content history_proof in
    level

  type nonrec payloads_proof = payloads_proof = {
    proof : Sc_rollup_inbox_merkelized_payload_hashes_repr.proof;
    payload : Sc_rollup_inbox_message_repr.serialized option;
  }

  let pp_payloads_proof = pp_payloads_proof

  let produce_payloads_proof = produce_payloads_proof

  let verify_payloads_proof = verify_payloads_proof

  type nonrec level_proof = level_proof = {
    hash : Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.t;
    level : Raw_level_repr.t;
  }

  let level_proof_of_history_proof = Skip_list.content

  let expose_proof {inclusion_proof; message_proof} =
    (inclusion_proof, message_proof)

  let make_proof inclusion_proof message_proof =
    {inclusion_proof; message_proof}
end

type inbox = t
