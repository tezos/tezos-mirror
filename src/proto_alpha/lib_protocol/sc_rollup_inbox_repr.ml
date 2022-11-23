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
type error += Invalid_level_add_messages of Raw_level_repr.t

type error += Inbox_proof_error of string

type error += Tried_to_add_zero_messages

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"sc_rollup_inbox.invalid_level_add_messages"
    ~title:"Internal error: Trying to add a message to an inbox from the past"
    ~description:
      "An inbox can only accept messages for its current level or for the next \
       levels."
    (obj1 (req "level" Raw_level_repr.encoding))
    (function Invalid_level_add_messages level -> Some level | _ -> None)
    (fun level -> Invalid_level_add_messages level) ;

  register_error_kind
    `Permanent
    ~id:"sc_rollup_inbox.inbox_proof_error"
    ~title:
      "Internal error: error occurred during proof production or validation"
    ~description:"An inbox proof error."
    ~pp:(fun ppf e -> Format.fprintf ppf "Inbox proof error: %s" e)
    (obj1 (req "error" string))
    (function Inbox_proof_error e -> Some e | _ -> None)
    (fun e -> Inbox_proof_error e) ;

  register_error_kind
    `Permanent
    ~id:"sc_rollup_inbox.add_zero_messages"
    ~title:"Internal error: trying to add zero messages"
    ~description:
      "Message adding functions must be called with a positive number of \
       messages"
    ~pp:(fun ppf _ -> Format.fprintf ppf "Tried to add zero messages")
    empty
    (function Tried_to_add_zero_messages -> Some () | _ -> None)
    (fun () -> Tried_to_add_zero_messages)

module Int64_map = Map.Make (Int64)

(* 32 *)
let hash_prefix = "\003\250\174\238\208" (* scib1(55) *)

module Hash = struct
  let prefix = "scib1"

  let encoded_size = 55

  module H =
    Blake2B.Make
      (Base58)
      (struct
        let name = "inbox_hash"

        let title = "The hash of an inbox of a smart contract rollup"

        let b58check_prefix = hash_prefix

        (* defaults to 32 *)
        let size = None
      end)

  include H

  let () = Base58.check_encoded_prefix b58check_encoding prefix encoded_size

  include Path_encoding.Make_hex (H)
end

module Skip_list_parameters = struct
  let basis = 2
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
        let name = "inbox_history"
      end)
      (Hash)
      (struct
        type t = history_proof

        let pp = pp_history_proof

        let equal = equal_history_proof

        let encoding = history_proof_encoding
      end)

  (*

   At a given level, an inbox is composed of metadata of type [t] and
   [current_level], a [tree] representing the messages of the current level
   (held by the [Raw_context.t] in the protocol).

   The metadata contains :
   - [level] : the inbox level ;
   - [nb_messages_in_commitment_period] :
     the number of messages during the commitment period ;
   - [current_level_proof] : the [current_level] and its root hash ;
   - [old_levels_messages] : a witness of the inbox history.

   When new messages are appended to the current level inbox, the
   metadata stored in the context may be related to an older level.
   In that situation, an archival process is applied to the metadata.
   This process saves the [current_level_proof] in the
   [old_levels_messages] and empties [current_level]. It then
   initializes a new level tree for the new messages---note that any
   intermediate levels are simply skipped. See
   {!Make_hashing_scheme.archive_if_needed} for details.

  *)
  type t = {
    level : Raw_level_repr.t;
    nb_messages_in_commitment_period : int64;
    (* Lazy to avoid hashing O(n^2) time in [add_messages] *)
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4242

       Because there is a current level proof at all level with `sol/eol` we no
       longer need to delay the computation. The computation can be done only
       once with `eol`. *)
    current_level_proof : unit -> level_proof;
    old_levels_messages : history_proof;
  }

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3978

     The number of messages during commitment period is broken with the
     unique inbox. *)

  let equal inbox1 inbox2 =
    (* To be robust to addition of fields in [t]. *)
    let {
      level;
      nb_messages_in_commitment_period;
      current_level_proof;
      old_levels_messages;
    } =
      inbox1
    in
    Raw_level_repr.equal level inbox2.level
    && Compare.Int64.(
         equal
           nb_messages_in_commitment_period
           inbox2.nb_messages_in_commitment_period)
    && equal_level_proof
         (current_level_proof ())
         (inbox2.current_level_proof ())
    && equal_history_proof old_levels_messages inbox2.old_levels_messages

  let pp fmt
      {
        level;
        nb_messages_in_commitment_period;
        current_level_proof;
        old_levels_messages;
      } =
    Format.fprintf
      fmt
      "@[<hov 2>{ level = %a@;\
       current messages hash  = %a@;\
       nb_messages_in_commitment_period = %s@;\
       old_levels_messages = %a@;\
       }@]"
      Raw_level_repr.pp
      level
      pp_level_proof
      (current_level_proof ())
      (Int64.to_string nb_messages_in_commitment_period)
      pp_history_proof
      old_levels_messages

  let inbox_level inbox = inbox.level

  let old_levels_messages inbox = inbox.old_levels_messages

  let current_level_proof inbox = inbox.current_level_proof ()

  let encoding =
    Data_encoding.(
      conv
        (fun {
               nb_messages_in_commitment_period;
               level;
               current_level_proof;
               old_levels_messages;
             } ->
          ( nb_messages_in_commitment_period,
            level,
            current_level_proof (),
            old_levels_messages ))
        (fun ( nb_messages_in_commitment_period,
               level,
               current_level_proof,
               old_levels_messages ) ->
          {
            nb_messages_in_commitment_period;
            level;
            current_level_proof = (fun () -> current_level_proof);
            old_levels_messages;
          })
        (obj4
           (req "nb_messages_in_commitment_period" int64)
           (req "level" Raw_level_repr.encoding)
           (req "current_level_proof" level_proof_encoding)
           (req "old_levels_messages" history_proof_encoding)))

  let number_of_messages_during_commitment_period inbox =
    inbox.nb_messages_in_commitment_period
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

let serialized_proof_encoding = Data_encoding.(string' Hex)

type level_tree_proof = {
  proof : Sc_rollup_inbox_merkelized_payload_hashes_repr.proof;
  payload : Sc_rollup_inbox_message_repr.serialized option;
}

let level_tree_proof_encoding =
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
       (opt "payload" string))

let add_message inbox payload level_tree_history level_tree =
  let open Result_syntax in
  let* level_tree_history, level_tree =
    Sc_rollup_inbox_merkelized_payload_hashes_repr.add_payload
      level_tree_history
      level_tree
      payload
  in
  let nb_messages_in_commitment_period =
    Int64.succ inbox.nb_messages_in_commitment_period
  in
  let inbox = {inbox with nb_messages_in_commitment_period} in
  return (level_tree_history, level_tree, inbox)

(** [initialize_level_tree_when_needed level_tree_history inbox level_tree
    payloads] creates a new [level_tree] with the first element of [payloads] if
    [level_tree] is None. *)
let initialize_level_tree_when_needed level_tree_history inbox level_tree
    payloads =
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4242

     This function is needed because a skip list can't be empty. A level
     tree is never empty because of `sol` so this corner case can be
     removed. *)
  let open Result_syntax in
  match level_tree with
  | Some level_tree -> return (inbox, level_tree_history, level_tree, payloads)
  | None ->
      let* first_payload, payloads =
        match payloads with
        | hd :: tl -> return (hd, tl)
        | [] -> error Tried_to_add_zero_messages
      in
      let* level_tree_history, level_tree =
        Sc_rollup_inbox_merkelized_payload_hashes_repr.genesis
          level_tree_history
          first_payload
      in
      let nb_messages_in_commitment_period =
        Int64.succ inbox.nb_messages_in_commitment_period
      in
      let inbox = {inbox with nb_messages_in_commitment_period} in
      return (inbox, level_tree_history, level_tree, payloads)

(** [no_history] creates an empty history with [capacity] set to
    zero---this makes the [remember] function a no-op. We want this
    behaviour in the protocol because we don't want to store
    previous levels of the inbox. *)
let no_history = History.empty ~capacity:0L

let take_snapshot inbox = inbox.old_levels_messages

let form_history_proof history inbox =
  let open Result_syntax in
  let prev_cell = inbox.old_levels_messages in
  let prev_cell_ptr = hash_history_proof prev_cell in
  let* history = History.remember prev_cell_ptr prev_cell history in
  let level_proof = current_level_proof inbox in
  let cell = Skip_list.next ~prev_cell ~prev_cell_ptr level_proof in
  return (history, cell)

(** [archive_if_needed level_tree_history history inbox new_level level_tree]
    is responsible for ensuring that the {!add_messages} function
    below has a correctly set-up [level_tree] to which to add the
    messages. If [new_level] is a higher level than the current inbox,
    we create a new inbox level tree at that level in which to start
    adding messages, and archive the earlier levels depending on the
    [history] parameter's [capacity]. If [level_tree] is [None] (this
    happens when the inbox is first created) we similarly create a new
    empty level tree.

    This function is the only place we begin new level trees. *)
let archive_if_needed level_tree_history history inbox new_level level_tree
    payloads =
  let open Result_syntax in
  if Raw_level_repr.(inbox.level = new_level) then
    match level_tree with
    | Some tree -> return (history, inbox, level_tree_history, tree, payloads)
    | None ->
        let* inbox, level_tree_history, tree, payloads =
          initialize_level_tree_when_needed
            level_tree_history
            inbox
            level_tree
            payloads
        in
        return (history, inbox, level_tree_history, tree, payloads)
  else
    let* history, old_levels_messages = form_history_proof history inbox in
    let* inbox, level_tree_history, tree, payloads =
      initialize_level_tree_when_needed
        level_tree_history
        inbox
        level_tree
        payloads
    in
    let inbox =
      {
        current_level_proof = inbox.current_level_proof;
        nb_messages_in_commitment_period =
          inbox.nb_messages_in_commitment_period;
        old_levels_messages;
        level = new_level;
      }
    in
    return (history, inbox, level_tree_history, tree, payloads)

let add_messages level_tree_history history inbox level payloads level_tree =
  let open Result_syntax in
  let* () =
    error_when
      (match payloads with [] -> true | _ -> false)
      Tried_to_add_zero_messages
  in
  let* () =
    error_when
      Raw_level_repr.(level < inbox.level)
      (Invalid_level_add_messages level)
  in
  let* history, inbox, level_tree_history, level_tree, payloads =
    archive_if_needed level_tree_history history inbox level level_tree payloads
  in
  let* level_tree_history, level_tree, inbox =
    List.fold_left_e
      (fun (level_tree_history, level_tree, inbox) payload ->
        add_message inbox payload level_tree_history level_tree)
      (level_tree_history, level_tree, inbox)
      payloads
  in
  let current_level_proof () =
    let hash = Sc_rollup_inbox_merkelized_payload_hashes_repr.hash level_tree in
    {hash; level}
  in
  return
    (level_tree_history, level_tree, history, {inbox with current_level_proof})

let add_messages_no_history inbox level payloads level_tree =
  let open Result_syntax in
  let+ _, level_tree, _, inbox =
    add_messages
      Sc_rollup_inbox_merkelized_payload_hashes_repr.History.no_history
      no_history
      inbox
      level
      payloads
      level_tree
  in
  (level_tree, inbox)

(* An [inclusion_proof] is a path in the Merkelized skip list
   showing that a given inbox history is a prefix of another one.
   This path has a size logarithmic in the difference between the
   levels of the two inboxes. *)
type inclusion_proof = history_proof list

let inclusion_proof_encoding =
  let open Data_encoding in
  list history_proof_encoding

let pp_inclusion_proof fmt proof =
  Format.pp_print_list pp_history_proof fmt proof

let number_of_proof_steps proof = List.length proof

let lift_ptr_path deref ptr_path =
  let rec aux accu = function
    | [] -> Some (List.rev accu)
    | x :: xs -> Option.bind (deref x) @@ fun c -> aux (c :: accu) xs
  in
  aux [] ptr_path

type proof =
  (* See the main docstring for this type (in the mli file) for
     definitions of the three proof parameters [starting_point],
     [message] and [snapshot]. In the below we deconstruct
     [starting_point] into [(l, n)] where [l] is a level and [n] is a
     message index.

     In a [Single_level] proof, [history_proof] is the skip list cell for the
     level [l], [inc] is an inclusion proof of [history_proof] into [snapshot]
     and [message_proof] is an inclusion proof of [payload_level_tree] into
     [history_proof.content]:

       [exists level_tree, payload_level_tree .
            (hash_level_tree level_tree = history_proof.content.hash)
        AND (get_messages_payload message_proof = (payload_level_tree, message_opt))
        AND ((
              (message_opt = Some message)
           AND (Inbox_message.hash_payload message = payload_level_tree.content))
          OR (
               (message_opt = None)
           AND (payload_level_tree = level_tree))]

     Note: in the case that [message] is [None] this shows that there's no
     value at the index [n]; in this case we also must check that
     [history_proof] equals [snapshot] (otherwise, we'd need a [Next_level]
     proof instead). *)
  | Single_level of {inc : inclusion_proof; message_proof : level_tree_proof}
  (* See the main docstring for this type (in the mli file) for
     definitions of the three proof parameters [starting_point],
     [message] and [snapshot]. In the below we deconstruct
     [starting_point] as [(l, n)] where [l] is a level and [n] is a
     message index.

     In a [Next_level] proof, [lower_history_proof] is the skip list cell for
     the level [l], [inc] is an inclusion proof of [lower_history_proof] into
     [snapshot] and [lower_message_proof] is a tree proof showing that there
     is no message at [(l, n)] with [lower_message_proof].

     The first message to read at the next level of [l] is the first input
     [Start_of_level]. *)
  | Next_level of {
      lower_message_proof : level_tree_proof;
      inc : inclusion_proof;
    }

let pp_proof fmt proof =
  match proof with
  | Single_level _ -> Format.fprintf fmt "Single_level inbox proof"
  | Next_level _ -> Format.fprintf fmt "Next_level inbox proof"

let proof_encoding =
  let open Data_encoding in
  union
    ~tag_size:`Uint8
    [
      case
        ~title:"Single_level"
        (Tag 0)
        (obj2
           (req "inclusion_proof" inclusion_proof_encoding)
           (req "message_proof" level_tree_proof_encoding))
        (function
          | Single_level {inc; message_proof} -> Some (inc, message_proof)
          | _ -> None)
        (fun (inc, message_proof) -> Single_level {inc; message_proof});
      case
        ~title:"Next_level"
        (Tag 1)
        (obj2
           (req "lower_message_proof" level_tree_proof_encoding)
           (req "inclusion_proof" inclusion_proof_encoding))
        (function
          | Next_level {lower_message_proof; inc} ->
              Some (lower_message_proof, inc)
          | _ -> None)
        (fun (lower_message_proof, inc) ->
          Next_level {lower_message_proof; inc});
    ]

let of_serialized_proof = Data_encoding.Binary.of_string_opt proof_encoding

let to_serialized_proof = Data_encoding.Binary.to_string_exn proof_encoding

(** [verify_level_tree_proof {proof; payload} head_cell_hash n label] handles
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
let verify_level_tree_proof {proof; payload} head_cell_hash n label =
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
      (Inbox_proof_error
         (Format.sprintf "message_proof (%s) does not match history" label))
  in
  let max_index =
    Sc_rollup_inbox_merkelized_payload_hashes_repr.get_index head_cell
  in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3975
     We could check that index = snapshot_max_index + 1 *)
  if Compare.Z.(n > max_index) then
    (* [n] is superior to the index of [head_cell] then the provided [payload]
       must be empty (,and [payload_cell = head_cell]) *)
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
  else
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
           (Format.sprintf
              "found index in message_proof (%s) is incorrect"
              label))
    in
    return_some payload

(** [produce_level_tree_proof get_level_tree_history head_cell_hash ~index]

    [get_level_tree_history cell_hash] is a function that returns an
    {!Sc_rollup_inbox_merkelized_payload_hashes_repr.History.t}. The returned
    history must contains the cell with hash [cell_hash], all its ancestor cell
    and their associated payload.

    [head_cell] the latest cell of the [level_tree] we want to produce a proof on
    with hash [head_cell_hash].

    This function produce either:

    - if [index <= head_cell_max_index], a proof that [payload_cell] with
    [index] is an ancestor to [head_cell] where [head_cell] is the cell with
    hash [head_cell_hash]. It returns the proof and the payload associated to
    [payload_cell];

   - else a proof that [index] is out of bound for [head_cell]. It returns the
   proof and no payload.
*)
let produce_level_tree_proof get_level_tree_history head_cell_hash ~index =
  let open Lwt_result_syntax in
  (* We first retrieve the history of cells for this level. *)
  let*! level_tree_history = get_level_tree_history head_cell_hash in
  (* We then fetch the actual head cell in the history. *)
  let*? head_cell =
    match
      Sc_rollup_inbox_merkelized_payload_hashes_repr.History.find
        head_cell_hash
        level_tree_history
    with
    | Some {merkelized = head_cell; payload = _} -> ok head_cell
    | None ->
        error
          (Inbox_proof_error
             "could not find head_cell in the level_tree_history")
  in
  let head_cell_max_index =
    Sc_rollup_inbox_merkelized_payload_hashes_repr.get_index head_cell
  in
  (* if [index <= level_tree_max_index] then the index belongs to this level, we
     prove its existence. Else the index is out of bounds, we prove its
     non-existence. *)
  let target_index = Compare.Z.(min index head_cell_max_index) in
  (* We look for the cell at `target_index` starting from `head_cell`. If it
     exists, we return the payload held in this cell. Otherwise, we prove that
     [index] does not exist in this level. *)
  let proof =
    Sc_rollup_inbox_merkelized_payload_hashes_repr.produce_proof
      level_tree_history
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
    | [] -> error (Inbox_proof_error "inclusion proof is empty")
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

let verify_proof (l, n) inbox_snapshot proof =
  assert (Z.(geq n zero)) ;
  let open Result_syntax in
  match proof with
  | Single_level {inc; message_proof} -> (
      let* history_proof = verify_inclusion_proof inc inbox_snapshot in
      let level_proof = Skip_list.content history_proof in
      let* payload_opt =
        verify_level_tree_proof message_proof level_proof.hash n "single level"
      in
      match payload_opt with
      | None ->
          if equal_history_proof inbox_snapshot history_proof then return_none
          else
            tzfail
            @@ Inbox_proof_error "payload is None but proof.level not top level"
      | Some payload ->
          return_some
            Sc_rollup_PVM_sig.{inbox_level = l; message_counter = n; payload})
  | Next_level {inc; lower_message_proof} -> (
      let* lower_history_proof = verify_inclusion_proof inc inbox_snapshot in
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/3975
         We could prove that the last message to read is SOL, and is
         before [n]. *)
      let lower_level_proof = Skip_list.content lower_history_proof in
      let* should_be_none =
        verify_level_tree_proof
          lower_message_proof
          lower_level_proof.hash
          n
          "lower"
      in
      match should_be_none with
      | None ->
          let* payload =
            Sc_rollup_inbox_message_repr.(serialize (Internal Start_of_level))
          in
          let inbox_level = Raw_level_repr.succ l in
          let message_counter = Z.zero in
          return_some Sc_rollup_PVM_sig.{inbox_level; message_counter; payload}
      | Some _ ->
          tzfail (Inbox_proof_error "more messages to read in current level"))

let produce_proof ~get_level_tree_history history inbox_snapshot (l, n) =
  let open Lwt_result_syntax in
  let deref ptr = History.find ptr history in
  let compare {hash = _; level} = Raw_level_repr.compare level l in
  let result = Skip_list.search ~deref ~compare ~cell:inbox_snapshot in
  let* inc, history_proof =
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
  in
  let level_proof = Skip_list.content history_proof in
  let* ({payload; proof = _} as message_proof) =
    produce_level_tree_proof get_level_tree_history level_proof.hash ~index:n
  in
  match payload with
  | Some payload ->
      return
        ( Single_level {inc; message_proof},
          Some Sc_rollup_PVM_sig.{inbox_level = l; message_counter = n; payload}
        )
  | None ->
      (* No payload means that there is no more message to read at the level of
         [history_proof]. *)
      if equal_history_proof inbox_snapshot history_proof then
        (* if [history_proof] is equal to the snapshot then it means that there
           is no more message to read. *)
        return (Single_level {inc; message_proof}, None)
      else
        (* Else we must read the [sol] of the next level. *)
        let lower_message_proof = message_proof in
        let* input_given =
          let inbox_level = Raw_level_repr.succ l in
          let message_counter = Z.zero in
          let*? payload =
            Sc_rollup_inbox_message_repr.(serialize (Internal Start_of_level))
          in
          return_some Sc_rollup_PVM_sig.{inbox_level; message_counter; payload}
        in
        return (Next_level {inc; lower_message_proof}, input_given)

let empty level =
  let pre_genesis_level = Raw_level_repr.root in
  let initial_level_proof =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/4242

       Because there is no empty level with `sol/eol` we don't need the zero
       hash but instead we could use the real value. *)
    let hash = Sc_rollup_inbox_merkelized_payload_hashes_repr.Hash.zero in
    {hash; level = pre_genesis_level}
  in
  {
    level;
    nb_messages_in_commitment_period = 0L;
    current_level_proof = (fun () -> initial_level_proof);
    old_levels_messages = Skip_list.genesis initial_level_proof;
  }

module Internal_for_tests = struct
  let eq_tree = Sc_rollup_inbox_merkelized_payload_hashes_repr.equal

  let produce_inclusion_proof history a b =
    let open Result_syntax in
    let cell_ptr = hash_history_proof b in
    let target_index = Skip_list.index a in
    let* history = History.remember cell_ptr b history in
    let deref ptr = History.find ptr history in
    Skip_list.back_path ~deref ~cell_ptr ~target_index
    |> Option.map (lift_ptr_path deref)
    |> Option.join |> return

  let serialized_proof_of_string x = x
end

type inbox = t
