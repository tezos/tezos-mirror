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

   To solve (2), we maintain a separate proof tree C witnessing the
   contents of messages of the current level.

   The protocol maintains the hashes of the head of H, and the root hash of C.

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

  let of_context_hash context_hash =
    Context_hash.to_bytes context_hash |> of_bytes_exn

  let to_context_hash hash = to_bytes hash |> Context_hash.of_bytes_exn

  include Path_encoding.Make_hex (H)
end

module Skip_list_parameters = struct
  let basis = 2
end

module Skip_list = Skip_list_repr.Make (Skip_list_parameters)

module V1 = struct
  type level_proof = {hash : Hash.t; level : Raw_level_repr.t}

  let level_proof_encoding =
    let open Data_encoding in
    conv
      (fun {hash; level} -> (hash, level))
      (fun (hash, level) -> {hash; level})
      (obj2 (req "hash" Hash.encoding) (req "level" Raw_level_repr.encoding))

  let equal_level_proof {hash; level} level_proof_2 =
    Hash.equal hash level_proof_2.hash
    && Raw_level_repr.equal level level_proof_2.level

  type history_proof = (level_proof, Hash.t) Skip_list.cell

  let hash_history_proof cell =
    let {hash; level} = Skip_list.content cell in
    let back_pointers_hashes = Skip_list.back_pointers cell in
    Hash.to_bytes hash
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
      Hash.pp
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
   - [message_counter] : the number of messages in the [level]'s inbox ;
     the number of messages that have not been consumed by a commitment cementing ;
   - [nb_messages_in_commitment_period] :
     the number of messages during the commitment period ;
   - [current_level_proof] : the [current_level] and its root hash ;
   - [old_levels_messages] : a witness of the inbox history.

   When new messages are appended to the current level inbox, the
   metadata stored in the context may be related to an older level.
   In that situation, an archival process is applied to the metadata.
   This process saves the [current_level_proof] in the
   [old_levels_messages] and empties [current_level]. It then
   initialises a new level tree for the new messages---note that any
   intermediate levels are simply skipped. See
   {!Make_hashing_scheme.archive_if_needed} for details.

  *)
  type t = {
    level : Raw_level_repr.t;
    nb_messages_in_commitment_period : int64;
    message_counter : Z.t;
    (* Lazy to avoid hashing O(n^2) time in [add_messages] *)
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
      message_counter;
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
    && Z.equal message_counter inbox2.message_counter
    && equal_level_proof
         (current_level_proof ())
         (inbox2.current_level_proof ())
    && equal_history_proof old_levels_messages inbox2.old_levels_messages

  let pp fmt
      {
        level;
        nb_messages_in_commitment_period;
        message_counter;
        current_level_proof;
        old_levels_messages;
      } =
    Format.fprintf
      fmt
      "@[<hov 2>{ level = %a@;\
       current messages hash  = %a@;\
       nb_messages_in_commitment_period = %s@;\
       message_counter = %a@;\
       old_levels_messages = %a@;\
       }@]"
      Raw_level_repr.pp
      level
      pp_level_proof
      (current_level_proof ())
      (Int64.to_string nb_messages_in_commitment_period)
      Z.pp_print
      message_counter
      pp_history_proof
      old_levels_messages

  let inbox_level inbox = inbox.level

  let inbox_message_counter inbox = inbox.message_counter

  let old_levels_messages inbox = inbox.old_levels_messages

  let current_level_proof inbox = inbox.current_level_proof ()

  let encoding =
    Data_encoding.(
      conv
        (fun {
               message_counter;
               nb_messages_in_commitment_period;
               level;
               current_level_proof;
               old_levels_messages;
             } ->
          ( message_counter,
            nb_messages_in_commitment_period,
            level,
            current_level_proof (),
            old_levels_messages ))
        (fun ( message_counter,
               nb_messages_in_commitment_period,
               level,
               current_level_proof,
               old_levels_messages ) ->
          {
            message_counter;
            nb_messages_in_commitment_period;
            level;
            current_level_proof = (fun () -> current_level_proof);
            old_levels_messages;
          })
        (obj5
           (req "message_counter" n)
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

let key_of_message ix =
  ["message"; Data_encoding.Binary.to_string_exn Data_encoding.n ix]

let number_of_messages_key = ["number_of_messages"]

type serialized_proof = string

let serialized_proof_encoding = Data_encoding.(string' Hex)

type inbox = t
