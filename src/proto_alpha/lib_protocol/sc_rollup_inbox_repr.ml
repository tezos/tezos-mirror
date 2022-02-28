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

   A Merkelized inbox represents a list of available messages. This
   list is decomposed into sublist of messages, one for each Tezos
   level greater than the level of the Last Finalized Commitment
   (LFC).

   This module is designed to:

   1. give a constant-time access to the number of available messages
   ;

   2. provide a space-efficient representation for proofs of inbox
   inclusions (only for inboxes obtained at the end of block
   validation) ;

   3. offer an efficient function to add a new batch of messages in
   the inbox at the current level.

   To solve (1), we simply maintain the number of available messages
   in a field.

   To solve (2), we use a proof tree H which is implemented by a merkelized
   skip list allowing for compact inclusion proofs
   (See {!skip_list_repr.ml}).

   To solve (3), we maintain a separate proof tree C witnessing the
   contents of messages of the current level.

   The protocol maintains the number of available messages, the
   hashes of the head of H, and the root hash of C.

   The rollup node needs to maintain a full representation for C and a
   partial representation for H back to the level of the LFC.

*)
type error += Invalid_level_add_messages of Raw_level_repr.t

type error += Invalid_number_of_messages_to_consume of int64

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"sc_rollup_inbox.invalid_level_add_messages"
    ~title:"Internal error: Trying to add an message to a previous level inbox"
    ~description:
      "An inbox can only accept messages for its current level or for the next \
       levels."
    (obj1 (req "level" Raw_level_repr.encoding))
    (function Invalid_level_add_messages level -> Some level | _ -> None)
    (fun level -> Invalid_level_add_messages level) ;

  register_error_kind
    `Permanent
    ~id:"sc_rollup_inbox.consume_n_messages"
    ~title:"Internal error: Trying to consume a negative number of messages"
    ~description:
      "Sc_rollup_inbox.consume_n_messages must be called with a non negative \
       integer."
    (obj1 (req "n" int64))
    (function Invalid_number_of_messages_to_consume n -> Some n | _ -> None)
    (fun n -> Invalid_number_of_messages_to_consume n)

type t = {
  rollup : Sc_rollup_repr.t;
  level : Raw_level_repr.t;
  nb_available_messages : int64;
  message_counter : Z.t;
  current_messages_hash : Context.Proof.hash;
}

let pp fmt inbox =
  Format.fprintf
    fmt
    {|
         rollup = %a
         level = %a
         current messages hash  = %a
         nb_available_messages = %s
         message_counter = %a
    |}
    Sc_rollup_repr.Address.pp
    inbox.rollup
    Raw_level_repr.pp
    inbox.level
    Context_hash.pp
    inbox.current_messages_hash
    (Int64.to_string inbox.nb_available_messages)
    Z.pp_print
    inbox.message_counter

let encoding =
  Data_encoding.(
    conv
      (fun {
             rollup;
             message_counter;
             nb_available_messages;
             level;
             current_messages_hash;
           } ->
        ( rollup,
          message_counter,
          nb_available_messages,
          level,
          current_messages_hash ))
      (fun ( rollup,
             message_counter,
             nb_available_messages,
             level,
             current_messages_hash ) ->
        {
          rollup;
          message_counter;
          nb_available_messages;
          level;
          current_messages_hash;
        })
      (obj5
         (req "rollup" Sc_rollup_repr.encoding)
         (req "message_counter" n)
         (req "nb_available_messages" int64)
         (req "level" Raw_level_repr.encoding)
         (req "hash" Context_hash.encoding)))

let number_of_available_messages inbox = Z.of_int64 inbox.nb_available_messages

let empty rollup level =
  {
    rollup;
    level;
    message_counter = Z.zero;
    nb_available_messages = 0L;
    current_messages_hash = Context_hash.hash_bytes [Bytes.empty];
  }

let consume_n_messages n ({nb_available_messages; _} as inbox) :
    t option tzresult =
  if Compare.Int.(n < 0) then
    error (Invalid_number_of_messages_to_consume (Int64.of_int n))
  else if Compare.Int64.(Int64.of_int n > nb_available_messages) then ok None
  else
    let nb_available_messages = Int64.(sub nb_available_messages (of_int n)) in
    ok (Some {inbox with nb_available_messages})

let key_of_message = Data_encoding.Binary.to_string_exn Data_encoding.z

module type MerkelizedOperations = sig
  type tree

  type messages = tree

  type message = tree

  val add_messages :
    t -> Raw_level_repr.t -> string list -> messages -> (messages * t) Lwt.t

  val get_message : messages -> Z.t -> message option Lwt.t

  val get_message_payload : messages -> Z.t -> string option Lwt.t
end

module MakeHashingScheme
    (Tree : Context.TREE with type key = string list and type value = bytes) :
  MerkelizedOperations with type tree = Tree.tree = struct
  module Tree = Tree

  type tree = Tree.tree

  type messages = tree

  type message = tree

  let add_message inbox payload messages =
    let message_index = inbox.message_counter in
    let message_counter = Z.succ inbox.message_counter in
    let key = key_of_message message_index in
    let nb_available_messages = Int64.succ inbox.nb_available_messages in
    Tree.(add messages [key; "payload"] (Bytes.of_string payload))
    >>= fun messages ->
    let inbox = {inbox with message_counter; nb_available_messages} in
    Lwt.return (messages, inbox)

  let get_message messages message_index =
    let key = key_of_message message_index in
    Tree.(find_tree messages [key])

  let get_message_payload messages message_index =
    let key = key_of_message message_index in
    Tree.(find messages [key; "payload"]) >|= Option.map Bytes.to_string

  let add_messages inbox level payloads messages =
    let inbox =
      if Raw_level_repr.(level > inbox.level) then
        {inbox with level; message_counter = Z.zero}
      else inbox
    in
    List.fold_left_s
      (fun (messages, inbox) payload -> add_message inbox payload messages)
      (messages, inbox)
      payloads
    >>= fun (messages, inbox) ->
    let current_messages_hash = Tree.hash messages in
    Lwt.return (messages, {inbox with current_messages_hash})
end

include (
  MakeHashingScheme (struct
    include Context.Tree

    type t = Context.t

    type tree = Context.tree

    type value = bytes

    type key = string list
  end) :
    MerkelizedOperations with type tree = Context.tree)
