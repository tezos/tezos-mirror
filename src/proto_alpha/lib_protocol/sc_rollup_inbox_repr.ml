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
   level greater than the level of the Last Cemented Commitment
   (LCC).

   This module is designed to:

   1. give a constant-time access to the number of available messages ;

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
   partial representation for H back to the level of the LCC.

*)
type error += Invalid_level_add_messages of Raw_level_repr.t

type error += Invalid_number_of_messages_to_consume of int64

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
    ~id:"sc_rollup_inbox.consume_n_messages"
    ~title:"Internal error: Trying to consume a negative number of messages"
    ~description:
      "Sc_rollup_inbox.consume_n_messages must be called with a non negative \
       integer."
    (obj1 (req "consume_n_messages" int64))
    (function Invalid_number_of_messages_to_consume n -> Some n | _ -> None)
    (fun n -> Invalid_number_of_messages_to_consume n)

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
    hash_bytes [Context_hash.to_bytes context_hash]

  include Path_encoding.Make_hex (H)
end

module Skip_list_parameters = struct
  let basis = 2
end

module Skip_list = Skip_list_repr.Make (Skip_list_parameters)

type proof_hash = Hash.t

type history_proof_hash = Hash.t

type history_proof = (proof_hash, history_proof_hash) Skip_list.cell

let equal_history_proof = Skip_list.equal Hash.equal Hash.equal

let history_proof_encoding : history_proof Data_encoding.t =
  Skip_list.encoding Hash.encoding Hash.encoding

let pp_history_proof fmt cell =
  Format.fprintf
    fmt
    {|
       content = %a
       index = %d
       back_pointers = %a
    |}
    Hash.pp
    (Skip_list.content cell)
    (Skip_list.index cell)
    (Format.pp_print_list Hash.pp)
    (Skip_list.back_pointers cell)

module V1 = struct
  (*

   At a given level, an inbox is composed of metadata of type [t] and
   [current_messages], a [tree] representing the messages of the current level
   (held by the [Raw_context.t] in the protocol).

   The metadata contains :
   - [rollup] : the address of the rollup ;
   - [level] : the inbox level ;
   - [message_counter] : the number of messages in the [level]'s inbox ;
   - [nb_available_messages] :
     the number of messages that have not been consumed by a commitment cementing ;
   - [nb_messages_in_commitment_period] :
     the number of messages during the commitment period ;
   - [starting_level_of_current_commitment_period] :
     the level marking the beginning of the current commitment period ;
   - [current_messages_hash] : the root hash of [current_messages] ;
   - [old_levels_messages] : a witness of the inbox history.

   When new messages are appended to the current level inbox, the
   metadata stored in the context may be related to an older level.
   In that situation, an archival process is applied to the metadata.
   This process saves the [current_messages_hash] in the
   [old_levels_messages] and empties [current_messages]. If
   there are intermediate levels between [inbox.level] and the current
   level, this archival process is applied until we reach the current
   level using an empty [current_messages]. See {!MakeHashingScheme.archive}
   for details.

   The [current_messages_hash] is either:
   - the hash of 'empty bytes' when there are no current messages ;
   - the root hash of the tree, where the contents of each message sit at the
     key [[message_index, "payload"]], where [message_index] is the index of the
     message in the list of [current_messages], if there are one or more
     messages.

*)
  type t = {
    rollup : Sc_rollup_repr.t;
    level : Raw_level_repr.t;
    nb_available_messages : int64;
    nb_messages_in_commitment_period : int64;
    starting_level_of_current_commitment_period : Raw_level_repr.t;
    message_counter : Z.t;
    (* Lazy to avoid hashing O(n^2) time in [add_external_messages] *)
    current_messages_hash : unit -> Hash.t;
    old_levels_messages : history_proof;
  }

  let equal inbox1 inbox2 =
    (* To be robust to addition of fields in [t]. *)
    let {
      rollup;
      level;
      nb_available_messages;
      nb_messages_in_commitment_period;
      starting_level_of_current_commitment_period;
      message_counter;
      current_messages_hash;
      old_levels_messages;
    } =
      inbox1
    in
    Sc_rollup_repr.Address.equal rollup inbox2.rollup
    && Raw_level_repr.equal level inbox2.level
    && Compare.Int64.(equal nb_available_messages inbox2.nb_available_messages)
    && Compare.Int64.(
         equal
           nb_messages_in_commitment_period
           inbox2.nb_messages_in_commitment_period)
    && Raw_level_repr.(
         equal
           starting_level_of_current_commitment_period
           inbox2.starting_level_of_current_commitment_period)
    && Z.equal message_counter inbox2.message_counter
    && Hash.equal (current_messages_hash ()) (inbox2.current_messages_hash ())
    && equal_history_proof old_levels_messages inbox2.old_levels_messages

  let pp fmt
      {
        rollup;
        level;
        nb_available_messages;
        nb_messages_in_commitment_period;
        starting_level_of_current_commitment_period;
        message_counter;
        current_messages_hash;
        old_levels_messages;
      } =
    Format.fprintf
      fmt
      {|
         rollup = %a
         level = %a
         current messages hash  = %a
         nb_available_messages = %Ld
         nb_messages_in_commitment_period = %s
         starting_level_of_current_commitment_period = %a
         message_counter = %a
         old_levels_messages = %a
    |}
      Sc_rollup_repr.Address.pp
      rollup
      Raw_level_repr.pp
      level
      Hash.pp
      (current_messages_hash ())
      nb_available_messages
      (Int64.to_string nb_messages_in_commitment_period)
      Raw_level_repr.pp
      starting_level_of_current_commitment_period
      Z.pp_print
      message_counter
      pp_history_proof
      old_levels_messages

  let inbox_level inbox = inbox.level

  let old_levels_messages_encoding =
    Skip_list.encoding Hash.encoding Hash.encoding

  let encoding =
    Data_encoding.(
      conv
        (fun {
               rollup;
               message_counter;
               nb_available_messages;
               nb_messages_in_commitment_period;
               starting_level_of_current_commitment_period;
               level;
               current_messages_hash;
               old_levels_messages;
             } ->
          ( rollup,
            message_counter,
            nb_available_messages,
            nb_messages_in_commitment_period,
            starting_level_of_current_commitment_period,
            level,
            current_messages_hash (),
            old_levels_messages ))
        (fun ( rollup,
               message_counter,
               nb_available_messages,
               nb_messages_in_commitment_period,
               starting_level_of_current_commitment_period,
               level,
               current_messages_hash,
               old_levels_messages ) ->
          {
            rollup;
            message_counter;
            nb_available_messages;
            nb_messages_in_commitment_period;
            starting_level_of_current_commitment_period;
            level;
            current_messages_hash = (fun () -> current_messages_hash);
            old_levels_messages;
          })
        (obj8
           (req "rollup" Sc_rollup_repr.encoding)
           (req "message_counter" n)
           (req "nb_available_messages" int64)
           (req "nb_messages_in_commitment_period" int64)
           (req
              "starting_level_of_current_commitment_period"
              Raw_level_repr.encoding)
           (req "level" Raw_level_repr.encoding)
           (req "current_messages_hash" Hash.encoding)
           (req "old_levels_messages" old_levels_messages_encoding)))

  let number_of_available_messages inbox =
    Z.of_int64 inbox.nb_available_messages

  let number_of_messages_during_commitment_period inbox =
    inbox.nb_messages_in_commitment_period

  let start_new_commitment_period inbox level =
    {
      inbox with
      starting_level_of_current_commitment_period = level;
      nb_messages_in_commitment_period = 0L;
    }

  let starting_level_of_current_commitment_period inbox =
    inbox.starting_level_of_current_commitment_period

  let no_messages_hash = Hash.hash_bytes [Bytes.empty]

  let empty rollup level =
    {
      rollup;
      level;
      message_counter = Z.zero;
      nb_available_messages = 0L;
      nb_messages_in_commitment_period = 0L;
      starting_level_of_current_commitment_period = level;
      current_messages_hash = (fun () -> no_messages_hash);
      old_levels_messages = Skip_list.genesis no_messages_hash;
    }

  let consume_n_messages n ({nb_available_messages; _} as inbox) :
      t option tzresult =
    let n = Int64.of_int32 n in
    if Compare.Int64.(n < 0L) then
      error (Invalid_number_of_messages_to_consume n)
    else if Compare.Int64.(n > nb_available_messages) then ok None
    else
      let nb_available_messages = Int64.(sub nb_available_messages n) in
      ok (Some {inbox with nb_available_messages})
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

let key_of_message = Data_encoding.Binary.to_string_exn Data_encoding.z

module type MerkelizedOperations = sig
  type tree

  type messages = tree

  type message = tree

  type history

  val history_encoding : history Data_encoding.t

  val pp_history : Format.formatter -> history -> unit

  val history_at_genesis : bound:int64 -> history

  val add_external_messages :
    history ->
    t ->
    Raw_level_repr.t ->
    string list ->
    messages ->
    (messages * history * t) tzresult Lwt.t

  val add_messages_no_history :
    t ->
    Raw_level_repr.t ->
    Sc_rollup_inbox_message_repr.serialized list ->
    messages ->
    (messages * t) tzresult Lwt.t

  val get_message : messages -> Z.t -> message option Lwt.t

  val get_message_payload : messages -> Z.t -> string option Lwt.t

  type inclusion_proof

  val inclusion_proof_encoding : inclusion_proof Data_encoding.t

  val pp_inclusion_proof : Format.formatter -> inclusion_proof -> unit

  val number_of_proof_steps : inclusion_proof -> int

  val produce_inclusion_proof : history -> t -> t -> inclusion_proof option

  val verify_inclusion_proof : inclusion_proof -> t -> t -> bool
end

module type TREE = sig
  type t

  type tree

  type key = string list

  type value = bytes

  val find : tree -> key -> value option Lwt.t

  val find_tree : tree -> key -> tree option Lwt.t

  val add : tree -> key -> value -> tree Lwt.t

  val is_empty : tree -> bool

  val hash : tree -> Context_hash.t
end

module MakeHashingScheme (Tree : TREE) :
  MerkelizedOperations with type tree = Tree.tree = struct
  module Tree = Tree

  type tree = Tree.tree

  type messages = tree

  type message = tree

  let add_message inbox payload messages =
    let open Lwt_tzresult_syntax in
    let message_index = inbox.message_counter in
    let message_counter = Z.succ message_index in
    let key = key_of_message message_index in
    let nb_available_messages = Int64.succ inbox.nb_available_messages in
    let*! messages =
      Tree.add
        messages
        [key; "payload"]
        (Bytes.of_string
           (payload : Sc_rollup_inbox_message_repr.serialized :> string))
    in
    let nb_messages_in_commitment_period =
      Int64.succ inbox.nb_messages_in_commitment_period
    in
    let inbox =
      {
        inbox with
        message_counter;
        nb_available_messages;
        nb_messages_in_commitment_period;
      }
    in
    return (messages, inbox)

  let get_message messages message_index =
    let key = key_of_message message_index in
    Tree.(find_tree messages [key])

  let get_message_payload messages message_index =
    let key = key_of_message message_index in
    Tree.(find messages [key; "payload"]) >|= Option.map Bytes.to_string

  let hash_old_levels_messages cell =
    let current_messages_hash = Skip_list.content cell in
    let back_pointers_hashes = Skip_list.back_pointers cell in
    Hash.to_bytes current_messages_hash
    :: List.map Hash.to_bytes back_pointers_hashes
    |> Hash.hash_bytes

  module Int64_map = Map.Make (Int64)

  type history = {
    events : history_proof Hash.Map.t;
    sequence : Hash.t Int64_map.t;
    bound : int64;
    counter : int64;
  }

  let history_encoding : history Data_encoding.t =
    let open Data_encoding in
    let events_encoding = Hash.Map.encoding history_proof_encoding in
    let sequence_encoding =
      conv
        (fun m -> Int64_map.bindings m)
        (List.fold_left (fun m (k, v) -> Int64_map.add k v m) Int64_map.empty)
        (list (tup2 int64 Hash.encoding))
    in
    conv
      (fun {events; sequence; bound; counter} ->
        (events, sequence, bound, counter))
      (fun (events, sequence, bound, counter) ->
        {events; sequence; bound; counter})
      (obj4
         (req "events" events_encoding)
         (req "sequence" sequence_encoding)
         (req "bound" int64)
         (req "counter" int64))

  let pp_history fmt history =
    Hash.Map.bindings history.events |> fun bindings ->
    let pp_binding fmt (hash, history_proof) =
      Format.fprintf
        fmt
        "@[%a -> %a@]"
        Hash.pp
        hash
        pp_history_proof
        history_proof
    in
    Format.pp_print_list pp_binding fmt bindings

  let history_at_genesis ~bound =
    {events = Hash.Map.empty; sequence = Int64_map.empty; bound; counter = 0L}

  type without_history_witness

  type with_history_witness

  type _ with_history =
    | No_history : without_history_witness with_history
    | With_history : history -> with_history_witness with_history

  (** [remember_history ptr cell history] extends [history] with a new
      mapping from [ptr] to [cell]. If [history] is full, the
      oldest mapping is removed. If the history bound is less
      or equal to zero, then this function returns [history]
      untouched. *)
  let remember_history ptr cell history =
    if Compare.Int64.(history.bound <= 0L) then history
    else
      let events = Hash.Map.add ptr cell history.events in
      let counter = Int64.succ history.counter in
      let history =
        {
          events;
          sequence = Int64_map.add history.counter ptr history.sequence;
          bound = history.bound;
          counter;
        }
      in
      if Int64.(equal history.counter history.bound) then
        match Int64_map.min_binding history.sequence with
        | None -> history
        | Some (l, h) ->
            let sequence = Int64_map.remove l history.sequence in
            let events = Hash.Map.remove h events in
            {
              counter = Int64.pred history.counter;
              bound = history.bound;
              sequence;
              events;
            }
      else history

  let remember :
      type history_witness.
      history_proof_hash ->
      history_proof ->
      history_witness with_history ->
      history_witness with_history =
   fun ptr cell history ->
    match history with
    | No_history -> No_history
    | With_history history -> With_history (remember_history ptr cell history)

  let archive_if_needed history inbox target_level =
    let archive_level history inbox =
      let prev_cell = inbox.old_levels_messages in
      let prev_cell_ptr = hash_old_levels_messages prev_cell in
      let history = remember prev_cell_ptr prev_cell history in
      let old_levels_messages =
        Skip_list.next
          ~prev_cell
          ~prev_cell_ptr
          (inbox.current_messages_hash ())
      in
      let level = Raw_level_repr.succ inbox.level in
      let current_messages_hash () = no_messages_hash in
      let inbox =
        {
          rollup = inbox.rollup;
          nb_available_messages = inbox.nb_available_messages;
          nb_messages_in_commitment_period =
            inbox.nb_messages_in_commitment_period;
          starting_level_of_current_commitment_period =
            inbox.starting_level_of_current_commitment_period;
          old_levels_messages;
          level;
          current_messages_hash;
          message_counter = Z.zero;
        }
      in
      (history, inbox)
    in
    let rec aux (history, inbox) =
      if Raw_level_repr.(inbox.level = target_level) then (history, inbox)
      else aux (archive_level history inbox)
    in
    aux (history, inbox)

  let hash_messages messages =
    if Tree.is_empty messages then no_messages_hash
    else Hash.of_context_hash @@ Tree.hash messages

  let add_messages_aux history inbox level payloads messages =
    let open Lwt_tzresult_syntax in
    let* () =
      fail_when
        Raw_level_repr.(level < inbox.level)
        (Invalid_level_add_messages level)
    in
    let history, inbox = archive_if_needed history inbox level in
    let* messages, inbox =
      List.fold_left_es
        (fun (messages, inbox) payload -> add_message inbox payload messages)
        (messages, inbox)
        payloads
    in
    let current_messages_hash () = hash_messages messages in
    return (messages, history, {inbox with current_messages_hash})

  let add_external_messages history inbox level payloads messages =
    let open Lwt_tzresult_syntax in
    let*? payloads =
      List.map_e
        (fun payload ->
          Sc_rollup_inbox_message_repr.(to_bytes @@ External payload))
        payloads
    in
    let* messages, With_history history, inbox =
      add_messages_aux (With_history history) inbox level payloads messages
    in
    return (messages, history, inbox)

  let add_messages_no_history inbox level payloads messages =
    let open Lwt_tzresult_syntax in
    let* messages, No_history, inbox =
      add_messages_aux No_history inbox level payloads messages
    in
    return (messages, inbox)

  (* An [inclusion_proof] is a path in the Merkelized skip list
     showing that a given inbox history is a prefix of another one.
     This path has a size logarithmic in the difference between the
     levels of the two inboxes.

     [Irmin.Proof.{tree_proof, stream_proof}] could not be reused here
     because there is no obviously encoding of sequences in these data
     structures with the same guarantee about the size of proofs. *)
  type inclusion_proof = history_proof list

  let inclusion_proof_encoding =
    let open Data_encoding in
    list history_proof_encoding

  let pp_inclusion_proof fmt proof =
    Format.pp_print_list pp_history_proof fmt proof

  let number_of_proof_steps proof = List.length proof

  let lift_ptr_path history ptr_path =
    let rec aux accu = function
      | [] -> Some (List.rev accu)
      | x :: xs -> Option.bind (history x) @@ fun c -> aux (c :: accu) xs
    in
    aux [] ptr_path

  let produce_inclusion_proof history inbox1 inbox2 =
    let cell_ptr = hash_old_levels_messages inbox2.old_levels_messages in
    let target_index = Skip_list.index inbox1.old_levels_messages in
    let (With_history history) =
      remember cell_ptr inbox2.old_levels_messages (With_history history)
    in
    let deref ptr = Hash.Map.find_opt ptr history.events in
    Skip_list.back_path ~deref ~cell_ptr ~target_index
    |> Option.map (lift_ptr_path deref)
    |> Option.join

  let verify_inclusion_proof proof inbox1 inbox2 =
    let assoc = List.map (fun c -> (hash_old_levels_messages c, c)) proof in
    let path = List.split assoc |> fst in
    let deref =
      let open Hash.Map in
      let map = of_seq (List.to_seq assoc) in
      fun ptr -> find_opt ptr map
    in
    let cell_ptr = hash_old_levels_messages inbox2.old_levels_messages in
    let target_ptr = hash_old_levels_messages inbox1.old_levels_messages in
    Skip_list.valid_back_path
      ~equal_ptr:Hash.equal
      ~deref
      ~cell_ptr
      ~target_ptr
      path
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

type inbox = t

module Proof = struct
  type starting_point = {inbox_level : Raw_level_repr.t; message_counter : Z.t}

  type t = {
    skips : (inbox * inclusion_proof) list;
    (* The [skips] value in this record makes it potentially unbounded
       in size. There is an issue #2997 to deal with this problem. *)
    level : inbox;
    inc : inclusion_proof;
    message_proof : Context.Proof.tree Context.Proof.t;
  }

  let pp fmt proof =
    Format.fprintf fmt "Inbox proof with %d skips" (List.length proof.skips)

  let encoding =
    let open Data_encoding in
    conv
      (fun {skips; level; inc; message_proof} ->
        (skips, level, inc, message_proof))
      (fun (skips, level, inc, message_proof) ->
        {skips; level; inc; message_proof})
      (obj4
         (req "skips" (list (tup2 encoding inclusion_proof_encoding)))
         (req "level" encoding)
         (req "inc" inclusion_proof_encoding)
         (req
            "message_proof"
            Context.Proof_encoding.V1.Tree32.tree_proof_encoding))

  (* This function is for pattern matching on proofs based on whether
     they involve multiple levels or if they only concern a single
     level.

     [split_proof proof] is [None] in the case that [proof] is a
     'simple' inbox proof that only involves one level. In this case
     [skips] is empty and we just check the single [level], [inc]
     pair, and the [message_proof].

     [split_proof proof] is [Some (level, inc, remaining_proof)] if
     there are [skips]. In this case, we must check the [level] and
     [inc] given, and then continue (recursively) on to the
     [remaining_proof]. *)
  let split_proof proof =
    match proof.skips with
    | [] -> None
    | (level, inc) :: rest -> Some (level, inc, {proof with skips = rest})

  (* A proof might include several sub-inboxes as evidence of different
     levels being empty in the actual inbox snapshot. This returns the
     _lowest_ such sub-inbox for a given proof.

     It's used with the function above in the recursive case of [valid].
     When [split_proof proof] gives [Some (level, inc, remaining_proof)]
     we have to check that [inc] is an inclusion proof between [level]
     and [bottom_level remaining_proof]. *)
  let bottom_level proof =
    match proof.skips with [] -> proof.level | (level, _) :: _ -> level

  (* The [message_proof] part of an inbox proof is a
     [Context.tree_proof].

     To validate this, we need a function of type

       [tree -> (tree, result) Lwt.t].

     For a given [n], [message_payload n] is such a function: it takes a
     [Context.tree] representing the messages in a single level of the
     inbox and extracts the message payload at index [n], so [result] in
     this case is [string]. (It also returns the tree just to satisfy
     the function [Context.verify_tree_proof]). *)
  let message_payload n tree =
    let open Lwt_syntax in
    let* r = get_message_payload tree n in
    return (tree, r)

  let check_hash hash kinded_hash =
    match kinded_hash with
    | `Node h -> Hash.(equal (of_context_hash h) hash)
    | `Value h -> Hash.(equal (of_context_hash h) hash)

  type error += Inbox_proof_error of string

  let proof_error reason =
    let open Lwt_result_syntax in
    fail (Inbox_proof_error reason)

  let drop_error promise reason =
    let open Lwt_tzresult_syntax in
    let*! result = promise in
    match result with Ok r -> return r | Error _ -> proof_error reason

  let rec valid {inbox_level = l; message_counter = n} inbox proof =
    assert (Z.(geq n zero)) ;
    let open Lwt_result_syntax in
    match split_proof proof with
    | None ->
        if
          verify_inclusion_proof proof.inc proof.level inbox
          && Raw_level_repr.equal (inbox_level proof.level) l
          && check_hash
               (proof.level.current_messages_hash ())
               proof.message_proof.before
        then
          let* (_ : Context.tree), payload =
            drop_error
              (Context.verify_tree_proof
                 proof.message_proof
                 (message_payload n))
              "message_proof invalid"
          in
          match payload with
          | None ->
              if equal proof.level inbox then return None
              else proof_error "payload is None, inbox proof.level not top"
          | Some msg ->
              return
              @@ Some
                   Sc_rollup_PVM_sem.
                     {inbox_level = l; message_counter = n; payload = msg}
        else proof_error "Inbox proof parameters don't match (message level)"
    | Some (level, inc, remaining_proof) ->
        if
          verify_inclusion_proof inc level (bottom_level remaining_proof)
          && Raw_level_repr.equal (inbox_level level) l
          && Z.equal level.message_counter n
        then
          valid
            {inbox_level = Raw_level_repr.succ l; message_counter = Z.zero}
            inbox
            remaining_proof
        else proof_error "Inbox proof parameters don't match (lower level)"

  (* TODO #2997 This needs to be implemented when the inbox structure is
     improved. *)
  let produce_proof _ _ = assert false
end
