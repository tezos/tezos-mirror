(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type header = {
  block_hash : Block_hash.t;
  level : int32;
  predecessor : Block_hash.t;
  commitment_hash : Commitment.Hash.t option;
  previous_commitment_hash : Commitment.Hash.t;
  context : Smart_rollup_context_hash.t;
  inbox_witness :
    Tezos_crypto.Hashed.Smart_rollup_merkelized_payload_hashes_hash.t;
  inbox_hash : Inbox.Hash.t;
}

type content = {
  inbox : Inbox.t;
  messages : string list;
  commitment : Commitment.t option;
  outbox : (int * Outbox_message.summary) list option;
}

type ('header, 'content) block = {
  header : 'header;
  content : 'content;
  initial_tick : Z.t;
  num_ticks : int64;
}

type t = (header, unit) block

type full = (header, content) block

let commitment_hash_opt_encoding =
  let open Data_encoding in
  let binary =
    conv
      (Option.value ~default:Commitment.Hash.zero)
      (fun h -> if Commitment.Hash.(h = zero) then None else Some h)
      Commitment.Hash.encoding
  in
  let json = option Commitment.Hash.encoding in
  splitted ~binary ~json

let header_encoding =
  let open Data_encoding in
  conv
    (fun {
           block_hash;
           level;
           predecessor;
           commitment_hash;
           previous_commitment_hash;
           context;
           inbox_witness;
           inbox_hash;
         }
       ->
      ( block_hash,
        level,
        predecessor,
        commitment_hash,
        previous_commitment_hash,
        context,
        inbox_witness,
        inbox_hash ))
    (fun ( block_hash,
           level,
           predecessor,
           commitment_hash,
           previous_commitment_hash,
           context,
           inbox_witness,
           inbox_hash )
       ->
      {
        block_hash;
        level;
        predecessor;
        commitment_hash;
        previous_commitment_hash;
        context;
        inbox_witness;
        inbox_hash;
      })
  @@ obj8
       (req "block_hash" Block_hash.encoding ~description:"Tezos block hash.")
       (req
          "level"
          int32
          ~description:
            "Level of the block, corresponds to the level of the tezos block.")
       (req
          "predecessor"
          Block_hash.encoding
          ~description:"Predecessor hash of the Tezos block.")
       (req
          "commitment_hash"
          commitment_hash_opt_encoding
          ~description:
            "Hash of this block's commitment if any was computed for it.")
       (req
          "previous_commitment_hash"
          Commitment.Hash.encoding
          ~description:
            "Previous commitment hash in the chain. If there is a commitment \
             for this block, this field contains the commitment that was \
             previously computed.")
       (req
          "context"
          Smart_rollup_context_hash.encoding
          ~description:"Hash of the layer 2 context for this block.")
       (req
          "inbox_witness"
          Tezos_crypto.Hashed.Smart_rollup_merkelized_payload_hashes_hash
          .encoding
          ~description:
            "Witness for the inbox for this block, i.e. the Merkle hash of \
             payloads of messages.")
       (req
          "inbox_hash"
          Inbox.Hash.encoding
          ~description:"Hash of the inbox for this block.")

let header_size =
  WithExceptions.Option.get ~loc:__LOC__
  @@ Data_encoding.Binary.fixed_length header_encoding

let content_encoding =
  let open Data_encoding in
  conv
    (fun {inbox; messages; commitment; outbox} ->
      (inbox, messages, commitment, outbox))
    (fun (inbox, messages, commitment, outbox) ->
      {inbox; messages; commitment; outbox})
  @@ obj4
       (req "inbox" Inbox.encoding ~description:"Inbox for this block.")
       (req
          "messages"
          (list (dynamic_size (Variable.string' Hex)))
          ~description:"Messages added to the inbox in this block.")
       (opt
          "commitment"
          Commitment.encoding
          ~description:"Commitment, if any is computed for this block.")
       (opt
          "outbox"
          (list
             (merge_objs
                (obj1 (req "message_index" int31))
                Outbox_message.summary_encoding))
          ~description:"Outbox messages produced by PVM execution of inbox")

let block_encoding header_encoding content_encoding =
  let open Data_encoding in
  conv
    (fun {header; content; initial_tick; num_ticks} ->
      (header, (content, (initial_tick, num_ticks))))
    (fun (header, (content, (initial_tick, num_ticks))) ->
      {header; content; initial_tick; num_ticks})
  @@ merge_objs header_encoding
  @@ merge_objs content_encoding
  @@ obj2
       (req
          "initial_tick"
          Data_encoding.n
          ~description:
            "Initial tick of the PVM at this block, i.e. before evaluation of \
             the messages.")
       (req
          "num_ticks"
          int64
          ~description:
            "Number of ticks produced by the evaluation of the messages in \
             this block.")

let encoding = block_encoding header_encoding Data_encoding.unit

let full_encoding = block_encoding header_encoding content_encoding

let most_recent_commitment (header : header) =
  Option.value header.commitment_hash ~default:header.previous_commitment_hash

let final_tick {initial_tick; num_ticks; _} =
  Z.max Z.zero (Z.add initial_tick (Z.of_int64 num_ticks))
