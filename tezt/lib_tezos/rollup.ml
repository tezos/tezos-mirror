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

open Runnable.Syntax

module Tx_rollup = struct
  type range = Empty of int | Interval of int * int

  type commitments_hashes = {message_hash : string; commitment_hash : string}

  let pp_range fmt = function
    | Empty x -> Format.fprintf fmt "next: %d" x
    | Interval (t, h) -> Format.fprintf fmt "oldest: %d newest: %d" t h

  let range_of_json json =
    if JSON.(json |-> "newest" |> is_null) then
      Empty JSON.(json |-> "next" |> as_int)
    else
      let tail = JSON.(json |-> "oldest" |> as_int) in
      let head = JSON.(json |-> "newest" |> as_int) in
      Interval (tail, head)

  let commitment_newest_hash_of_json json =
    let open JSON in
    if is_null json then None
    else
      Some
        {
          message_hash = json |-> "last_message_hash" |> as_string;
          commitment_hash = json |-> "commitment_hash" |> as_string;
        }

  type state = {
    finalized_commitments : range;
    unfinalized_commitments : range;
    uncommitted_inboxes : range;
    tezos_head_level : int option;
    commitment_newest_hash : string option;
    burn_per_byte : int;
    inbox_ema : int;
    last_removed_commitment_hashes : commitments_hashes option;
  }

  type inbox = {inbox_length : int; cumulated_size : int; merkle_root : string}

  type messages = {
    count : int;
    root : string;
    last_message_result_hash : string;
  }

  type commitment = {
    level : int;
    messages : messages;
    predecessor : string option;
    inbox_merkle_root : string;
  }

  type submitted_commitment = {
    commitment : commitment;
    commitment_hash : string;
    committer : string;
    submitted_at : int;
    finalized_at : int option;
  }

  type operation_content_payload = {
    qty : Int64.t;
    destination : string;
    ticket : string;
  }

  type l2_transfer = [`Transfer of operation_content_payload]

  type l2_withdraw = [`Withdraw of operation_content_payload]

  type operation_content = [l2_transfer | l2_withdraw]

  let operation_content_encoding : operation_content Data_encoding.t =
    let open Data_encoding in
    let payload_encoding =
      obj3
        (req "qty" int64)
        (req "destination" string)
        (req "ticket_hash" string)
    in
    union
      [
        case
          ~title:"transfer"
          (Tag 0)
          payload_encoding
          (function
            | `Transfer {qty; destination; ticket} ->
                Some (qty, destination, ticket)
            | _ -> None)
          (fun (qty, destination, ticket) ->
            `Transfer {qty; destination; ticket});
        case
          ~title:"withdraw"
          (Tag 1)
          payload_encoding
          (function
            | `Withdraw {qty; destination; ticket} ->
                Some (qty, destination, ticket)
            | _ -> None)
          (fun (qty, destination, ticket) ->
            `Withdraw {qty; destination; ticket});
      ]

  type operation = {
    signer : string;
    counter : int64 option;
    contents : operation_content list;
  }

  type deposit_content = {
    sender : string;
    destination : string;
    ticket_hash : string;
    amount : int64;
  }

  type deposit = [`Deposit of deposit_content]

  type batch = [`Batch of Hex.t]

  type message = [deposit | batch]

  let make_batch batch = `Batch (Hex.of_string batch)

  let make_deposit ~sender ~destination ~ticket_hash ~amount =
    `Deposit {sender; destination; ticket_hash; amount}

  let json_of_batch (`Hex message) = `O [("batch", `String message)]

  let json_of_deposit {sender; destination; ticket_hash; amount} =
    `O
      [
        ( "deposit",
          `O
            [
              ("sender", `String sender);
              ("destination", `String destination);
              ("ticket_hash", `String ticket_hash);
              ("amount", `String (Int64.to_string amount));
            ] );
      ]

  let json_of_message = function
    | `Batch batch -> json_of_batch batch
    | `Deposit deposit -> json_of_deposit deposit

  type withdraw = {claimer : string; ticket_hash : string; amount : int64}

  let json_of_withdraw {claimer; ticket_hash; amount} =
    `O
      [
        ("claimer", `String claimer);
        ("ticket_hash", `String ticket_hash);
        ("amount", `String (Int64.to_string amount));
      ]

  type ticket_dispatch_info = {
    contents : string;
    ty : string;
    ticketer : string;
    amount : int64;
    claimer : string;
  }

  let get_json_of_ticket_dispatch_info {contents; ty; ticketer; amount; claimer}
      client =
    let* contents_json = Client.convert_data_to_json ~data:contents client in
    let* ty_json = Client.convert_data_to_json ~data:ty client in
    return
      (`O
        [
          ("contents", contents_json);
          ("ty", ty_json);
          ("ticketer", `String ticketer);
          ("amount", `String (Int64.to_string amount));
          ("claimer", `String claimer);
        ])

  let get_state ?hooks ~rollup client =
    let parse json =
      let finalized_commitments =
        JSON.(json |-> "finalized_commitments" |> range_of_json)
      in
      let unfinalized_commitments =
        JSON.(json |-> "unfinalized_commitments" |> range_of_json)
      in
      let uncommitted_inboxes =
        JSON.(json |-> "uncommitted_inboxes" |> range_of_json)
      in
      let tezos_head_level = JSON.(json |-> "tezos_head_level" |> as_int_opt) in
      let commitment_newest_hash =
        JSON.(json |-> "commitment_newest_hash" |> as_string_opt)
      in
      let burn_per_byte = JSON.(json |-> "burn_per_byte" |> as_int) in
      let inbox_ema = JSON.(json |-> "inbox_ema" |> as_int) in
      let last_removed_commitment_hashes =
        JSON.(
          json |-> "last_removed_commitment_hashes"
          |> commitment_newest_hash_of_json)
      in
      {
        finalized_commitments;
        unfinalized_commitments;
        uncommitted_inboxes;
        tezos_head_level;
        commitment_newest_hash;
        burn_per_byte;
        inbox_ema;
        last_removed_commitment_hashes;
      }
    in
    RPC.Tx_rollup.get_state ?hooks ~rollup client |> Runnable.map parse

  let get_inbox ?hooks ~rollup ~level client =
    let parse json =
      if JSON.is_null json then None
      else
        let inbox_length = JSON.(json |-> "inbox_length" |> as_int) in
        let cumulated_size = JSON.(json |-> "cumulated_size" |> as_int) in
        let merkle_root = JSON.(json |-> "merkle_root" |> as_string) in
        Some {inbox_length; cumulated_size; merkle_root}
    in
    RPC.Tx_rollup.get_inbox ?hooks ~rollup ~level client |> Runnable.map parse

  let get_commitment ?hooks ?block ~rollup ~level client =
    let parse json =
      if JSON.is_null json then None
      else
        let commitment_json = JSON.(json |-> "commitment") in
        let level = JSON.(commitment_json |-> "level" |> as_int) in
        let messages_json = JSON.(commitment_json |-> "messages") in
        let count = JSON.(messages_json |-> "count" |> as_int) in
        let root = JSON.(messages_json |-> "root" |> as_string) in
        let last_message_result_hash =
          JSON.(messages_json |-> "last_message_result_hash" |> as_string)
        in
        let messages = {count; root; last_message_result_hash} in
        let predecessor =
          JSON.(commitment_json |-> "predecessor" |> as_string_opt)
        in
        let inbox_merkle_root =
          JSON.(commitment_json |-> "inbox_merkle_root" |> as_string)
        in
        let commitment = {level; messages; predecessor; inbox_merkle_root} in
        let commitment_hash = JSON.(json |-> "commitment_hash" |> as_string) in
        let committer = JSON.(json |-> "committer" |> as_string) in
        let submitted_at = JSON.(json |-> "submitted_at" |> as_int) in
        let finalized_at = JSON.(json |-> "finalized_at" |> as_int_opt) in
        Some
          {commitment; commitment_hash; committer; submitted_at; finalized_at}
    in
    RPC.Tx_rollup.get_commitment ?hooks ?block ~rollup ~level client
    |> Runnable.map parse

  let get_pending_bonded_commitments ?hooks ?block ~rollup ~pkh client =
    RPC.Tx_rollup.get_pending_bonded_commitments
      ?hooks
      ?block
      ~rollup
      ~pkh
      client

  let message_hash ?hooks ~message client =
    let parse json = `Hash JSON.(json |-> "hash" |> as_string) in
    let data : JSON.u = `O [("message", json_of_message message)] in
    RPC.Tx_rollup.Forge.Inbox.message_hash ?hooks ~data client
    |> Runnable.map parse

  let inbox_merkle_tree_hash ?hooks ~message_hashes client =
    let parse json = `Hash JSON.(json |-> "hash" |> as_string) in
    let make_message (`Hash message) : JSON.u = `String message in
    let data =
      `O [("message_hashes", `A (List.map make_message message_hashes))]
    in
    RPC.Tx_rollup.Forge.Inbox.merkle_tree_hash ?hooks ~data client
    |> Runnable.map parse

  let inbox_merkle_tree_path ?hooks ~message_hashes ~position client =
    let parse json = JSON.(json |-> "path") in
    let make_message (`Hash message) : JSON.u = `String message in
    let data =
      `O
        [
          ("message_hashes", `A (List.map make_message message_hashes));
          ("position", `Float (float_of_int position));
        ]
    in
    RPC.Tx_rollup.Forge.Inbox.merkle_tree_path ?hooks ~data client
    |> Runnable.map parse

  let commitment_merkle_tree_hash ?hooks ~message_result_hashes client =
    let parse json = `Hash JSON.(json |-> "hash" |> as_string) in
    let make_message (`Hash message) : JSON.u = `String message in
    let data =
      `O
        [
          ( "message_result_hashes",
            `A (List.map make_message message_result_hashes) );
        ]
    in
    let runnable =
      RPC.Tx_rollup.Forge.Commitment.merkle_tree_hash ?hooks ~data client
    in
    Runnable.map parse runnable

  let commitment_merkle_tree_path ?hooks ~message_result_hashes ~position client
      =
    let parse json = JSON.(json |-> "path") in
    let make_message (`Hash message) : JSON.u = `String message in
    let data =
      `O
        [
          ( "message_result_hashes",
            `A (List.map make_message message_result_hashes) );
          ("position", `Float (float_of_int position));
        ]
    in
    let runnable =
      RPC.Tx_rollup.Forge.Commitment.merkle_tree_path ?hooks ~data client
    in
    Runnable.map parse runnable

  let withdraw_list_hash ?hooks ~withdrawals client =
    let parse json = JSON.(json |-> "hash" |> as_string) in
    let data =
      `O [("withdraw_list", `A (List.map json_of_withdraw withdrawals))]
    in
    RPC.Tx_rollup.Forge.Withdraw.withdraw_list_hash ?hooks ~data client
    |> Runnable.map parse

  let message_result_hash ?hooks ~context_hash ~withdraw_list_hash client =
    let parse json = JSON.(json |-> "hash" |> as_string) in
    let data =
      `O
        [
          ("context_hash", `String context_hash);
          ("withdraw_list_hash", `String withdraw_list_hash);
        ]
    in
    RPC.Tx_rollup.Forge.Commitment.message_result_hash ?hooks ~data client
    |> Runnable.map parse

  let compute_inbox_from_messages ?hooks messages client =
    let* message_hashes =
      Lwt_list.map_p
        (fun message ->
          let*! message_hash = message_hash ?hooks ~message client in
          return message_hash)
        messages
    in
    let*! (`Hash merkle_root) =
      inbox_merkle_tree_hash ?hooks ~message_hashes client
    in
    return
      {
        inbox_length = List.length messages;
        cumulated_size =
          List.map
            (function
              | `Batch (`Hex message) ->
                  (* In the Hex represented as a string, a byte is
                     encoded using two characters. *)
                  String.length message / 2
              | `Deposit _ ->
                  let sender_pkh_size = 65 in
                  let destination_bls_pkh_size = 36 in
                  let ticket_hash_size = 32 in
                  let amount_size = 8 in
                  sender_pkh_size + destination_bls_pkh_size + ticket_hash_size
                  + amount_size)
            messages
          |> List.fold_left ( + ) 0;
        merkle_root;
      }

  module Check = struct
    let range : range Check.typ = Check.equalable pp_range ( = )

    let state : state Check.typ =
      let open Check in
      convert
        (fun {
               last_removed_commitment_hashes;
               finalized_commitments;
               unfinalized_commitments;
               uncommitted_inboxes;
               commitment_newest_hash;
               tezos_head_level;
               burn_per_byte;
               inbox_ema;
             } ->
          ( finalized_commitments,
            unfinalized_commitments,
            uncommitted_inboxes,
            tezos_head_level,
            commitment_newest_hash,
            burn_per_byte,
            inbox_ema,
            Option.map
              (function r -> (r.message_hash, r.commitment_hash))
              last_removed_commitment_hashes ))
        (tuple8
           range
           range
           range
           (option int)
           (option string)
           int
           int
           (option (tuple2 string string)))

    let inbox : inbox Check.typ =
      let open Check in
      convert
        (fun {inbox_length; cumulated_size; merkle_root} ->
          (inbox_length, cumulated_size, merkle_root))
        (tuple3 int int string)

    let commitment : submitted_commitment Check.typ =
      let open Check in
      convert
        (fun {
               commitment =
                 {
                   level;
                   messages = {count; root; last_message_result_hash};
                   predecessor;
                   inbox_merkle_root;
                 };
               commitment_hash;
               committer;
               submitted_at;
               finalized_at;
             } ->
          ( ( level,
              (count, root, last_message_result_hash),
              predecessor,
              inbox_merkle_root ),
            commitment_hash,
            committer,
            submitted_at,
            finalized_at ))
        (tuple5
           (tuple4 int (tuple3 int string string) (option string) string)
           string
           string
           int
           (option int))

    let commitments_hashes : commitments_hashes Check.typ =
      let open Check in
      convert
        (fun r -> (r.message_hash, r.commitment_hash))
        (tuple2 string string)
  end

  module Parameters = struct
    type t = {finality_period : int; withdraw_period : int}

    let default = {finality_period = 60_000; withdraw_period = 60_000}

    let parameter_file ?(parameters = default) protocol =
      let args =
        [(["tx_rollup_enable"], Some "true")]
        @ [
            ( ["tx_rollup_finality_period"],
              Some (string_of_int parameters.finality_period) );
          ]
        @ [
            ( ["tx_rollup_withdraw_period"],
              Some (string_of_int parameters.withdraw_period) );
          ]
      in
      Protocol.write_parameter_file ~base:(Either.right (protocol, None)) args
  end
end
