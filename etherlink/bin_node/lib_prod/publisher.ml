(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

module type TxEncoder = sig
  (* Transactions to be encoded *)
  type transactions = {raw : string list; delayed : Delayed_transaction.t list}

  (* Encoded messages to be injected *)
  type messages

  val encode_transactions :
    smart_rollup_address:string ->
    transactions:transactions ->
    (hash list * messages) tzresult
end

module type Publisher = sig
  type messages

  val publish_messages :
    timestamp:Time.Protocol.t ->
    smart_rollup_address:string ->
    messages:messages ->
    unit tzresult Lwt.t
end

module Make
    (TxEncoder : TxEncoder)
    (Publisher : Publisher with type messages = TxEncoder.messages) =
struct
  let inject_raw_transactions ~timestamp ~smart_rollup_address ~transactions
      ~delayed =
    let open Lwt_result_syntax in
    let*? tx_hashes, to_publish =
      TxEncoder.encode_transactions
        ~smart_rollup_address
        ~transactions:{raw = transactions; delayed}
    in
    let* () =
      Publisher.publish_messages
        ~timestamp
        ~smart_rollup_address
        ~messages:to_publish
    in
    return tx_hashes
end
