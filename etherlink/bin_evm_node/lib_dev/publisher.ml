(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

module type TxEncoder = sig
  val encode_transaction :
    smart_rollup_address:string ->
    transaction:string ->
    (hash * string list) tzresult
end

module type Publisher = sig
  val publish_messages :
    smart_rollup_address:string -> messages:string list -> unit tzresult Lwt.t
end

module Make (TxEncoder : TxEncoder) (Publisher : Publisher) = struct
  let inject_raw_transactions ~smart_rollup_address ~transactions =
    let open Lwt_result_syntax in
    let* rev_tx_hashes, to_publish =
      List.fold_left_es
        (fun (tx_hashes, to_publish) tx_raw ->
          let*? tx_hash, messages =
            TxEncoder.encode_transaction
              ~smart_rollup_address
              ~transaction:tx_raw
          in
          return (tx_hash :: tx_hashes, to_publish @ messages))
        ([], [])
        transactions
    in
    let* () =
      Publisher.publish_messages ~smart_rollup_address ~messages:to_publish
    in
    return (List.rev rev_tx_hashes)
end
