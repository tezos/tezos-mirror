(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let produce_block (module Backend : Services_backend_sig.S) ~force ~timestamp =
  let open Lwt_result_syntax in
  let* transactions, delayed = Tx_pool.pop_transactions () in
  let n = List.length transactions + List.length delayed in
  if force || n > 0 then
    let* hashes =
      Backend.inject_raw_transactions
        ~timestamp
        ~smart_rollup_address:Backend.smart_rollup_address
        ~transactions
        ~delayed
    in
    let*! () =
      List.iter_p
        (fun hash -> Block_producer_events.transaction_selected ~hash)
        hashes
    in
    return n
  else return 0
