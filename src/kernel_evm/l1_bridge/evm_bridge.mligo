(* SPDX-CopyrightText 2023 Trilitech <contact@trili.tech> *)
(* SPDX-CopyrightText Nomadic Labs <contact@nomadic-labs.com> *)

#include "./ticket_type.mligo"
#include "./evm_type.mligo"

type request_deposit = {
  l2_address: bytes; (* L2 address. *)
  evm_address: address; (* EVM rollup address. *)
}

type storage = {
  exchanger: address; (* Address of exchanger contract minting tez tickets. *)
  request_deposit: request_deposit option; (* Address of L2 depositee *)
}

type parameter =
| Deposit of request_deposit
| Callback of tez_ticket

type return = operation list * storage

let deposit request ({exchanger; request_deposit}: storage) : return =
  let () =
    match request_deposit with
    | None -> ()
    | Some _ -> failwith "deposit locked"
  in
  let amount = Tezos.get_amount () in
  let callback = Tezos.address (Tezos.self("%callback") : tez_ticket contract) in
  match Tezos.get_entrypoint_opt "%mint" exchanger with
  | None -> failwith "Invalid tez ticket contract"
  | Some contract ->
    let mint = Tezos.transaction callback amount contract in
    let callback_storage = {exchanger; request_deposit = Some request} in
    [mint], callback_storage

let callback (ticket: tez_ticket) {exchanger; request_deposit} : return =
  let request_deposit =
    match request_deposit with
    | None -> failwith "Callback on non-locked deposit"
    | Some r -> r
  in
  let {l2_address; evm_address} = request_deposit in
  let evm_address: evm contract =
    Tezos.get_contract_with_error evm_address "Invalid rollup address"
  in
  let deposit =
    Tezos.transaction
      (DepositTicket (l2_address, ticket))
      0mutez
      evm_address
  in
  let reset_storage = {exchanger; request_deposit = None} in
  [deposit], reset_storage

let main (action, storage : parameter * storage) : return =
  match action with
  | Deposit request -> deposit request storage
  | Callback tez_ticket -> callback tez_ticket storage

