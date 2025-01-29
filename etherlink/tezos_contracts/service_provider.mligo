(* SPDX-CopyrightText 2025 Functori <contact@functori.com> *)
(* SPDX-CopyrightText 2025 Nomadic Labs <contact@nomadic-labs.com> *)

#include "./ticket_type.mligo"

type storage = {
  fast_withdrawal_contract: address;
  exchanger : address;
  withdrawal_id : nat;
  target : address;
  timestamp : timestamp;
  service_provider : address;
  payload : bytes;
}

type payout_entry = {
  fast_withdrawal_contract: address;
  withdrawal_id : nat;
  ticket : tez_ticket;
  target : address;
  timestamp : timestamp;
  service_provider : address;
  payload: bytes;
}

type payout_entry_proxy = {
  fast_withdrawal_contract: address;
  exchanger : address;
  withdrawal_id : nat;
  target : address;
  timestamp : timestamp;
  service_provider : address;
  payload: bytes;
}

type return = operation list * storage

[@entry]
let payout_proxy ({fast_withdrawal_contract; exchanger; withdrawal_id; target; timestamp; service_provider; payload} : payout_entry_proxy) (_storage: storage) : return =
  let amount = Tezos.get_amount () in
  let payout = Tezos.address (Tezos.self("%payout"): tez_ticket contract) in
  match Tezos.get_entrypoint_opt "%mint" exchanger with
  | None -> failwith "Invalid tez ticket contract"
  | Some contract ->
    let mint = Tezos.Next.Operation.transaction payout amount contract in
    let payout_storage = {fast_withdrawal_contract; exchanger; withdrawal_id; target; timestamp; service_provider; payload} in
    [mint], payout_storage

[@entry]
let payout (ticket: tez_ticket) ({fast_withdrawal_contract; exchanger; withdrawal_id; target; timestamp; service_provider; payload}: storage) : return =
  match Tezos.get_entrypoint_opt "%payout_withdrawal" fast_withdrawal_contract with
  | None -> failwith "Invalid entrypoint"
  | Some contract ->
      let full_payload = (withdrawal_id, (ticket, (target, (timestamp, (service_provider, payload))))) in
      [Tezos.Next.Operation.transaction full_payload 0mutez contract], {fast_withdrawal_contract; exchanger; withdrawal_id; target; timestamp; service_provider; payload}
