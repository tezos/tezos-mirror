(* SPDX-CopyrightText 2025 Functori <contact@functori.com> *)
(* SPDX-CopyrightText 2025 Nomadic Labs <contact@nomadic-labs.com> *)

#include "./ticket_type.mligo"

type storage = {
  exchanger: address; (* the address of the ticketer *)
  withdrawals : (nat, (nat * timestamp * address * bytes)) big_map; (* stores (quantity, timestamp, payer_address, payload) for each withdrawal id *)
}

type withdrawal_entry = {
  withdrawal_id : nat;
  ticket : tez_ticket;
  timestamp : timestamp;
  base_withdrawer: address;
  payload: bytes;
}

type payout_entry = {
  withdrawal_id : nat;
  ticket : tez_ticket;
  target : address;
  timestamp : timestamp;
  service_provider : address;
  payload: bytes;
}

type return = operation list * storage

[@entry]
let payout_withdrawal ({withdrawal_id; ticket; target; timestamp; service_provider; payload} : payout_entry) (storage: storage) : return =
  let is_in_storage = Option.is_some (Big_map.find_opt withdrawal_id storage.withdrawals) in
  (* Ensure that the fast withdrawal was not already payed. *)
  if not is_in_storage then 
    (* Update storage to record prepayment. *)
    let (_, (_, amount)), ticket = Tezos.Next.Ticket.read ticket in
    let updated_withdrawals = Big_map.add withdrawal_id (amount, timestamp, service_provider, payload) storage.withdrawals in
    let storage = { storage with withdrawals = updated_withdrawals } in
    (match Tezos.get_entrypoint_opt "%burn" storage.exchanger with
      | None -> failwith "Invalid tez ticket contract"
      | Some contract ->
        [Tezos.Next.Operation.transaction (target, ticket) 0mutez contract], storage)
  else
    failwith "The fast withdrawal was already payed"

[@entry]
let default ({ withdrawal_id; ticket; timestamp; base_withdrawer; payload} : withdrawal_entry)  (storage: storage) : return =
  match Big_map.find_opt withdrawal_id storage.withdrawals with
  | None -> 
    (* No advance payment found, send to the withdrawer. *)
    (match Tezos.get_entrypoint_opt "%burn" storage.exchanger with
    | None -> failwith "Invalid tez ticket contract"
    | Some contract ->
      [Tezos.Next.Operation.transaction (base_withdrawer, ticket) 0mutez contract], storage)
  | Some (prepaid_quantity, prepaid_timestamp, payer, stored_payload) ->
    (* Check if the provider has prepaid. *)
    let (_, (_, amount)), ticket = Tezos.Next.Ticket.read ticket in
    if prepaid_timestamp = timestamp && prepaid_quantity = amount && stored_payload = payload then
      (* Everything matches, the withdrawal was payed, we send the amount
         to the payer. *)
      let updated_withdrawals  = Big_map.remove withdrawal_id storage.withdrawals in 
      let storage =  { storage with withdrawals = updated_withdrawals } in        
      (match Tezos.get_entrypoint_opt "%burn" storage.exchanger with
      | None -> failwith "Invalid tez ticket contract"
      | Some contract ->
        [Tezos.Next.Operation.transaction (payer, ticket) 0mutez contract], storage)
    else
      failwith "Unexpected behavior"
