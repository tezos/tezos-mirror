(* SPDX-CopyrightText 2025 Functori <contact@functori.com> *)
(* SPDX-CopyrightText 2025 Nomadic Labs <contact@nomadic-labs.com> *)

(*
 * Withdrawal Management Contract
 *
 * This contract handles ticket-based withdrawals with support for both
 * standard withdrawals and fast withdrawal payouts of native and FA tokens.
 * It maintains a record of processed withdrawals to prevent duplicate processing.
 *)

#include "./ticket_type.mligo"

(* Types *)

type payout_key = {
  withdrawal_id: nat;    (* Unique identifier for the withdrawal operation *)
  ticket_amount: nat;    (* The amount in the ticket associated with the withdrawal *)
  timestamp: timestamp;  (* Time when the withdrawal request was made *)
  target: address;       (* Address receiving the withdrawn assets *)
  payload: bytes;        (* Additional data attached to the withdrawal *)
  l2_caller: bytes;      (* Identifier of the L2 caller initiating the withdrawal *)

}

(* Storage structure containing the tez ticket exchanger registered
   on Etherlink and a big_map to track withdrawals. *)
type storage = {
  tez_ticket_exchanger: address;   (* Address of the tez ticket exchanger contract *)
  withdrawals: (payout_key, address) big_map;
  (* A map of processed withdrawals, indexed by withdrawal metadata,
     with entries storing the addresses of the payers. *)
}

type withdrawal_info = {
  withdrawal_id: nat;    (* Unique identifier for the withdrawal operation *)
  ticket: tez_ticket;    (* The ticket associated with the withdrawal *)
  timestamp: timestamp;  (* Time when the withdrawal request was made *)
  target: address;       (* Address receiving the withdrawn assets *)
  payload: bytes;        (* Additional data attached to the withdrawal *)
  l2_caller: bytes;      (* Identifier of the L2 caller initiating the withdrawal *)
}

type payout_info = {
  withdrawal_id: nat;    (* Unique identifier for the withdrawal operation *)
  ticket: tez_ticket;    (* The ticket associated with the withdrawal *)
  timestamp: timestamp;  (* Time when the withdrawal request was made *)
  target: address;       (* Address receiving the withdrawn assets *)
  payer: address;        (* The entity funding the payout *)
  payload: bytes;        (* Additional data attached to the withdrawal *)
  l2_caller: bytes;      (* Identifier of the L2 caller initiating the withdrawal *)
}

(* Return type for entrypoints: a list of operations and the updated storage *)
type return = operation list * storage

(* Helper Functions *)

(*
 * Retrieves the appropriate contract for burning or withdrawing tickets
 * based on the ticketer address. If the address of the ticketer does not
 * correspond to the tez ticketer registered on Etherlink, we assume that
 * it exposes the %withdraw entrypoint to burn the ticket and transfer the
 * unlocked token to the provided address.
 *
 * @param ticketer The address of the ticket issuer
 * @param storage The current contract storage
 * @return The contract to process the ticket
 *)
let get_contract (ticketer: address) (storage: storage): (address * tez_ticket) contract =
  if ticketer = storage.tez_ticket_exchanger then
    match Tezos.get_entrypoint_opt "%burn" storage.tez_ticket_exchanger with
    | None -> failwith "Invalid tez ticket exchanger contract"
    | Some contract -> contract
  else
    match Tezos.get_entrypoint_opt "%withdraw" ticketer with
    | None -> failwith "Invalid FA ticket exchanger contract"
    | Some contract -> contract

(*
 * Validates that the ticket metadata is correct for tez withdrawals.
 * For tez tickets, token_id should be 0 and metadata should be None.
 *
 * @param ticketer The address of the ticket issuer
 * @param ticket_contents The contents of the ticket to validate
 * @param storage The current contract storage
 *)
let validate_ticket_contents (ticketer: address) (ticket_contents: nat * (bytes option)) (storage: storage) =
  if ticketer = storage.tez_ticket_exchanger then
    match ticket_contents with
    | (_, Some _) -> failwith "Unexpected token metadata for tez withdrawal"
    | (token_id, None) -> if token_id <> 0n then failwith "Invalid token ID for tez withdrawal"
  else ()

(* Entrypoints *)

(*
 * Entry point to record and process a fast withdrawal payout
 *
 * Fast withdrawals involve a payer who facilitates quicker withdrawals.
 * The payout is recorded in the withdrawals map to prevent duplicate processing.
 *)
[@entry]
let payout_withdrawal ({withdrawal_id; ticket; target; timestamp; payer; payload; l2_caller} : payout_info) (storage: storage) : return =
  let (ticketer, (ticket_contents, ticket_amount)), ticket = Tezos.Next.Ticket.read ticket in

  (* Create unique key for this withdrawal *)
  let payout_key = {withdrawal_id; ticket_amount; timestamp; target; payload; l2_caller} in

  (* Check if this withdrawal was already processed *)
  if Option.is_some (Big_map.find_opt payout_key storage.withdrawals) then
    failwith "This fast withdrawal has already been processed"
  else
    (* Validate the ticket contents *)
    let () = validate_ticket_contents ticketer ticket_contents storage in

    (* Get the appropriate contract to handle the ticket *)
    let contract = get_contract ticketer storage in

    (* Record the payer in the withdrawals map *)
    let updated_withdrawals = Big_map.add payout_key payer storage.withdrawals in
    let updated_storage = { storage with withdrawals = updated_withdrawals } in

    (* Create the transaction operation *)
    let payout = Tezos.Next.Operation.transaction (target, ticket) 0mutez contract in

    ([payout], updated_storage)

(*
 * Entry point to process a standard withdrawal request
 *
 * Handles both:
 * 1. New withdrawals (not in the withdrawals map)
 * 2. Previously processed fast withdrawals (sending ticket to the payer)
 *
 * Note: The payer field exists in the withdrawal_info type but is not used
 * in this entrypoint's logic.
 *)
[@entry]
let default ({ withdrawal_id; ticket; timestamp; target; payload; l2_caller }: withdrawal_info) (storage: storage) : return =
  let (ticketer, (ticket_contents, ticket_amount)), ticket = Tezos.Next.Ticket.read ticket in

  (* Create unique key for this withdrawal *)
  let payout_key = {withdrawal_id; ticket_amount; timestamp; target; payload; l2_caller} in

  (* Validate the ticket contents *)
  let () = validate_ticket_contents ticketer ticket_contents storage in

  (* Get the appropriate contract to handle the ticket *)
  let contract = get_contract ticketer storage in

  match Big_map.find_opt payout_key storage.withdrawals with
  | None ->
      (* Withdrawal without early payout - send ticket to target *)
      let standard_withdrawal = Tezos.Next.Operation.transaction (target, ticket) 0mutez contract in
      ([standard_withdrawal], storage)

  | Some payer ->
      (* This is a previously processed fast withdrawal - send ticket to payer *)
      let updated_withdrawals = Big_map.remove payout_key storage.withdrawals in
      let updated_storage = { storage with withdrawals = updated_withdrawals } in

      let reimburse_payer = Tezos.Next.Operation.transaction (payer, ticket) 0mutez contract in
      ([reimburse_payer], updated_storage)
