(* SPDX-CopyrightText 2025 Functori <contact@functori.com> *)
(* SPDX-CopyrightText 2025 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-CopyrightText 2026 Trilitech <contact@trili.tech> *)

#import "../common/types/fast-withdrawal.mligo" "FastWithdrawal"

type tez_ticket = (nat * (bytes option)) ticket

(*
 * Fast Withdrawal Proxy Contract
 *
 * This contract is used for testing purposes only and acts as an intermediary
 * for processing fast withdrawals. For tez withdrawals, it forwards the call
 * directly to the fast-withdrawal contract with the attached tez. For FA withdrawals,
 * it still handles the ticket creation/routing through the exchanger.
 *)

(* Type Definitions *)

(* Main contract storage *)
type storage = {
  fast_withdrawal_contract: address;  (* Address of the withdrawal contract *)
  exchanger: address;                 (* Address of the ticket exchanger *)
  withdrawal_id: nat;                 (* Unique identifier for the withdrawal *)
  target: address;                    (* Recipient address *)
  timestamp: timestamp;               (* Timestamp of the operation *)
  service_provider: address;          (* Service provider address *)
  payload: bytes;                     (* Additional payload data *)
  l2_caller: bytes;                   (* L2 caller address (20 bytes) *)
  payout_amount: tez;                 (* Amount to forward to payout_withdrawal *)
}

type payout_withdrawal_params = FastWithdrawal.t * address

(* tez payout proxy parameters *)
type payout_entry_proxy = {
  fast_withdrawal_contract: address;
  exchanger: address;
  withdrawal_id: nat;
  target: address;
  timestamp: timestamp;
  service_provider: address;
  payload: bytes;
  l2_caller: bytes;
}

(* FA token payout proxy parameters with additional amount field *)
type fa_payout_entry_proxy = {
  fast_withdrawal_contract: address;
  exchanger: address;
  amount: nat;                        (* Amount of FA tokens *)
  withdrawal_id: nat;
  target: address;
  timestamp: timestamp;
  service_provider: address;
  payload: bytes;
  l2_caller: bytes;
}

(* Return type for all entrypoints *)
type return = operation list * storage

(* Entrypoint types for internal routing *)
type entrypoint_t =
  | Default of unit
  | RouterWithdraw of address
  | RollupDeposit of bytes

(* Internal call structure for routing *)
type internal_call_t = [@layout:comb] {
  target: address;
  entrypoint: entrypoint_t;
  xtz_amount: tez;
}

(* Helper Functions *)

(*
 * Validates that the L2 caller address has the correct length
 *
 * @param l2_caller The L2 caller address as bytes
 * @raises If L2 caller address is not exactly 20 bytes
 *)
let validate_l2_caller (l2_caller: bytes) : unit =
  if not (Bytes.length l2_caller = 20n) then
    failwith "L2 caller's address size must be exactly 20 bytes"
  else
    ()

(*
 * Gets the mint entrypoint for the given exchanger contract
 *
 * @param exchanger Address of the exchanger contract
 * @return The mint entrypoint contract
 * @raises If the mint entrypoint does not exist
 *)
let get_mint_entrypoint (exchanger: address) =
  match Tezos.get_entrypoint_opt "%mint" exchanger with
  | None -> failwith "Invalid exchanger contract: missing %mint entrypoint"
  | Some contract -> contract

let get_payout_withdrawal_entrypoint (fast_withdrawal_contract: address) =
  match Tezos.get_entrypoint_opt "%payout_withdrawal" fast_withdrawal_contract with
  | None ->
      failwith
        "Invalid fast withdrawal contract: missing %payout_withdrawal entrypoint"
  | Some contract -> contract


(* Entrypoints *)

(*
 * Proxy entrypoint for tez payouts
 *
 * Forwards the payout directly to the fast-withdrawal contract with the
 * attached tez.
 *)
[@entry]
let payout_proxy_tez (params: payout_entry_proxy) (storage: storage) : return =
  let () = validate_l2_caller params.l2_caller in
  let amount = Tezos.get_amount () in
  let full_amount : nat = amount / 1mutez in
  let withdrawal : FastWithdrawal.t = {
    withdrawal_id = params.withdrawal_id;
    full_amount;
    ticketer = params.exchanger;
    content = (0n, (None : bytes option));
    timestamp = params.timestamp;
    base_withdrawer = params.target;
    payload = params.payload;
    l2_caller = params.l2_caller;
  } in
  let full_payload = (withdrawal, params.service_provider) in
  let contract = get_payout_withdrawal_entrypoint params.fast_withdrawal_contract in
  let withdrawal_operation =
    Tezos.Next.Operation.transaction full_payload amount contract
  in
  [withdrawal_operation], storage

(*
 * Proxy entrypoint for FA token payouts
 *
 * This entrypoint performs two key operations:
 * 1. Sets up internal routing by configuring the exchanger to send the ticket
 *    to this contract's payout entrypoint after withdrawal
 * 2. Mints a new FA token ticket with the specified amount
 *
 * The Default() routing mode is used to ensure that after withdrawal,
 * the ticket is automatically sent back to this contract's payout entrypoint
 * where it can be properly processed for the fast withdrawal.
 *)
[@entry]
let payout_proxy_fa (params: fa_payout_entry_proxy) (_storage: storage) : return =
  (* Validate L2 caller address *)
  let () = validate_l2_caller params.l2_caller in

  (* Get our own payout entrypoint address *)
  let payout_address = Tezos.address (Tezos.self("%payout"): tez_ticket contract) in

  (* Create internal call for routing *)
  let internal_call = {
    target = payout_address;
    entrypoint = Default();
    xtz_amount = 0mutez;
  } in

  (* Get the set routing entrypoint on the exchanger contract *)
  match Tezos.get_entrypoint_opt "%set" params.exchanger with
  | None -> failwith "Invalid exchanger contract: missing %set entrypoint"
  | Some (set_contract : (internal_call_t contract)) -> (
    (* Create operation to set internal routing *)
    let set_operation = Tezos.Next.Operation.transaction internal_call 0mutez set_contract in

    (* Get the mint entrypoint for FA tokens *)
    match Tezos.get_entrypoint_opt "%mint" params.exchanger with
    | None -> failwith "Invalid exchanger contract: missing %mint entrypoint"
    | Some (mint_contract : ((nat * (bytes option)) * nat) contract) -> (
      (* Create token with ID 0 and no metadata, amount from parameters *)
      let mint_data = ((0n, None), params.amount) in

      (* Create mint operation *)
      let mint_operation = Tezos.Next.Operation.transaction mint_data 0mutez mint_contract in

      (* Update the storage with input parameters (excluding amount which is FA-specific) *)
      let updated_storage = {
        fast_withdrawal_contract = params.fast_withdrawal_contract;
        exchanger = params.exchanger;
        withdrawal_id = params.withdrawal_id;
        target = params.target;
        timestamp = params.timestamp;
        service_provider = params.service_provider;
        payload = params.payload;
        l2_caller = params.l2_caller;
        payout_amount = 0mutez;
      } in

      (* Return both operations and updated storage *)
      [set_operation; mint_operation], updated_storage))

(*
 * Main payout entrypoint
 *
 * Receives a ticket and forwards it to the fast withdrawal contract
 * along with all the necessary metadata.
 *)
[@entry]
let payout (ticket: tez_ticket) (storage: storage) : return =
  let contract = get_payout_withdrawal_entrypoint storage.fast_withdrawal_contract in
  let (ticketer, (content, full_amount)), _ticket = Tezos.Next.Ticket.read ticket in
  let withdrawal : FastWithdrawal.t = {
    withdrawal_id = storage.withdrawal_id;
    full_amount;
    ticketer;
    content;
    timestamp = storage.timestamp;
    base_withdrawer = storage.target;
    payload = storage.payload;
    l2_caller = storage.l2_caller;
  } in
  let full_payload = (withdrawal, storage.service_provider) in

  let withdrawal_operation =
    Tezos.Next.Operation.transaction full_payload storage.payout_amount contract
  in

  [withdrawal_operation], storage
