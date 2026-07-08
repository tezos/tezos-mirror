(* SPDX-CopyrightText 2025 Functori <contact@functori.com> *)
(* SPDX-CopyrightText 2025 Nomadic Labs <contact@nomadic-labs.com> *)
(* SPDX-CopyrightText 2025 Baking Bad <hello@baking-bad.org> *)

#import "../common/entrypoints/settle-withdrawal.mligo" "SettleWithdrawalEntry"
#import "../common/entrypoints/xtz-ticketer-burn.mligo" "XtzTicketerBurnEntry"
#import "../common/types/ticket.mligo" "Ticket"
#import "../common/types/fast-withdrawal.mligo" "FastWithdrawal"
#import "../common/errors.mligo" "CommonErrors"
#import "./storage.mligo" "Storage"
#import "./events.mligo" "Events"
#import "./errors.mligo" "Errors"


(*
    Fast Withdrawal is a contract allowing service providers to promptly
    finalize user withdrawals and later receive funds from Etherlink after
    outbox message settlement.

    Supported withdrawal types:
    - XTZ withdrawals (requires exchanger to implement the `burn` entrypoint)

    Workflow:
    1. Providers initiate withdrawal claims by calling `payout_withdrawal`.
    2. For native withdrawals, providers must attach the required XTZ amount.
    3. The payout amount is determined based on withdrawal expiration:
        - If the withdrawal has not expired, a discounted amount (encoded in
          the payload as Michelson nat) applies.
        - If the withdrawal has expired, the full amount must be paid.
    4. After successful validation, funds are transferred to the user, and the
        provider's address is recorded in the `withdrawals` ledger as a claim.
    5. Withdrawal settlement occurs via the `default` entrypoint triggered by
        an outbox message from the smart rollup:
        - If paid out, the provider receives the unwrapped ticket as payout.
        - Otherwise, the ticket is unwrapped for the `base_withdrawer`.
*)

type payout_withdrawal_params = {
    withdrawal : FastWithdrawal.t;
    service_provider : address;
}

type return = operation list * Storage.t

[@inline]
let assert_sender_is_allowed
        (smart_rollup : address) : unit =
    if Tezos.get_sender () <> smart_rollup then
        failwith Errors.sender_is_not_allowed

[@inline]
let unpack_payload
        (payload : bytes) : nat =
    match Bytes.unpack payload with
    | Some amount -> amount
    | None -> failwith Errors.payload_unpack_failed

[@inline]
let is_withdrawal_expired
        (withdrawal : FastWithdrawal.t)
        (expiration_seconds : int) : bool =
    Tezos.get_now() > withdrawal.timestamp + expiration_seconds

[@inline]
let assert_withdrawal_not_in_future
        (withdrawal : FastWithdrawal.t) : unit =
    if Tezos.get_now() < withdrawal.timestamp
    then failwith Errors.timestamp_in_future
    else unit

[@inline]
let assert_l2_caller_is_20_bytes_long
        (l2_caller : bytes) : unit =
    if Bytes.length l2_caller <> 20n
    then failwith Errors.wrong_l2_caller_length
    else unit

[@inline]
let assert_attached_amount_is_valid
        (valid_amount : nat) : unit =
    if Tezos.get_amount () <> valid_amount * 1mutez
    then failwith Errors.invalid_xtz_amount
    else unit

[@inline]
let assert_payout_at_most_full_amount
        (payout_amount : nat)
        (withdrawal : FastWithdrawal.t) : unit =
    if payout_amount > withdrawal.full_amount
    then failwith Errors.payout_exceeds_full_amount
    else unit

[@inline]
let assert_ticket_content_is_valid_for_xtz
        (withdrawal : FastWithdrawal.t) : unit =
    let valid_xtz_content : Ticket.content_t = (0n, None) in
    if withdrawal.content <> valid_xtz_content
    then failwith Errors.wrong_xtz_content
    else unit

[@inline]
let assert_no_xtz_deposit
        (_ : unit) : unit =
    if Tezos.get_amount () > 0mutez
    then failwith CommonErrors.xtz_deposit_disallowed
    else unit

[@inline]
let send_xtz_op
        (amount : nat)
        (receiver : address) : operation =
    let entry = Tezos.get_contract receiver in
    Tezos.Next.Operation.transaction unit (amount * 1mutez) entry

[@entry]
let payout_withdrawal
        (params : payout_withdrawal_params)
        (storage: Storage.t) : return =
    (*
        `payout_withdrawal` allows a service provider to finalize a user's
        withdrawal early, creating a claim for future settlement.

        Parameters:
        @param withdrawal: details of the user's requested withdrawal.
        @param service_provider: address eligible to receive reimbursement.

        Effects:
        - records the withdrawal claim in the `withdrawals` ledger.
        - transfers funds immediately to the withdrawer.
        - emits the `payout_withdrawal` event.
    *)

    let { withdrawal; service_provider } = params in
    let expiration_seconds = storage.config.expiration_seconds in
    let receiver = withdrawal.base_withdrawer in
    let _ = Storage.assert_withdrawal_was_not_paid_before withdrawal storage in
    let _ = assert_withdrawal_not_in_future withdrawal in
    let _ = assert_l2_caller_is_20_bytes_long withdrawal.l2_caller in

    (* If expired, pay full; else pay discounted amount from payload. *)
    let payout_amount =
        if is_withdrawal_expired withdrawal expiration_seconds
        then withdrawal.full_amount
        else unpack_payload withdrawal.payload in
    let _ = assert_payout_at_most_full_amount payout_amount withdrawal in

    let payout_operation = if Storage.is_xtz_ticketer withdrawal storage then
        (* Service provider payout of an XTZ withdrawal. *)
        let _ = assert_ticket_content_is_valid_for_xtz withdrawal in
        let _ = assert_attached_amount_is_valid payout_amount in
        send_xtz_op payout_amount receiver
    else
        (* Service provider payout of an FA withdrawal. *)
        failwith Errors.only_xtz_withdrawals_are_supported in

    let storage = Storage.add_withdrawal withdrawal service_provider storage in
    let event_params = { withdrawal; service_provider; payout_amount } in
    let payout_event = Events.payout_withdrawal event_params in
    [payout_operation; payout_event], storage

[@entry]
let default
        (params : SettleWithdrawalEntry.t)
        (storage: Storage.t) : return =
    (*
        `default` is an entrypoint that receives tickets from the Etherlink
        smart rollup after the corresponding outbox withdrawal message has been
        executed. It finalizes previously paid out withdrawal or, if no claim
        was recorded, unwraps the ticket directly to the withdrawer.

        Parameters:
        @param withdrawal_id: unique identifier of the withdrawal.
        @param ticket: ticket provided with withdrawal (XTZ or FA).
        @param timestamp: time when the withdrawal was applied on Etherlink.
        @param base_withdrawer: original withdrawal receiver address.
        @param payload: fast withdrawal conditions packed into bytes.
        @param l2_caller: original sender address from the Etherlink side.

        Effects:
        - updates `withdrawals` from `Paid_out` to `Cemented` if payout existed.
        - unwraps ticket to provider (if prepaid) or original withdrawer.
        - emits the `settle_withdrawal` event.
    *)

    let (ticket, withdrawal) = SettleWithdrawalEntry.to_key_and_ticket params in
    let _ = assert_no_xtz_deposit () in
    let _ = assert_sender_is_allowed storage.config.smart_rollup in

    let provider_opt = Storage.get_provider_opt withdrawal storage in
    let receiver = match provider_opt with
    | None -> withdrawal.base_withdrawer
    | Some provider -> provider in
    (* Non-XTZ tickets received on settlement are sent to base_withdrawer *)
    let finalize_operation = if Storage.is_xtz_ticketer withdrawal storage then
        XtzTicketerBurnEntry.send withdrawal.ticketer { receiver; ticket }
    else
        Ticket.send ticket withdrawal.base_withdrawer in
    let finalize_event = Events.settle_withdrawal { withdrawal; receiver } in
    let storage = Storage.finalize_withdrawal withdrawal provider_opt storage in
    [finalize_operation; finalize_event], storage

[@view]
let get_status
        (withdrawal : FastWithdrawal.t)
        (storage : Storage.t) : Storage.status option =
    Big_map.find_opt withdrawal storage.withdrawals

[@view]
let get_config (_ : unit) (storage : Storage.t) : Storage.config =
    storage.config
