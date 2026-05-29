#import "../common/types/fast-withdrawal.mligo" "FastWithdrawal"
#import "./errors.mligo" "Errors"


(*
    Fast Withdrawal status:
    - Paid_out: user received payout, claim registered for the provider
    - Cemented: claim finalized, provider received funds from the smart rollup
*)
type status =
    | Paid_out of address
    | Cemented
type withdrawals = (FastWithdrawal.t, status) big_map

(*
    Fast Withdrawal contract configuration:
    - xtz_ticketer: address of the native XTZ ticketer (exchanger)
    - smart_rollup: address of the Etherlink smart rollup
    - expiration_seconds: number of seconds during which a withdrawal can be
      purchased at a discount
*)
type config = {
    xtz_ticketer : address;
    smart_rollup : address;
    expiration_seconds : int;
}

(*
    Fast Withdrawal contract storage:
    - withdrawals: maps each withdrawal key to a service provider
    - config: fast withdrawal contract configuration
    - metadata: the metadata of the contract (TZIP-016), immutable
*)
type t = {
    withdrawals : withdrawals;
    config : config;
    metadata : (string, bytes) big_map;
}

[@inline]
let add_withdrawal
        (withdrawal : FastWithdrawal.t)
        (service_provider : address)
        (storage : t) : t =
    let status = Paid_out service_provider in
    let withdrawals = Big_map.add withdrawal status storage.withdrawals in
    { storage with withdrawals }

[@inline]
let finalize_withdrawal
        (withdrawal : FastWithdrawal.t)
        (provider_opt : address option)
        (storage : t) : t =
    (* Update ledger only if paid out by provider, otherwise ignore. *)
    let withdrawals = match provider_opt with
    | Some _ -> Big_map.update withdrawal (Some Cemented) storage.withdrawals
    | None -> storage.withdrawals in
    { storage with withdrawals }

[@inline]
let assert_withdrawal_was_not_paid_before
        (withdrawal : FastWithdrawal.t)
        (storage : t) : unit =
    if Option.is_some (Big_map.find_opt withdrawal storage.withdrawals)
    then failwith Errors.duplicate_withdrawal_payout

[@inline]
let provider_of_paid_out
        (status : status) : address =
    match status with
    | Paid_out service_provider -> service_provider
    (* Unexpected state: possible duplicate execution of withdrawal outbox *)
    | Cemented -> failwith Errors.unexpected_cemented_withdrawal

[@inline]
let get_provider_opt
        (withdrawal : FastWithdrawal.t)
        (storage : t) : address option =
    let status_opt = Big_map.find_opt withdrawal storage.withdrawals in
    match status_opt with
    | Some status -> Some (provider_of_paid_out status)
    | None -> None

[@inline]
let is_xtz_ticketer
        (withdrawal : FastWithdrawal.t)
        (storage : t) : bool =
    withdrawal.ticketer = storage.config.xtz_ticketer
