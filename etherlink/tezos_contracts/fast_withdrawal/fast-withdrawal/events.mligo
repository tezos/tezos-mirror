#import "../common/types/fast-withdrawal.mligo" "FastWithdrawal"


type settle_withdrawal_event = {
    withdrawal : FastWithdrawal.t;
    receiver : address;
}

[@inline]
let settle_withdrawal
        (event : settle_withdrawal_event) : operation =
    Tezos.Next.Operation.emit "%settle_withdrawal" event

type payout_withdrawal_event = {
    withdrawal : FastWithdrawal.t;
    service_provider : address;
    payout_amount : nat;
}

[@inline]
let payout_withdrawal
        (event : payout_withdrawal_event) : operation =
    Tezos.Next.Operation.emit "%payout_withdrawal" event
