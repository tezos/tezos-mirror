#import "errors.mligo" "Errors"

let assert_no_tez_in_transaction
        (_ : unit)
        : unit =
    Assert.Error.assert (Tezos.get_amount () = 0mutez) Errors.tez_in_transaction_disallowed

let assert_voting_power_positive
        (voting_power : nat)
        : unit =
    Assert.Error.assert (voting_power > 0n) Errors.no_voting_power
