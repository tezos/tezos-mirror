(* SPDX-CopyrightText Marigold <contact@marigold.dev> *)
#include "./ticket_type.mligo"
#include "./evm_type.mligo"

type storage = unit

// Ethereum RLP encoded transaction
type delayed_inbox_payload = {
  transaction : bytes;
  evm_rollup : address;
}

type return = operation list * storage

let main {transaction; evm_rollup} store : return =
  // Check that one tez has been sent
  let fees = Tezos.get_amount () in
  if fees < 1tez then
    failwith "Not enough tez to include the transaction in the delayed inbox"
  else
    // Craft an internal inbox message that respect the EVM rollup type
    // and put the payload in the bytes field.
    let evm_rollup : evm contract =
      Option.unopt ((Tezos.get_contract_opt evm_rollup) : evm contract option)
    in
    let send_to_delayed =
      Tezos.transaction (Other transaction) 0mutez evm_rollup
    in
    let burn_contract =
      Tezos.get_contract_with_error
        ("tz1burnburnburnburnburnburnburjAYjjX" : address)
        "Invalid burn address"
    in
    let burn_1tez =
      Tezos.transaction () 1tez burn_contract
    in
    [burn_1tez; send_to_delayed], store
