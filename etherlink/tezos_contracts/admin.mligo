(* SPDX-CopyrightText Nomadic Labs <contact@nomadic-labs.com> *)

#include "./ticket_type.mligo"
#include "./evm_type.mligo"

// The EVM administrator contract is controlled by a L1 address.
type storage = { admin : address }


type return = operation list * storage

type upgrade_parameters = {
  evm_rollup : address;
  payload : bytes;
}

let main {evm_rollup; payload} (store : storage) : return =
  if Tezos.get_sender () <> store.admin then
    failwith "Unauthorized set entrypoint"
  else
    // Craft an internal inbox message that respect the EVM rollup type
    // and put the payload in the last "bytes" field.
    let evm_rollup : evm contract =
      Option.unopt ((Tezos.get_contract_opt evm_rollup) : evm contract option)
    in
    [Tezos.transaction (Upgrade payload) 0mutez evm_rollup], store
