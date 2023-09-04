// The EVM administrator contract is controlled by a L1 address.
type storage = { admin : address }

// EVM rollup type used to send internal inbox message.
type rollup_type = (bytes * unit ticket * nat * bytes)

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
    let evm_rollup : rollup_type contract =
      Option.unopt ((Tezos.get_contract_opt evm_rollup) : rollup_type contract option)
    in
    let ticket =
      match Tezos.create_ticket () 1n with
      | Some ticket -> ticket
      | None -> failwith "Failed to create the ticket"
    in
    [Tezos.transaction (("" : bytes), ticket, 0n, payload) 0mutez evm_rollup], store
