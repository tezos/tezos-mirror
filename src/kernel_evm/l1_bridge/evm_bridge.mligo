// The bridge communicates with only one EVM rollup, and accepts FA1.2 tokens
// from only one contract.
type storage = {
  admin : address;
  rollup : address option;
  ctez_contract : address;
}

type deposit_args = {
  evm_address : bytes;
  amount : nat;
  max_gas_price : nat;
}

// The bridge contains 2 public entrypoints: deposit and withdraw.
// Set is available only to the administrator.
type parameter =
  | Deposit of deposit_args
  | Withdraw of (address * nat)
  | Set of address

// EVM rollup expected type.
type rollup_type = (bytes * unit ticket * nat)

// Entrypoint return type.
type return = operation list * storage

type fa12_transfer_params = [@layout:comb] {
    [@annot:from] from_: address;
    [@annot:to] to_: address;
    value: nat;
}

// Deposit transfers CTEZ from the contract to the L1 bridge. Transfer
// the triplet (evm_address * nat * nat) to the EVM rollup.
let deposit evm_address (amount : nat) (max_gas_price : nat) (store : storage)
  : operation list =
  // Sender
  let from_ = Tezos.get_sender () in
  // L1 bridge
  let self_address = Tezos.get_self_address () in
  // CTEZ transfer entrypoint
  let ctez_transfer : fa12_transfer_params contract =
    match Tezos.get_entrypoint_opt "%transfer" store.ctez_contract with
    | Some entrypoint -> entrypoint
    | None -> failwith "Failed to find the entrypoint %transfer"
  in
  // EVM rollup
  let evm_rollup : rollup_type contract =
    match store.rollup with
    | Some rollup ->
      Option.unopt ((Tezos.get_contract_opt rollup) : rollup_type contract option)
    | None -> failwith "The EVM rollup was not set"
  in
  // Transfer CTEZ to L1 bridge
  let transfer_ctez =
    let params = { from_; to_ = self_address; value = amount } in
    Tezos.transaction params 0mutez ctez_transfer
  in
  // Mint the ticket
  let ticket =
    match Tezos.create_ticket () amount with
    | Some ticket -> ticket
    | None -> failwith "Failed to create the ticket"
  in
  // Create deposit transfer
  let deposit =
    Tezos.transaction (evm_address, ticket, max_gas_price) 0mutez evm_rollup
  in
  [transfer_ctez; deposit]

// Withdraw the CTEZ from the rollup and sends the according CTEZ to
// the L1 address withdrawing assets.
let withdraw (_l1_address, _amount) (_store : storage) : operation list =
    []

// Set the EVM rollup, only the smart contract admin is able to do it.
let set (evm_rollup : address) (store : storage) : storage =
 if Tezos.get_sender () <> store.admin then
   failwith "Unauthorized set entrypoint"
 else
   { store with rollup = Some evm_rollup }
    

let main (action : parameter) (store: storage) : return =
  match action with
  | Deposit {evm_address; amount; max_gas_price} ->
    deposit evm_address amount max_gas_price store, store
  | Withdraw p -> withdraw p store, store
  | Set evm_rollup -> [], set evm_rollup store
