(* SPDX-CopyrightText Nomadic Labs <contact@nomadic-labs.com> *)

#include "./ticket_type.mligo"
#include "./evm_type.mligo"

type storage = unit

type delayed_inbox_payload = {
  transaction : bytes; // Ethereum RLP encoded transaction
  evm_rollup : address;
}

type return = operation list * storage

let chunk_size = 4000n

let chunk_bytes (b: bytes): bytes list =
  let len = Bytes.length b in
  let rec split (seq : bytes list) (offset : nat) : bytes list =
    if offset = len then seq
    else if offset + chunk_size > len then
      let rst = abs (len - offset) in
      (Bytes.sub offset rst b :: seq)
    else
      let s = Bytes.sub offset chunk_size b in
      split (s :: seq) (offset + chunk_size)
  in
  split [] 0n

[@entry]
let main {transaction; evm_rollup} store : return =
  // Check that one tez has been sent
  let fees = Tezos.get_amount () in
  if fees < 1tez then
    failwith "Not enough tez to include the transaction in the delayed inbox"
  else
    // Craft as many internal inbox message as necessary that respect the EVM
    // rollup type
    let evm_rollup : evm contract =
      Tezos.get_contract_with_error evm_rollup "Invalid smart rollup address"
    in
    let chunks = chunk_bytes transaction in
    let n_chunks : nat = List.length chunks in
    let operations =
      // First message to announce the chunk bytes
      let new_chunk_payload = Bytes.concat 0x00 (bytes n_chunks) in
      let announce =
        Tezos.transaction (Other new_chunk_payload) 0mutez evm_rollup
      in
      let chunks =
        List.fold_left (fun (acc, chunk) ->
            let chunk_payload = Bytes.concat 0x01 chunk in
            Tezos.transaction (Other chunk_payload) 0mutez evm_rollup :: acc)
          [] chunks
      in
      announce :: chunks
    in
    // Burn the tez
    let burn_contract =
      Tezos.get_contract_with_error
        ("tz1burnburnburnburnburnburnburjAYjjX" : address)
        "Invalid burn address"
    in
    let burn_1tez = Tezos.transaction () 1tez burn_contract
    in
    burn_1tez :: operations, store
