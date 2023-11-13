(* SPDX-CopyrightText Trilitech <contact@trili.tech> *)
(* SPDX-CopyrightText Nomadic Labs <contact@nomadic-labs.com> *)

#include "./ticket_type.mligo"

type storage = unit

type parameter =
  | Mint of address
  | Burn of (address * tez_ticket)

type return = operation list * storage

// Mint creates [Tezos.get_amount ()] tickets and transfers them to [address].
let mint address : return =
  let contract : tez_ticket contract =
    Tezos.get_contract_with_error address "Invalid callback"
  in
  let amount: nat = Tezos.get_amount () / 1mutez in
  let tickets : tez_ticket =
    match Tezos.create_ticket (0n, None) amount with
    | Some (t : tez_ticket) -> t
    | None -> failwith "Could not mint ticket."
  in
  ([Tezos.transaction tickets 0mutez contract], ())

// Burn destructs the [ticket] and sends back the tez to [address].
let burn address (ticket: tez_ticket) : return =
  if Tezos.get_amount () > 0tez then
    failwith "Burn does not accept tez."
  else
    let (addr, (_, amt)), _ticket = Tezos.read_ticket ticket in
    if addr <> (Tezos.get_self_address ()) then
      failwith "Burn only accepts tez tickets."
    else
      let contract = Tezos.get_contract_with_error address "Invalid callback" in
      let amount: tez = amt * 1mutez in
      ([Tezos.transaction () amount contract], ())

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)
let main (action, _store : parameter * storage) : return =
  match action with
  | Mint callback -> mint callback
  | Burn (callback, tt) -> burn callback tt
