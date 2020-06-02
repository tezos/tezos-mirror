(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Protocol
open Alpha_context

type t = {
  pkh : Signature.Public_key_hash.t;
  pk : Signature.Public_key.t;
  sk : Signature.Secret_key.t;
}

type baker = {key : t; baker : Baker_hash.t}

type account = t

let known_accounts = Signature.Public_key_hash.Table.create 17

module Baker_hash_iter = Helpers.MakeIterator (struct
  include Baker_hash

  type t = Baker_hash.t

  let hash = Hashtbl.hash

  let seeded_hash = Hashtbl.seeded_hash
end)

module Baker_table = Baker_hash_iter.Table

let known_bakers = Baker_table.create 17

let new_account ?seed () =
  let (pkh, pk, sk) = Signature.generate_key ?seed () in
  let account = {pkh; pk; sk} in
  Signature.Public_key_hash.Table.add known_accounts pkh account ;
  account

let new_baker ?seed ?origination_nonce () =
  let (pkh, pk, sk) = Signature.generate_key ?seed () in
  let nonce =
    Option.value
      ~default:(Contract.initial_origination_nonce Operation_hash.zero)
      origination_nonce
  in
  let baker = Contract.baker_from_nonce nonce in
  let account = {key = {pkh; pk; sk}; baker} in
  Baker_table.add known_bakers baker account ;
  account

let add_account ({pkh; _} as account) =
  Signature.Public_key_hash.Table.add known_accounts pkh account

let add_baker ({baker; _} as account) =
  Baker_table.add known_bakers baker account

let activator_account = new_account ()

let find pkh =
  match Signature.Public_key_hash.Table.find known_accounts pkh with
  | Some k ->
      return k
  | None ->
      failwith "Missing account: %a" Signature.Public_key_hash.pp pkh

let find_alternate pkh =
  let exception Found of t in
  try
    Signature.Public_key_hash.Table.iter
      (fun pkh' account ->
        if not (Signature.Public_key_hash.equal pkh pkh') then
          raise (Found account))
      known_accounts ;
    raise Not_found
  with Found account -> account

let find_baker baker =
  let exception Found of baker in
  try
    Baker_table.iter
      (fun _pkh -> function
        | {baker = baker'; _} as account when Baker_hash.equal baker baker' ->
            raise (Found account) | _ -> ())
      known_bakers ;
    raise Not_found
  with
  | Found account ->
      return account
  | Not_found ->
      failwith "Missing baker: %a" Baker_hash.pp baker

let find_alternate_baker baker =
  let exception Found of Baker_hash.t in
  try
    Baker_table.iter
      (fun _pkh -> function
        | {baker = baker'; _} when not (Baker_hash.equal baker baker') ->
            raise (Found baker') | _ -> ())
      known_bakers ;
    raise Not_found
  with Found baker -> baker

let dummy_account = new_account ()

let generate_accounts ?(initial_implicit_balances = [])
    ?(initial_baker_balances = []) n :
    (t * Tez.t) list * (baker * Tez.t) list * Contract.origination_nonce =
  Signature.Public_key_hash.Table.clear known_accounts ;
  Baker_table.clear known_bakers ;
  let default_amount = Tez.of_mutez_exn 4_000_000_000_000L in
  let implicit_amount i =
    match List.nth_opt initial_implicit_balances i with
    | None ->
        default_amount
    | Some a ->
        Tez.of_mutez_exn a
  in
  let baker_amount i =
    match List.nth_opt initial_baker_balances i with
    | None ->
        default_amount
    | Some a ->
        Tez.of_mutez_exn a
  in
  let nonce = Contract.initial_origination_nonce Operation_hash.zero in
  List.fold_left
    (fun (accounts, baker_accounts, nonce) i ->
      let (pkh, pk, sk) = Signature.generate_key () in
      let account = {pkh; pk; sk} in
      Signature.Public_key_hash.Table.add known_accounts pkh account ;
      let (pkh, pk, sk) = Signature.generate_key () in
      let baker_key_account = {pkh; pk; sk} in
      let baker = Contract.baker_from_nonce nonce in
      let baker_account = {key = baker_key_account; baker} in
      Signature.Public_key_hash.Table.add known_accounts pkh baker_key_account ;
      Baker_table.add known_bakers baker baker_account ;
      let nonce = Contract.incr_origination_nonce nonce in
      ( List.append accounts [(account, implicit_amount i)],
        List.append baker_accounts [(baker_account, baker_amount i)],
        nonce ))
    ([], [], nonce)
    (0 -- (n - 1))

let commitment_secret =
  Blinded_public_key_hash.activation_code_of_hex
    "aaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbb"

let new_commitment ?seed () =
  let (pkh, pk, sk) = Signature.generate_key ?seed ~algo:Ed25519 () in
  let unactivated_account = {pkh; pk; sk} in
  let open Commitment in
  let pkh = match pkh with Ed25519 pkh -> pkh | _ -> assert false in
  let bpkh = Blinded_public_key_hash.of_ed25519_pkh commitment_secret pkh in
  Lwt.return
    ( (Environment.wrap_tzresult @@ Tez.(one *? 4_000L))
    >|? fun amount ->
    (unactivated_account, {blinded_public_key_hash = bpkh; amount}) )
