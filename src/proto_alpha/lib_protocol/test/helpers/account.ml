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

type account = t

let known_accounts = Signature.Public_key_hash.Table.create 17

let random_seed ~rng_state =
  Bytes.init Tezos_crypto.Hacl.Ed25519.sk_size (fun _i ->
      Char.chr (Random.State.int rng_state 256))

let random_algo ~rng_state : Signature.algo =
  match Random.State.int rng_state 3 with
  | 0 -> Ed25519
  | 1 -> Secp256k1
  | 2 -> P256
  | 3 -> Bls
  | _ -> assert false

let new_account ?(rng_state = Random.State.make_self_init ())
    ?(seed = random_seed ~rng_state) ?(algo = random_algo ~rng_state) () =
  let pkh, pk, sk = Signature.generate_key ~algo ~seed () in
  let account = {pkh; pk; sk} in
  Signature.Public_key_hash.Table.add known_accounts pkh account ;
  account

let add_account ({pkh; _} as account) =
  Signature.Public_key_hash.Table.add known_accounts pkh account

let activator_account =
  let seed = random_seed ~rng_state:(Random.State.make [|0x1337533D|]) in
  new_account ~seed ()

let find pkh =
  match Signature.Public_key_hash.Table.find known_accounts pkh with
  | Some k -> return k
  | None -> failwith "Missing account: %a" Signature.Public_key_hash.pp pkh

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

let dummy_account =
  let seed =
    random_seed ~rng_state:(Random.State.make [|0x1337533D; 0x1337533D|])
  in
  new_account ~seed ()

let default_initial_balance = Tez.of_mutez_exn 4_000_000_000_000L

let generate_accounts ?rng_state n : t list tzresult =
  Signature.Public_key_hash.Table.clear known_accounts ;
  List.init ~when_negative_length:[] n (fun _i -> new_account ?rng_state ())

let commitment_secret =
  Blinded_public_key_hash.activation_code_of_hex
    "aaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbb"
  |> WithExceptions.Option.get ~loc:__LOC__

let new_commitment ?seed () =
  let pkh, pk, sk = Signature.generate_key ?seed ~algo:Ed25519 () in
  let unactivated_account = {pkh; pk; sk} in
  let open Commitment in
  let pkh = match pkh with Ed25519 pkh -> pkh | _ -> assert false in
  let bpkh = Blinded_public_key_hash.of_ed25519_pkh commitment_secret pkh in
  Lwt.return
    ( (Environment.wrap_tzresult @@ Tez.(one *? 4_000L)) >|? fun amount ->
      (unactivated_account, {blinded_public_key_hash = bpkh; amount}) )

let pkh_of_contract_exn = function
  | Contract.Implicit pkh -> pkh
  | Originated _ -> assert false

let make_bootstrap_account ?(balance = default_initial_balance)
    ?(delegate_to = None) ?(consensus_key = None) account =
  Parameters.
    {
      public_key_hash = account.pkh;
      public_key = Some account.pk;
      amount = balance;
      delegate_to;
      consensus_key;
    }

let rec make_bootstrap_accounts ?(bootstrap_balances = [])
    ?(bootstrap_delegations = []) ?(bootstrap_consensus_keys = []) accounts =
  let decons_of_opt = function x :: xs -> (x, xs) | [] -> (None, []) in
  let decons = function x :: xs -> (Some x, xs) | [] -> (None, []) in
  match accounts with
  | account :: accounts ->
      let balance, bootstrap_balances = decons bootstrap_balances in
      let delegate_to, bootstrap_delegations =
        decons_of_opt bootstrap_delegations
      in
      let consensus_key, bootstrap_consensus_keys =
        decons_of_opt bootstrap_consensus_keys
      in
      make_bootstrap_account
        ?balance:(Option.map Tez.of_mutez_exn balance)
        ~delegate_to
        ~consensus_key
        account
      :: make_bootstrap_accounts
           ~bootstrap_balances
           ~bootstrap_delegations
           ~bootstrap_consensus_keys
           accounts
  | [] -> []
