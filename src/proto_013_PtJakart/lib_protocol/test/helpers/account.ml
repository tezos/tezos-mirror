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
  Bytes.init Hacl.Ed25519.sk_size (fun _i ->
      Char.chr (Random.State.int rng_state 256))

let new_account ?seed () =
  let pkh, pk, sk = Signature.generate_key ~algo:Ed25519 ?seed () in
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

let generate_accounts ?rng_state ?(initial_balances = []) n : (t * Tez.t) list =
  Signature.Public_key_hash.Table.clear known_accounts ;
  let amount i =
    match List.nth_opt initial_balances i with
    | None -> default_initial_balance
    | Some a -> Tez.of_mutez_exn a
  in
  let rng_state =
    match rng_state with
    | None -> Random.State.make_self_init ()
    | Some state -> state
  in
  List.map
    (fun i ->
      let pkh, pk, sk =
        Signature.generate_key ~algo:Ed25519 ~seed:(random_seed ~rng_state) ()
      in
      let account = {pkh; pk; sk} in
      Signature.Public_key_hash.Table.add known_accounts pkh account ;
      (account, amount i))
    (0 -- (n - 1))

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
