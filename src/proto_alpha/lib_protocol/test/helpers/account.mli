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

val known_accounts : t Signature.Public_key_hash.Table.t

val activator_account : account

val dummy_account : account

(** [new_account ?rng_state ?seed ?algo ()] creates a new account with curve
    [algo] with the given [seed] (or [rng_state] to generate the seed) and add
    it to the global account state.
*)
val new_account :
  ?rng_state:Random.State.t ->
  ?seed:Bytes.t ->
  ?algo:Signature.algo ->
  unit ->
  account

val add_account : t -> unit

val find : Signature.Public_key_hash.t -> t tzresult Lwt.t

val find_alternate : Signature.Public_key_hash.t -> t

(** Default full balance of bootstrap accounts: 4_000_000 tez *)
val default_initial_full_balance : Tez.t

(** Default staked balance of bootstrap accounts: 200_000 tez *)
val default_initial_staked_balance : Tez.t

(** Default spendable balance of bootstrap accounts: 3_800_000 tez *)
val default_initial_spendable_balance : Tez.t

(** [generate_accounts ?algo ?rng_state n] first frees the global account state then
    generates [n] random accounts with [rng_state] to generate the seed and adds
    them to the global account state. When provided, all accounts key generation
    will use [algo]. Otherwise uses a random algo.
*)
val generate_accounts :
  ?algo:Signature.algo -> ?rng_state:Random.State.t -> int -> t list tzresult

(** [generate_accounts_with_algo_list ?rng_state algo_list] does the same thing as
    [generate_accounts], but each account can be given a different algorithm. The number
    of accounts is the length of the given list, and if an algo is given as None,
    one is chosen at random.
*)
val generate_accounts_with_algo_list :
  ?rng_state:Random.State.t -> Signature.algo option list -> t list

val commitment_secret : Blinded_public_key_hash.activation_code

val new_commitment :
  ?seed:Bytes.t -> unit -> (account * Commitment.t) tzresult Lwt.t

(** Fails if the contract is not an implicit one  *)
val pkh_of_contract_exn : Contract.t -> Signature.Public_key_hash.t

(** [make_bootstrap_account ~balance ~delegate_to account] creates a
    {!Parameters.bootstrap_account} from an account with the default or set
    values. default [balance] is {!default_initial_full_balance},
    [delegate_to] is [None] and [consensus_key] is [None].
*)
val make_bootstrap_account :
  ?balance:Tez.t ->
  ?delegate_to:Signature.public_key_hash option ->
  ?consensus_key:Signature.public_key option ->
  t ->
  Parameters.bootstrap_account

(** [make_bootstrap_accounts ~bootstrap_balances ~bootstrap_delegations
    ~bootstrap_consensus_keys accounts] combines the lists [accounts],
    [bootstrap_balances], [bootstrap_delegations] and [bootstrap_consensus_keys]
    to create a list of {!Parameters.bootstrap_account} using
    [make_bootstrap_account].
*)
val make_bootstrap_accounts :
  ?bootstrap_balances:int64 list ->
  ?bootstrap_delegations:Signature.public_key_hash option list ->
  ?bootstrap_consensus_keys:Signature.public_key option list ->
  t list ->
  Parameters.bootstrap_account list
