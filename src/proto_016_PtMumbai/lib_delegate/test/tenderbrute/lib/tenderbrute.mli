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

(** The type of desired delegate selection. For each level, and each round,
    one can provide a public key hash that would be the proposer. All non-
    specified level and rounds are not constrained. *)
type delegate_selection =
  (Raw_level_repr.t * (Round_repr.t * Signature.public_key_hash) list) list

(** Brute-force an initial seed nonce for the desired delegate selection.
    When found, the seed nonce is returned as a byte sequence of size 32. If
    no nonce is necessary to obtain the desired selection, [None] is returned
    (there won't be any brute-forcing then).

    {b Note}: When using this function in your tests, take care of saving the
    nonce once it is found locallty (in this case call {!check_seed_nonce} to
    ensure it yields the desired selection) to avoid unnecessary computation
    (in particular this function should not be called in the CI).

    @param show_progress if [true], display a spinner and the number of iterations
      on the output (stderr) while [brutefore] is running.
    @param random_seed initialize OCaml's random number generator with this seed
      (by default [0]). This is useful to spawn multiple bruteforce in
      parallel or on multiple machines.
    @param constants_overrides_json JSON representation of some constants that are
      to be overwritten for the in-memory context. The most useful change is to
      set the size of cycles (which directly impacts the attribution of rights) or
      the committee size with {i e.g.}
      {[ { "blocks_per_cycle" : 8, "consensus_committee_size" : 25 } ]}
    @param bootstrap_accounts_json JSON representation of bootstrap accounts of
      the form:
      {[
       [ { "name" : "bootstrap1"
           "sk_uri": "unencrypted:edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh",
           "amount": "3000000000000" },
         ...]
      ]}
    @param parameters parameters of the protocol (optional).
    @param selection is the desired delegate selection. See {!delegate_selection}.
 *)
val bruteforce :
  ?show_progress:bool ->
  ?random_seed:int ->
  ?max:int ->
  ?parameters:Mockup.Protocol_parameters.t ->
  ?constants_overrides_json:json ->
  ?bootstrap_accounts_json:json ->
  delegate_selection ->
  State_hash.t option tzresult Lwt.t

(** Check that an initial seed nonce yields to the desired delegate selection.
    See {!bruteforce} for the arguments. *)
val check_seed :
  ?parameters:Mockup.Protocol_parameters.t ->
  ?constants_overrides_json:json ->
  ?bootstrap_accounts_json:json ->
  seed:State_hash.t option ->
  delegate_selection ->
  bool tzresult Lwt.t
