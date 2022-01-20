(* The MIT License (MIT)
 *
 * Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE. *)

include Rustzcash_sig.T

val max_amount : int64

(** In principle the definition of a valid position depends on the data
    structure that holds it, so it should be defined in storage.ml. However
    librustzcash imposes a particular tree structure of depth 32, so we
    hardcode it here. *)
val valid_position : int64 -> bool

val valid_amount : int64 -> bool

val valid_balance : int64 -> bool

(** Raised by {!init_params} if it failed to find parameters.

    The string list is the list of locations where they were looked up. *)
exception Params_not_found of string list

(** Location of parameter files for our instance of Groth16.

    We are only using and loading sapling parameters. *)
type parameter_files = {spend_path : string; output_path : string}

(** Find parameter files.

    The parameters are searched in:
    - [$XDG_DATA_HOME/.local/share/zcash-params];
    - [$XDG_DATA_DIRS/zcash-params] (splitting on the [:] character);
    - [$OPAM_SWITCH_PREFIX/share/zcash-params];
    - [_opam/share/zcash-params];
    - [$HOME/.zcash-params];
    - [$HOME/.local/share/zcash-params];
    - [/usr/local/share/zcash-params];
    - [/usr/share/zcash-params];
    in this order.

    This function uses [getenv_opt], [getcwd] and [file_exists] from the [Sys]
    module. You can use the corresponding optional arguments to override their
    behavior, for instance with a mock for testing purposes.

    @raise Params_not_found if parameters could not be found
    at any of those locations. *)
val find_params :
  ?getenv_opt:(string -> string option) ->
  ?getcwd:(unit -> string) ->
  ?file_exists:(string -> bool) ->
  unit ->
  parameter_files

(** Load parameter files.

    @raise Params_not_found if parameters could not be found
    (see {!find_params} for information regarding how parameter files
    are looked up). *)
val init_params : unit -> unit

(** Derives the nullifier pk corresponding to a nullifier sk *)
val nsk_to_nk : nsk -> nk

(** Derives the spending/signing pk corresponding to a secret spending/signing sk *)
val ask_to_ak : ask -> ak

(** Derives the incoming viewing key *)
val crh_ivk : ak -> nk -> ivk

(** Checks that a potential diversifier respects the needed properties *)
val check_diversifier : diversifier -> bool

(** Computes a diversified pk that the payee gives to the payer offline.

    @raise Assert_failure if the underlying binding in rust indicates a failure,
    which only happens if the arguments are not valid representations for their
    expected type, which can only happen if there's an error in the constructors
    for these abstract types. *)
val ivk_to_pkd : ivk -> diversifier -> pkd

(** Gives a random scalar *)
val generate_r : unit -> Bytes.t

(** Computes a nullifier. The first int64 is the amount of the note, the second
    is the position you want it inserted in.
    The rcm should be the same as the one to compute cm and the spend or output
    proof, and should be generated using generate_r.

    @raise Assert_failure if the underlying binding in rust indicates a failure,
    which only happens if the arguments are not valid representations for their
    expected type, which can only happen if there's an error in the constructors
    for these abstract types. *)
val compute_nf :
  diversifier ->
  pkd ->
  amount:int64 ->
  rcm ->
  ak ->
  nk ->
  position:int64 ->
  nullifier

(** Computes a commitment. The int64 is the amount, and the rcm is the same than
    for the nullifier and output or spend proof. It should be generated at
    random using generate_r.

    @raise Assert_failure if the underlying binding in rust indicates a failure,
    which only happens if the arguments are not valid representations for their
    expected type, which can only happen if there's an error in the constructors
    for these abstract types. *)
val compute_cm : diversifier -> pkd -> amount:int64 -> rcm -> commitment

(** Computes the shared secret of a Diffie Hellman key exchange (on the JubJub
    curve) with base depending on the diversifier.
    For the sender the epk is the pkd of the receiver, the esk was generated by
    him using generate_r.
    For the receiver the epk is the one published by the sender, and the secret is
    his ivk.

    @raise Assert_failure if the underlying binding in rust indicates a failure,
    which only happens if the arguments are not valid representations for their
    expected type, which can only happen if there's an error in the constructors
    for these abstract types. *)
val ka_agree_sender : pkd -> esk -> symkey

val ka_agree_receiver : epk -> ivk -> symkey

(** Computes the ephemeral pk from the ephemeral sk for a Diffie Hellman key
    exchange. This is used by the sender. The esk should be generated using
    generate_r

    @raise Assert_failure if the underlying binding in rust indicates a failure,
    which only happens if the arguments are not valid representations for their
    expected type, which can only happen if there's an error in the constructors
    for these abstract types. *)
val ka_derivepublic : diversifier -> esk -> epk

(** Creates the spend sig for an input. The sighash argument is the hash of
    the input ie. cv,cm,...
    This has to be generated using [generate_r]

    @raise Assert_failure if the underlying binding in rust indicates a failure,
    which only happens if the arguments are not valid representations for their
    expected type, which can only happen if there's an error in the constructors
    for these abstract types. *)
val spend_sig : ask -> ar -> sighash -> spend_sig

type proving_ctx

(** Creates and frees a proving context. The proving context has to be created
    before creating proofs for inputs and outputs.
    It is then used to create the binding sig, and freed.
    It is a rust pointer to a scalar and an elliptic curve point *)
val proving_ctx_init : unit -> proving_ctx

val proving_ctx_free : proving_ctx -> unit

(** Evaluates a function that needs a proving context. This function takes
    care of allocating and freeing it. The context should not escape the scope
    of the given function and should not be freed during its execution. *)
val with_proving_ctx : (proving_ctx -> 'a) -> 'a

(** Creates the binding signature for a transaction. It is effectively
    a zk proof that the sum of the amounts of a list of inputs and
    outputs is the same as the given balance. The necessary information
    is stored in the proving context when creating the proofs for
    inputs and outputs. The proving context has to be freed after
    calling this function.

    @raise Assert_failure if the underlying binding in rust indicates a failure,
    which only happens if the arguments are not valid representations for their
    expected type, which can only happen if there's an error in the constructors
    for these abstract types. *)
val make_binding_sig : proving_ctx -> balance:int64 -> sighash -> binding_sig

(** Creates proof and sig for an output

    @raise Assert_failure if the underlying binding in rust indicates a failure,
    which only happens if the arguments are not valid representations for their
    expected type, which can only happen if there's an error in the constructors
    for these abstract types. *)
val output_proof :
  proving_ctx ->
  esk ->
  diversifier ->
  pkd ->
  rcm ->
  amount:int64 ->
  cv * output_proof

(** Creates the zk proof and sig for an input.
    The first is the same as the one for the commitment and nullifier.
    The second one is the same as for the binding sig.
    This function can panic (e.g. if the arguments are not coherent).

    @raise Assert_failure if the underlying binding in rust indicates a failure,
    which only happens if the arguments are not valid representations for their
    expected type, which can only happen if there's an error in the constructors
    for these abstract types. *)
val spend_proof :
  proving_ctx ->
  ak ->
  nsk ->
  diversifier ->
  rcm ->
  ar ->
  amount:int64 ->
  root:hash ->
  witness:Bytes.t ->
  cv * rk * spend_proof

(** Reduces mod r_j, takes a 64 bytes input*)
val to_scalar : Bytes.t -> Bytes.t

type verification_ctx

(** Creates and frees a verifying context. The proving context has to be created
    before verifying proofs the inputs and outputs.
    It is then used to verify the binding sig, and freed.
    It is a rust pointer to an elliptic curve point *)
val verification_ctx_init : unit -> verification_ctx

val verification_ctx_free : verification_ctx -> unit

(** Evaluates a function that needs a verification context. This function takes
    care of allocating and freeing it. The context should not escape the scope
    of the given function and should not be freed during its execution. *)
val with_verification_ctx : (verification_ctx -> 'a) -> 'a

val check_output :
  verification_ctx -> cv -> commitment -> epk -> output_proof -> bool

val check_spend :
  verification_ctx ->
  cv ->
  hash ->
  nullifier ->
  rk ->
  spend_proof ->
  spend_sig ->
  sighash ->
  bool

val merkle_hash : height:int -> hash -> hash -> hash

val tree_uncommitted : hash

val final_check : verification_ctx -> int64 -> binding_sig -> sighash -> bool

val zip32_xsk_master : Bytes.t -> zip32_expanded_spending_key

val zip32_xfvk_address :
  zip32_full_viewing_key ->
  diversifier_index ->
  (diversifier_index * diversifier * pkd) option

val zip32_xsk_derive :
  zip32_expanded_spending_key -> Int32.t -> zip32_expanded_spending_key

(* @raise Assert_failure if the underlying binding in rust indicates a failure,
   which only happens if the arguments are not valid representations for their
   expected type, which can only happen if there's an error in the constructors
   for these abstract types. *)
val zip32_xfvk_derive :
  zip32_full_viewing_key -> Int32.t -> zip32_full_viewing_key
