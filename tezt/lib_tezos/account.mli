(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This type is used to construct values for secret keys.

    Note: The tests only use unencrypted keys for the moment, please
    add new constructors for other keys here, as needed. *)
type secret_key =
  | Unencrypted of string
      (** The string does NOT contain the 'encrypted:' prefix *)
  | Encrypted of string
  | Remote of string

(** Keys associated to an account. For example:
{[
    {
      alias = "bootstrap1";
      public_key_hash = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
      public_key = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";
      secret_key =
        Unencrypted "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh";
    }
]} *)
type key = {
  alias : string;
  public_key_hash : string;
  public_key : string;
  secret_key : secret_key;
}

(** A [Check.typ] for [key] *)
val key_typ : key Check.typ

(** [sign_bytes ~watermark ~signer message] signs the bytes [message] with
    [signer]'s secret key. Returns the corresponding Tezos signature. This
    function can be used to sign transactions, blocks, etc. depending on
    the given [watermark].

    Used for regular accounts. *)
val sign_bytes :
  ?watermark:Tezos_crypto.Signature.watermark ->
  signer:key ->
  bytes ->
  Tezos_crypto.Signature.t

(** [require_unencrypted_secret_key ~__LOC__ key] returns [sk] if [key] is [Unencrypted sk], or fails. *)
val require_unencrypted_secret_key : __LOC__:string -> secret_key -> string

val require_unencrypted_or_remote_secret_key :
  __LOC__:string -> secret_key -> unit

(** [uri_of_secret_key secret_key] returns [secret_key] as an URI.

    The URI of a secret key is its contents prefixed [unencrypted:] respectively
    [encrypted:] if it is unencrypted respetively encrypted. *)
val uri_of_secret_key : secret_key -> string

(** A [Check.typ] for [secret_key] *)
val secret_key_typ : secret_key Check.typ

(** [write keys ~base_dir] writes the keys into the [octez-client]'s data
   directory [base_dir]. This function has the same effect
   as importing all the keys manually  via [octez-client] but is
   faster. *)
val write : key list -> base_dir:string -> unit

(** [generate_new_key ~algo ~alias] generates a new key using the given [algo], and
    the Tezos crypto library. Even though an [alias] is given, this key is not
    registered in a client, and must be registered manually.
    Typically, this can be used to generate keys for bootstrap accounts, and include
    them in [overwrite_bootstrap_accounts] when calling [Protocol.write_parameter_file].
    At this point it is possible to specify the delegate and consensus keys of an
    arbitrary number of bootstrap accounts.
    To use these keys in a client and start baking, it is necessary to include them using
    [Client.import_secret_key] (which is usually not needed for the default
    bootstrap accounts). *)
val generate_new_key : algo:Tezos_crypto.Signature.algo -> alias:string -> key

module Bootstrap : sig
  (** Standard name for a bootstrap account parameterised by an
      integer. This alias can be used to name new bootstrap
      accounts. *)
  val alias : int -> string

  (** The default bootstrap keys. *)
  val keys : key array
end

(** [parse_client_output ~alias ~client_output] extracts keys from clients output that
    yields result of the form
{v
      Hash: tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx
      Public Key: edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav
      Secret Key: unencrypted:edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh
v}
    and returns the corresponding key.
*)
val parse_client_output : alias:string -> client_output:string -> key
