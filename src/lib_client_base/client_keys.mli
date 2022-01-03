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

(** {2 Cryptographic keys tables } *)

type pk_uri = private Uri.t

module Pk_uri_hashtbl : Hashtbl.S with type key = pk_uri

type sk_uri = private Uri.t

type sapling_uri = private Uri.t

type pvss_sk_uri = private Uri.t

type aggregate_pk_uri = private Uri.t

type aggregate_sk_uri = private Uri.t

val pk_uri_parameter : unit -> (pk_uri, 'a) Clic.parameter

val pk_uri_param :
  ?name:string ->
  ?desc:string ->
  ('a, 'b) Clic.params ->
  (pk_uri -> 'a, 'b) Clic.params

val sk_uri_parameter : unit -> (sk_uri, 'a) Clic.parameter

val sk_uri_param :
  ?name:string ->
  ?desc:string ->
  ('a, 'b) Clic.params ->
  (sk_uri -> 'a, 'b) Clic.params

type error += Unregistered_key_scheme of string

type error += Invalid_uri of Uri.t

module Public_key_hash :
  Client_aliases.Alias with type t = Signature.Public_key_hash.t

module Public_key :
  Client_aliases.Alias with type t = pk_uri * Signature.Public_key.t option

module Secret_key : Client_aliases.Alias with type t = sk_uri

type sapling_key = {
  sk : sapling_uri;
  (* zip32 derivation path *)
  path : int32 list;
  (* index of the last issued address *)
  address_index : Tezos_sapling.Core.Client.Viewing_key.index;
}

module Sapling_key : Client_aliases.Alias with type t = sapling_key

module PVSS_public_key :
  Client_aliases.Alias with type t = Pvss_secp256k1.Public_key.t

module PVSS_secret_key : Client_aliases.Alias with type t = pvss_sk_uri

module Logging : sig
  val tag : string Tag.def
end

(** {2 Interface for external signing modules.} *)

module type COMMON_SIGNER = sig
  type pk_uri = private Uri.t

  type sk_uri = private Uri.t

  type public_key_hash

  type public_key

  type secret_key

  (** [scheme] is the name of the scheme implemented by this signer
      module. *)
  val scheme : string

  (** [title] is a one-line human readable description of the signer. *)
  val title : string

  (** [description] is a multi-line human readable description of the
      signer, that should include the format of key specifications. *)
  val description : string

  (** [neuterize sk] is the corresponding [pk]. *)
  val neuterize : sk_uri -> pk_uri tzresult Lwt.t

  (** [import_secret_key ~io pk] is the function to be called when
      interactively importing a key-pair and returning the public key
      and its hash.

      Some signer implementations improve long-term security by
      requiring human/manual validation while importing keys, the
      [~io] argument can be used to prompt the user in such case. *)
  val import_secret_key :
    io:Client_context.io_wallet ->
    pk_uri ->
    (public_key_hash * public_key option) tzresult Lwt.t

  (** [public_key pk] is the Ed25519 version of [pk].*)
  val public_key : pk_uri -> public_key tzresult Lwt.t

  (** [public_key_hash pk] is the hash of [pk].
      As some signers will query the full public key to obtain the hash,
      it can be optionally returned to reduce the amount of queries. *)
  val public_key_hash :
    pk_uri -> (public_key_hash * public_key option) tzresult Lwt.t
end

(** [Signature_type] is a small module to be included in signer to conform to
    the module type [SIGNER] instead of rewriting all type. *)
module Signature_type : sig
  type public_key_hash = Signature.Public_key_hash.t

  type public_key = Signature.Public_key.t

  type secret_key = Signature.Secret_key.t

  type nonrec pk_uri = pk_uri

  type nonrec sk_uri = sk_uri
end

module Aggregate_type : sig
  type public_key_hash = Aggregate_signature.Public_key_hash.t

  type public_key = Aggregate_signature.Public_key.t

  type secret_key = Aggregate_signature.Secret_key.t

  type pk_uri = aggregate_pk_uri

  type sk_uri = aggregate_sk_uri
end

module type SIGNER = sig
  include
    COMMON_SIGNER
      with type public_key_hash = Signature.Public_key_hash.t
       and type public_key = Signature.Public_key.t
       and type secret_key = Signature.Secret_key.t
       and type pk_uri = pk_uri
       and type sk_uri = sk_uri

  (** [sign ?watermark sk data] is signature obtained by signing [data] with
        [sk]. *)
  val sign :
    ?watermark:Signature.watermark ->
    sk_uri ->
    Bytes.t ->
    Signature.t tzresult Lwt.t

  (** [deterministic_nonce sk data] is a nonce obtained
      deterministically from [data] and [sk]. *)
  val deterministic_nonce : sk_uri -> Bytes.t -> Bytes.t tzresult Lwt.t

  (** [deterministic_nonce_hash sk data] is a nonce hash obtained
      deterministically from [data] and [sk]. *)
  val deterministic_nonce_hash : sk_uri -> Bytes.t -> Bytes.t tzresult Lwt.t

  (** [supports_deterministic_nonces] indicates whether the
      [deterministic_nonce] functionality is supported. *)
  val supports_deterministic_nonces : sk_uri -> bool tzresult Lwt.t
end

module type AGGREGATE_SIGNER = sig
  include
    COMMON_SIGNER
      with type public_key_hash = Aggregate_signature.Public_key_hash.t
       and type public_key = Aggregate_signature.Public_key.t
       and type secret_key = Aggregate_signature.Secret_key.t
       and type pk_uri = aggregate_pk_uri
       and type sk_uri = aggregate_sk_uri

  (** [sign sk data] is signature obtained by signing [data] with [sk]. *)
  val sign : aggregate_sk_uri -> Bytes.t -> Aggregate_signature.t tzresult Lwt.t
end

type signer =
  | Simple of (module SIGNER)
  | Aggregate of (module AGGREGATE_SIGNER)

(** [register_signer signer] registers first-class module [signer] as
    signer for keys with scheme [(val signer : SIGNER).scheme]. *)
val register_signer : (module SIGNER) -> unit

val registered_signers : unit -> (string * signer) list

(** [register_aggregate_signer signer] registers first-class module [signer] as
    signer for keys with scheme [(val signer : AGGREGATE_SIGNER).scheme]. *)
val register_aggregate_signer : (module AGGREGATE_SIGNER) -> unit

val import_secret_key :
  io:Client_context.io_wallet ->
  pk_uri ->
  (Signature.Public_key_hash.t * Signature.Public_key.t option) tzresult Lwt.t

val public_key : pk_uri -> Signature.Public_key.t tzresult Lwt.t

val public_key_hash :
  pk_uri ->
  (Signature.Public_key_hash.t * Signature.Public_key.t option) tzresult Lwt.t

val neuterize : sk_uri -> pk_uri tzresult Lwt.t

val sign :
  #Client_context.wallet ->
  ?watermark:Signature.watermark ->
  sk_uri ->
  Bytes.t ->
  Signature.t tzresult Lwt.t

val append :
  #Client_context.wallet ->
  ?watermark:Signature.watermark ->
  sk_uri ->
  Bytes.t ->
  Bytes.t tzresult Lwt.t

val check :
  ?watermark:Signature.watermark ->
  pk_uri ->
  Signature.t ->
  Bytes.t ->
  bool tzresult Lwt.t

val deterministic_nonce : sk_uri -> Bytes.t -> Bytes.t tzresult Lwt.t

val deterministic_nonce_hash : sk_uri -> Bytes.t -> Bytes.t tzresult Lwt.t

val supports_deterministic_nonces : sk_uri -> bool tzresult Lwt.t

val register_key :
  #Client_context.wallet ->
  ?force:bool ->
  Signature.Public_key_hash.t * pk_uri * sk_uri ->
  ?public_key:Signature.Public_key.t ->
  string ->
  unit tzresult Lwt.t

(** Similar to repeated calls to [register_key], but is more efficient.
    Always forces addition of new elements. *)
val register_keys :
  #Client_context.wallet ->
  (string
  * Signature.Public_key_hash.t
  * Signature.public_key
  * pk_uri
  * sk_uri)
  list ->
  unit tzresult Lwt.t

val list_keys :
  #Client_context.wallet ->
  (string * Public_key_hash.t * Signature.public_key option * sk_uri option)
  list
  tzresult
  Lwt.t

val alias_keys :
  #Client_context.wallet ->
  string ->
  (Public_key_hash.t * Signature.public_key option * sk_uri option) option
  tzresult
  Lwt.t

val get_key :
  #Client_context.wallet ->
  Public_key_hash.t ->
  (string * Signature.Public_key.t * sk_uri) tzresult Lwt.t

val get_public_key :
  #Client_context.wallet ->
  Public_key_hash.t ->
  (string * Signature.Public_key.t) tzresult Lwt.t

val get_keys :
  #Client_context.wallet ->
  (string * Public_key_hash.t * Signature.Public_key.t * sk_uri) list tzresult
  Lwt.t

val force_switch : unit -> (bool, 'ctx) Clic.arg

(**/**)

val make_pk_uri : Uri.t -> pk_uri tzresult

val make_sk_uri : Uri.t -> sk_uri tzresult

val make_aggregate_pk_uri : Uri.t -> aggregate_pk_uri tzresult

val make_aggregate_sk_uri : Uri.t -> aggregate_sk_uri tzresult

val make_sapling_uri : Uri.t -> sapling_uri tzresult

val make_pvss_sk_uri : Uri.t -> pvss_sk_uri tzresult
