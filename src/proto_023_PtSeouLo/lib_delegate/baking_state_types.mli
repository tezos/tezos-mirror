(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** {2 Key type and functions}

    Used for both consensus keys and companion keys. *)
module Key_id : sig
  type t

  (** Only use at library frontiers *)
  val to_pkh : t -> Signature.public_key_hash

  val compare : t -> t -> int

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  module Table : sig
    include Hashtbl.SeededS with type key = t

    val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
  end
end

module Key : sig
  type t = private {
    alias : string option;
    id : Key_id.t;
    public_key : Signature.public_key;
    secret_key_uri : Client_keys.sk_uri;
  }

  val make :
    alias:string option ->
    public_key:Signature.public_key ->
    public_key_hash:Signature.public_key_hash ->
    secret_key_uri:Client_keys.sk_uri ->
    t

  (** Partial encoding for {!t} that omits the secret key to avoid
      leaking it in event logs (because {!Client_keys.sk_uri} contains
      the plain secret key when the key is unencrypted).

      Warning: As a consequence, decoding from this encoding will
      always fail. *)
  val encoding_for_logging__cannot_decode : t Data_encoding.t

  val pp : Format.formatter -> t -> unit
end

(** {2 Delegates slots type and functions} *)
module Delegate_id : sig
  type t

  (** Only use at library frontiers *)
  val of_pkh : Signature.public_key_hash -> t

  (** Only use at library frontiers *)
  val to_pkh : t -> Signature.public_key_hash

  val equal : t -> t -> bool

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit
end

module Delegate : sig
  type t = {
    consensus_key : Key.t;
    companion_key : Key.t option;
    delegate_id : Delegate_id.t;
  }

  (** Partial encoding for {!t} that omits secret keys to avoid
      leaking them in event logs; see
      {!Key.encoding_for_logging__cannot_decode}.

      Warning: As a consequence, decoding from this encoding will
      always fail. *)
  val encoding_for_logging__cannot_decode : t Data_encoding.t

  val pp : Format.formatter -> t -> unit
end
