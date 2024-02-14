(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module type PROTOCOL = sig
  type context

  type contract

  module Tez : sig
    type t

    val zero : t

    val compare : t -> t -> int

    val ( +? ) : t -> t -> t tzresult

    val to_mutez : t -> int64
  end

  module Signature : sig
    type public_key_hash

    type public_key

    type secret_key

    type signature

    module To_latest : sig
      val public_key_hash :
        public_key_hash -> Tezos_crypto.Signature.V_latest.public_key_hash

      val public_key : public_key -> Tezos_crypto.Signature.V_latest.public_key

      val secret_key : secret_key -> Tezos_crypto.Signature.V_latest.secret_key

      val signature : signature -> Tezos_crypto.Signature.V_latest.signature
    end
  end

  module Commitment : sig
    type t

    val fold :
      context ->
      order:[`Sorted | `Undefined] ->
      init:'a ->
      f:(t -> int64 -> 'a -> 'a Lwt.t) ->
      'a Lwt.t
  end

  module Contract : sig
    val fold : context -> init:'a -> f:('a -> contract -> 'a Lwt.t) -> 'a Lwt.t

    val balance : context -> contract -> Tez.t tzresult Lwt.t

    val frozen_bonds : context -> contract -> Tez.t tzresult Lwt.t

    val contract_address : contract -> string

    val get_staked_balance : context -> contract -> Tez.t option tzresult Lwt.t

    val get_unstaked_frozen_balance :
      context -> contract -> Tez.t option tzresult Lwt.t

    val get_unstaked_finalizable_balance :
      context -> contract -> Tez.t option tzresult Lwt.t

    val get_full_balance : context -> contract -> Tez.t tzresult Lwt.t

    val total_supply : context -> Tez.t tzresult Lwt.t
  end

  module Delegate : sig
    val fold :
      context ->
      order:[`Sorted | `Undefined] ->
      init:'a ->
      f:(Signature.public_key_hash -> 'a -> 'a Lwt.t) ->
      'a Lwt.t

    val pubkey :
      context ->
      Signature.public_key_hash ->
      Signature.public_key tzresult Lwt.t

    val staking_balance :
      context -> Signature.public_key_hash -> Tez.t tzresult Lwt.t

    val current_frozen_deposits :
      context -> Signature.public_key_hash -> Tez.t tzresult Lwt.t

    val unstaked_frozen_deposits :
      context -> Signature.public_key_hash -> Tez.t tzresult Lwt.t

    val deactivated :
      context -> Signature.public_key_hash -> bool tzresult Lwt.t
  end

  val hash : Protocol_hash.t

  val prepare_context :
    Tezos_protocol_environment.Context.t ->
    level:int32 ->
    predecessor_timestamp:Time.Protocol.t ->
    timestamp:Time.Protocol.t ->
    context tzresult Lwt.t
end

type protocol = (module PROTOCOL)
