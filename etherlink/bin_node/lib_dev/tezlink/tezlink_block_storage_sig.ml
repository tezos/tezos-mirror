(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type S = sig
  (** [nth_block n] returns the [n]th processed and stored tez block. *)
  val nth_block : Z.t -> L2_types.Tezos_block.t tzresult Lwt.t

  (** [nth_block_hash n] returns the hash of the [n]th processed and stored block. *)
  val nth_block_hash : Z.t -> Ethereum_types.block_hash option tzresult Lwt.t
end
