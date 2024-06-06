(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

val tzresult :
  'a Tezt_core.Check.typ -> 'a Error_monad.tzresult Tezt_core.Check.typ

val p2p_peer_id : P2p_peer.Id.t Tezt_core.Check.typ
