(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

open Tezt_core

(* TODO: https://gitlab.com/tezos/tezos/-/issues/7152
   add ['a tzresult typ] based on [('a, 'e) result typ]
*)

let p2p_peer_id : P2p_peer.Id.t Check.typ =
  Check.comparable P2p_peer.Id.pp P2p_peer.Id.compare
