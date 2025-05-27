(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

type t

val create : P2p.config * P2p_limits.t -> (t, tztrace) result Lwt.t

val shutdown : t -> unit Lwt.t
