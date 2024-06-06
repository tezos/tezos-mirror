(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

open Tezt_core

let tztrace : Error_monad.tztrace Check.typ =
  Check.comparable
    Error_monad.pp_print_trace
    (List.compare (fun x y ->
         Stdlib.String.compare
           (Error_monad.find_info_of_error x).id
           (Error_monad.find_info_of_error y).id))

let tzresult (type a) (t : a Check.typ) : a Error_monad.tzresult Check.typ =
  Check.result t tztrace

let p2p_peer_id : P2p_peer.Id.t Check.typ =
  Check.comparable P2p_peer.Id.pp P2p_peer.Id.compare
