(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  sync_fun : unit -> unit Lwt.t;
  checkout_fun :
    Context_hash.t -> Tezos_protocol_environment.Context.t option Lwt.t;
  finalize_fun : unit -> unit Lwt.t;
}

let abstract index =
  {
    sync_fun = (fun () -> Context_ops.sync index);
    checkout_fun = Context_ops.checkout index;
    finalize_fun = (fun () -> Context_ops.close index);
  }
