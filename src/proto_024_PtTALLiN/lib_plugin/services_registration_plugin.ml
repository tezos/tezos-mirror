(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/7369 *)

open Environment

let patched_services =
  ref (RPC_directory.empty : Updater.rpc_context RPC_directory.t)

let register0_fullctxt ~chunked s f =
  let open Lwt_result_syntax in
  patched_services :=
    RPC_directory.register ~chunked !patched_services s (fun ctxt q i ->
        let* ctxt = Services_registration.rpc_init ctxt `Head_level in
        f ctxt q i)

let register0 ~chunked s f =
  register0_fullctxt ~chunked s (fun {context; _} -> f context)

let register0_fullctxt_successor_level ~chunked s f =
  let open Lwt_result_syntax in
  patched_services :=
    RPC_directory.register ~chunked !patched_services s (fun ctxt q i ->
        let mode =
          if q#successor_level then `Successor_level else `Head_level
        in
        let* ctxt = Services_registration.rpc_init ctxt mode in
        f ctxt q i)

let register0_successor_level ~chunked s f =
  register0_fullctxt_successor_level ~chunked s (fun {context; _} -> f context)

let register0_noctxt ~chunked s f =
  patched_services :=
    RPC_directory.register ~chunked !patched_services s (fun _ q i -> f q i)

let opt_register0_fullctxt ~chunked s f =
  let open Lwt_result_syntax in
  patched_services :=
    RPC_directory.opt_register ~chunked !patched_services s (fun ctxt q i ->
        let* ctxt = Services_registration.rpc_init ctxt `Head_level in
        f ctxt q i)

let opt_register0 ~chunked s f =
  opt_register0_fullctxt ~chunked s (fun {context; _} -> f context)

let register1_fullctxt ~chunked s f =
  let open Lwt_result_syntax in
  patched_services :=
    RPC_directory.register ~chunked !patched_services s (fun (ctxt, arg) q i ->
        let* ctxt = Services_registration.rpc_init ctxt `Head_level in
        f ctxt arg q i)

let opt_register1_fullctxt ~chunked s f =
  let open Lwt_result_syntax in
  patched_services :=
    RPC_directory.opt_register
      ~chunked
      !patched_services
      s
      (fun (ctxt, arg) q i ->
        let* ctxt = Services_registration.rpc_init ctxt `Head_level in
        f ctxt arg q i)

let register1 ~chunked s f =
  register1_fullctxt ~chunked s (fun {context; _} x -> f context x)

let opt_register1 ~chunked s f =
  opt_register1_fullctxt ~chunked s (fun {context; _} x -> f context x)

let register2_fullctxt ~chunked s f =
  let open Lwt_result_syntax in
  patched_services :=
    RPC_directory.register
      ~chunked
      !patched_services
      s
      (fun ((ctxt, arg1), arg2) q i ->
        let* ctxt = Services_registration.rpc_init ctxt `Head_level in
        f ctxt arg1 arg2 q i)

let register2 ~chunked s f =
  register2_fullctxt ~chunked s (fun {context; _} a1 a2 q i ->
      f context a1 a2 q i)

let opt_register2_fullctxt ~chunked s f =
  let open Lwt_result_syntax in
  patched_services :=
    RPC_directory.opt_register
      ~chunked
      !patched_services
      s
      (fun ((ctxt, arg1), arg2) q i ->
        let* ctxt = Services_registration.rpc_init ctxt `Head_level in
        f ctxt arg1 arg2 q i)

let opt_register2 ~chunked s f =
  opt_register2_fullctxt ~chunked s (fun {context; _} a1 a2 q i ->
      f context a1 a2 q i)

let register3_fullctxt ~chunked s f =
  let open Lwt_result_syntax in
  patched_services :=
    RPC_directory.register
      ~chunked
      !patched_services
      s
      (fun (((ctxt, arg1), arg2), arg3) q i ->
        let* ctxt = Services_registration.rpc_init ctxt `Head_level in
        f ctxt arg1 arg2 arg3 q i)

let register3 ~chunked s f =
  register3_fullctxt ~chunked s (fun {context; _} a1 a2 a3 q i ->
      f context a1 a2 a3 q i)
