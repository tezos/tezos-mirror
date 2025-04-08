(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* THIS IS THE ENTRYPOINT *)
val register_tezlink_services :
  (module Services_backend_sig.S) -> Evm_directory.t
