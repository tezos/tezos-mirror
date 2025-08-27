(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [set_all_bakers_attest_first_level ctxt] reads the stored value in
    {!Storage.All_bakers_attest_activation}, and if not [None], puts it in
    the context back as [all_bakers_attest_first_level]. *)
val set_all_bakers_attest_first_level :
  Raw_context.t -> Raw_context.t tzresult Lwt.t
