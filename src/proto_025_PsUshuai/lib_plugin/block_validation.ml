(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type block_validation_state = unit

let init_block_validation_state _ = ()

let check_block_operation () _ = Lwt_result_syntax.return ()
