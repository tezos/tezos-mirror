(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
(*                                                                           *)
(*****************************************************************************)

module Block_services =
  Rollup_node_services.Make_services (Sc_rollup_services.Block)

let get_sc_rollup_addresses_command cctxt =
  Rollup_node_services.Global.(make_call sc_rollup_address) cctxt () () ()

let get_state_value_command cctxt block key =
  Block_services.(make_call1 Sc_rollup_services.Block.state_value)
    cctxt
    block
    {key}
    ()

let get_outbox_proof cctxt serialized_output =
  Block_services.(make_call Sc_rollup_services.Block.Helpers.outbox_proof)
    cctxt
    ((), `Head)
    serialized_output
    ()

let get_outbox_proof_simple cctxt outbox_level message_index =
  Block_services.(
    make_call2 Sc_rollup_services.Block.Helpers.outbox_proof_simple)
    cctxt
    `Head
    outbox_level
    message_index
    ()

let get_outbox cctxt block level =
  Block_services.(make_call1 Sc_rollup_services.Block.outbox)
    cctxt
    block
    level
    ()
