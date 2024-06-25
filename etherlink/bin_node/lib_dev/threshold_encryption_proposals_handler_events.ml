(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Internal_event.Simple

let section = Events.section

let proposal_is_locked =
  declare_0
    ~section
    ~name:"proposal_is_locked"
    ~msg:"Last proposal did not produce a preblock yet. Proposal is locked."
    ~level:Debug
    ()

let proposal_processed =
  declare_0
    ~section
    ~name:"proposal_processed"
    ~msg:"Proposal has been processed"
    ~level:Debug
    ()

let proposal_is_locked = emit proposal_is_locked

let proposal_processed = emit proposal_processed
