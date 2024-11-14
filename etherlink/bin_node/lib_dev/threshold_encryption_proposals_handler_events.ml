(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Internal_event.Simple

let section = Events.section

let started =
  declare_0
    ~section
    ~name:"proposal_handler_started"
    ~msg:"Threshold encryption Proposal Handler has been started"
    ~level:Notice
    ()

let shutdown =
  declare_0
    ~section
    ~name:"shutting_down_proposal_handler"
    ~msg:"Stopping the Threshold encryption proposal handler"
    ~level:Notice
    ()

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

let received_proposal_submission_request =
  declare_0
    ~section
    ~name:"received_proposal_submission_request"
    ~msg:"New request to submit a proposal to the DSN node"
    ~level:Debug
    ()

let started = emit started

let shutdown = emit shutdown

let proposal_is_locked = emit proposal_is_locked

let proposal_processed = emit proposal_processed

let received_proposal_submission_request =
  emit received_proposal_submission_request
