(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

include Internal_event.Simple

let section = Events.section @ ["gcp_kms"]

let is_ready =
  declare_1
    ~section
    ~name:"gcp_kms_is_ready"
    ~msg:"GCP KMS handler for {public_key} is ready"
    ~pp1:Signature.Public_key.pp
    ~level:Notice
    ("public_key", Signature.Public_key.encoding)

let new_token =
  declare_0
    ~section
    ~name:"new_access_token"
    ~msg:"successfully fetched a new access token"
    ~level:Info
    ()

let invalidated_token =
  declare_0
    ~section
    ~name:"invalidated_token"
    ~msg:"authentication token was invalidated before its expired date"
    ~level:Warning
    ()

let cannot_refresh_access_token =
  declare_1
    ~section
    ~name:"cannot_refresh_token"
    ~msg:"cannot refresh the access token for the GCP KMS: {trace}"
    ~level:Fatal
    ~pp1:Error_monad.pp_print_trace
    ("trace", Events.trace_encoding)

let is_ready pk = emit is_ready pk

let cannot_refresh_access_token pk = emit cannot_refresh_access_token pk

let new_token = emit new_token

let invalidated_token = emit invalidated_token
