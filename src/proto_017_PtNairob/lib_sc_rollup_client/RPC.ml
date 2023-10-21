(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech, <contact@trili.tech>                        *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
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
