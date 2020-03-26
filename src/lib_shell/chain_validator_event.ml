(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

include Internal_event.Simple

let section = ["node"; "chain_validator"]

let chain_bootstrapped =
  declare_0
    ~section
    ~name:"chain_bootstrapped"
    ~msg:"the chain is bootstrapped"
    ~level:Info
    ()

let updated_to_checkpoint =
  declare_2
    ~section
    ~name:"updated_to_checkpoint"
    ~msg:"updated to checkpoint {block_hash} (running in mode {history_mode})"
    ~level:Notice
    ("block_hash", Block_hash.encoding)
    ("history_mode", History_mode.encoding)

let prevalidator_filter_not_found =
  declare_1
    ~section
    ~name:"prevalidator_filter_not_found"
    ~msg:"no prevalidator filter found for protocol {protocol_hash}"
    ~level:Notice
    ("protocol_hash", Protocol_hash.encoding)

let prevalidator_reinstantiation_failure =
  declare_2
    ~section
    ~name:"prevalidator_reinstantiation_failure"
    ~msg:"failed to reinstantiate prevalidator error {fst_error}"
    ~level:Error
    ("fst_error", Data_encoding.string)
    ("errors", Data_encoding.list error_encoding)
