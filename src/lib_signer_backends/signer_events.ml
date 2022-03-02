(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Socket = struct
  let section = ["client"; "signer"; "socket"]

  let signer_timeout =
    declare_1
      ~section
      ~name:"signer_timeout"
      ~msg:"Remote signer timeout, retrying {retry} more time(s)"
      ~level:Error
      ("retry", Data_encoding.int8)
end

module Ledger = struct
  let section = ["client"; "signer"; "ledger"]

  let not_tezos =
    declare_2
      ~section
      ~name:"ledger_not_tezos"
      ~msg:"The device at [{position}] is not a Tezos application {error}"
      ~level:Warning
      ~pp2:Format.pp_print_text
      ("position", Data_encoding.string)
      ("error", Data_encoding.string)

  let found_application =
    declare_3
      ~section
      ~name:"ledger_found_application"
      ~msg:
        "Found a {version} application at [{position}] (git-description: \
         {commit})"
      ~level:Info
      ("version", Data_encoding.string)
      ("position", Data_encoding.string)
      ("commit", Data_encoding.string)

  let found =
    declare_2
      ~section
      ~name:"ledger_found"
      ~msg:"Found {number} Ledger(s) {info}"
      ~level:Debug
      ("number", Data_encoding.int8)
      ("info", Data_encoding.string)

  let processing =
    declare_1
      ~section
      ~name:"ledger_processing"
      ~msg:"Processing Ledger at path [{position}]"
      ~level:Info
      ("position", Data_encoding.string)

  let communication =
    declare_1
      ~section
      ~name:"ledger_communication"
      ~msg:"{message}"
      ~level:Debug
      ~pp1:Format.pp_print_text
      ("message", Data_encoding.string)
end
