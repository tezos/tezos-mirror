(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let level = Internal_event.Error

let section = ["protocol_updater"]

let node_uninitialized =
  declare_0
    ~section
    ~level
    ~name:"node_uninitialized"
    ~msg:"node not initialized"
    ()

let compiler_exit_error =
  declare_1
    ~section
    ~level
    ~name:"compiler_exit_error"
    ~msg:"error: {error}"
    ("error", Error_monad.(TzTrace.encoding error_encoding))

let compilation_interrupted =
  declare_1
    ~section
    ~level
    ~name:"compilation_interrupted"
    ~msg:"compilation interrupted (see logs in: {filename})"
    ("filename", Data_encoding.string)

let compilation_error =
  declare_1
    ~section
    ~level
    ~name:"compilation_error"
    ~msg:"compilation error (logs in file: {filename})"
    ("filename", Data_encoding.string)

let dynlink_error =
  declare_2
    ~section
    ~level
    ~name:"dynlink_error"
    ~msg:"can't load plugin: {plugin} ({reason})"
    ("plugin", Data_encoding.string)
    ("reason", Data_encoding.string)

let dynlink_error_static =
  declare_2
    ~section
    ~level
    ~name:"dynlink_error_static"
    ~msg:
      "can't load plugin either because the binary is statically linked or \
       because there was an error in its compilation: {plugin} ({reason})"
    ("plugin", Data_encoding.string)
    ("reason", Data_encoding.string)

let internal_error =
  declare_1
    ~section
    ~level
    ~name:"internal_error"
    ~msg:"internal error while compiling {protocol}"
    ("protocol", Protocol_hash.encoding)
