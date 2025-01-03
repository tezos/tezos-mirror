(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

include Tezos_event_logging.Internal_event.Simple

let section = ["dal"; "cryptobox"]

let installing_trusted_setup =
  declare_1
    ~section
    ~name:"installing_trusted_setup"
    ~msg:"Installing DAL trusted setup in {folder}"
    ~level:Notice
    ("folder", Data_encoding.string)

let download =
  declare_1
    ~section
    ~name:"download"
    ~msg:"Downloading from {uri}"
    ~level:Notice
    ("uri", Data_encoding.string)

let successful_download =
  declare_1
    ~section
    ~name:"successful_download"
    ~msg:"{file} has been successfully downloaded"
    ~level:Notice
    ("file", Data_encoding.string)

let valid_file =
  declare_1
    ~section
    ~name:"valid_file"
    ~msg:"File {file} exists, sha256 correct. All done!"
    ~level:Notice
    ("file", Data_encoding.string)

let invalid_file =
  declare_1
    ~section
    ~name:"invalid_file"
    ~msg:"File {file} exists, but its sha256 is incorrect."
    ~level:Warning
    ("file", Data_encoding.string)

let file_not_found =
  declare_1
    ~section
    ~name:"file_not_found"
    ~msg:"File {file} not found."
    ~level:Warning
    ("file", Data_encoding.string)
