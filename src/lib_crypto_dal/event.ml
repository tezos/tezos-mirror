(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
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
