(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs. <nomadic@tezcore.com>               *)
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

open Store_types

module Event = struct
  include Internal_event.Simple

  let section = ["node"; "snapshots"]

  (* Notice *)
  let export_info =
    declare_3
      ~section
      ~level:Notice
      ~name:"export_info"
      ~msg:
        "exporting a snapshot (v{version}) in {history_mode} mode, targeting \
         block hash {block}"
      ~pp1:Format.pp_print_int
      ("version", Data_encoding.int31)
      ~pp2:History_mode.pp_short
      ("history_mode", History_mode.encoding)
      ~pp3:pp_block_descriptor
      ("block", block_descriptor_encoding)

  let export_success =
    declare_1
      ~section
      ~level:Notice
      ~name:"export_success"
      ~msg:"successful export: {filename}"
      ~pp1:Format.pp_print_string
      ("filename", Data_encoding.string)

  let cleaning_tmp_export_directory =
    declare_1
      ~section
      ~level:Notice
      ~name:"cleaning_tmp_export_directory"
      ~msg:"cleaning leftover export directory '{filename}'"
      ~pp1:Format.pp_print_string
      ("filename", Data_encoding.string)

  let import_info =
    declare_2
      ~section
      ~level:Notice
      ~name:"import_info"
      ~msg:"importing data from snapshot {filename}: {header}"
      ~pp1:Format.pp_print_string
      ("filename", Data_encoding.string)
      ~pp2:Format.pp_print_string
      ("header", Data_encoding.(string))

  let import_unspecified_hash =
    declare_0
      ~section
      ~level:Notice
      ~name:"import_unspecified_hash"
      ~msg:
        "you may consider using the --block <block_hash> argument to ensure \
         that the block imported is the one you expected"
      ()

  let import_loading =
    declare_0
      ~section
      ~level:Notice
      ~name:"import_loading"
      ~msg:
        "retrieving and validating data. This can take a while, please bear \
         with us"
      ()

  let import_success =
    declare_1
      ~section
      ~level:Notice
      ~name:"import_succes"
      ~msg:"successful import from file {filename}"
      ~pp1:Format.pp_print_string
      ("filename", Data_encoding.string)

  let cleaning_after_failure =
    declare_0
      ~section
      ~level:Notice
      ~name:"cleaning_after_failure"
      ~msg:"cleaning up artifacts after failure"
      ()

  let suggest_no_check =
    declare_0
      ~section
      ~level:Notice
      ~name:"suggest_no_check"
      ~msg:
        "Note: the import of a snapshot can be sped up using the '--no-check' \
         option. Only use this option if you fully trust the snapshot source."
      ()

  (* Info *)
  let validate_protocol_sources =
    declare_1
      ~section
      ~level:Notice
      ~name:"validate_protocol_sources"
      ~msg:"validating protocol {hash} against sources"
      ~pp1:Protocol_hash.pp
      ("hash", Protocol_hash.encoding)

  (* Warning *)
  let warn_no_check =
    declare_0
      ~section
      ~level:Warning
      ~name:"warn_no_check"
      ~msg:
        "Warning: to speed up the import, the consistency of the imported data \
         will not be fully checked. It is not recommended to use this option \
         with a snapshot downloaded from an untrusted source"
      ()
end
