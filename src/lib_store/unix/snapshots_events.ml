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

  let warn_tar_corruption =
    declare_1
      ~section
      ~level:Warning
      ~name:"warn_tar_corruption"
      ~msg:
        "Warning: the snapshot being imported is in version {version} and \
         using the tar format: some internal files might be corrupted. If the \
         snapshot import fails, please import using a snapshot that is a least \
         version 7."
      ~pp1:Format.pp_print_int
      ("version", Data_encoding.int31)

  (* Debug *)
  let restoring_context =
    declare_0
      ~section
      ~level:Debug
      ~name:"restoring_context"
      ~msg:"restoring the context from the snapshot"
      ()

  let context_restored =
    declare_0
      ~section
      ~level:Debug
      ~name:"context_restored"
      ~msg:"context was successfully restored"
      ()

  let applying_target_block =
    declare_0
      ~section
      ~level:Debug
      ~name:"applying_target_block"
      ~msg:"applying the block targeted by the snapshot"
      ()

  let target_block_applied =
    declare_0
      ~section
      ~level:Debug
      ~name:"target_block_applied"
      ~msg:"block target was successfully applied"
      ()

  let restoring_protocols =
    declare_0
      ~section
      ~level:Debug
      ~name:"restoring_protocols"
      ~msg:"restoring protocols from snapshot"
      ()

  let protocols_restored =
    declare_0
      ~section
      ~level:Debug
      ~name:"protocols_restored"
      ~msg:"protocols were successfully restored"
      ()

  let restoring_cemented_indexes =
    declare_0
      ~section
      ~level:Debug
      ~name:"restoring_cemented_indexes"
      ~msg:"restoring cemented indexes"
      ()

  let restoring_cemented_cycles =
    declare_0
      ~section
      ~level:Debug
      ~name:"restoring_cemented_cycles"
      ~msg:"restoring cemented cycles"
      ()

  let cemented_cycles_restored =
    declare_0
      ~section
      ~level:Debug
      ~name:"cemented_cycles_restored"
      ~msg:"cemented cycles were successfully restored"
      ()

  let checking_cycles_consistency =
    declare_0
      ~section
      ~level:Debug
      ~name:"checking_cycles_consistency"
      ~msg:"checking cemented cycles consistency"
      ()

  let cycles_consistency_checked =
    declare_0
      ~section
      ~level:Debug
      ~name:"cycles_consistency_checked"
      ~msg:"cemented cycles consistency checked"
      ()

  let restoring_floating_blocks =
    declare_0
      ~section
      ~level:Debug
      ~name:"restoring_floating_blocks"
      ~msg:"restoring floating blocks"
      ()

  let floating_blocks_restored =
    declare_0
      ~section
      ~level:Debug
      ~name:"floating_blocks_restored"
      ~msg:"floating blocks were successfully restored"
      ()

  let exporting_cemented_cycles =
    declare_0
      ~section
      ~level:Debug
      ~name:"exporting_cemented_cycles"
      ~msg:"exporting cemented cycles"
      ()

  let cemented_cycles_exported =
    declare_0
      ~section
      ~level:Debug
      ~name:"cemented_cycles_exported"
      ~msg:"cemented cycles were successfully exported"
      ()

  let exporting_context =
    declare_0
      ~section
      ~level:Debug
      ~name:"exporting_context"
      ~msg:"exporting context"
      ()

  let context_exported =
    declare_0
      ~section
      ~level:Debug
      ~name:"exported_context"
      ~msg:"context was successfully exported"
      ()

  let exporting_floating_blocks =
    declare_0
      ~section
      ~level:Debug
      ~name:"exporting_floating_blocks"
      ~msg:"exporting floating blocks"
      ()

  let floating_blocks_exported =
    declare_0
      ~section
      ~level:Debug
      ~name:"floating_blocks_exported"
      ~msg:"floating blocks were successfully exported"
      ()

  let exporting_protocols =
    declare_0
      ~section
      ~level:Debug
      ~name:"exporting_protocols"
      ~msg:"exporting protocols"
      ()

  let protocols_exported =
    declare_0
      ~section
      ~level:Debug
      ~name:"protocols_exported"
      ~msg:"protocols were successfully exported"
      ()
end
