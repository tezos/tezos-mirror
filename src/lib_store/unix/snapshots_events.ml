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
    declare_2
      ~section
      ~level:Notice
      ~name:"export_info"
      ~msg:
        "exporting a snapshot in {history_mode} mode, targeting block hash \
         {block}"
      ~pp1:History_mode.pp_short
      ("history_mode", History_mode.encoding)
      ~pp2:pp_block_descriptor
      ("block", block_descriptor_encoding)

  let export_success =
    declare_1
      ~section
      ~level:Notice
      ~name:"export_success"
      ~msg:"successful export: {filename}"
      ~pp1:Format.pp_print_string
      ("filename", Data_encoding.string)

  let import_info =
    let option_pp ~default pp fmt = function
      | None -> Format.fprintf fmt "%s" default
      | Some x -> Format.fprintf fmt "%a" pp x
    in
    declare_2
      ~section
      ~level:Notice
      ~name:"import_info"
      ~msg:"importing data from snapshot {filename}{metadata}"
      ~pp1:Format.pp_print_string
      ("filename", Data_encoding.string)
      ~pp2:(fun fmt v ->
        Format.fprintf
          fmt
          "%a"
          (option_pp ~default:" (legacy)" (fun fmt metadata ->
               Format.fprintf fmt ": %s" metadata))
          v)
      ("metadata", Data_encoding.(option string))

  let import_on_disk_mode =
    declare_0
      ~section
      ~level:Notice
      ~name:"import_on_disk_mode"
      ~msg:
        "importing snapshot in on-disk mode. Mind using the --in-memory flag \
         to allow a faster import which uses a higher quantity of ram"
      ()

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

  (* Info *)
  let validate_protocol_sources =
    declare_1
      ~section
      ~level:Notice
      ~name:"validate_protocol_sources"
      ~msg:"validating protocol {hash} against sources"
      ~pp1:Protocol_hash.pp
      ("hash", Protocol_hash.encoding)
end
