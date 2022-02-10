(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

type error +=
  | Missing_file_argument
  | Cannot_locate_file of string
  | Data_dir_not_found of {path : string}
  | Cannot_read_info

let () =
  register_error_kind
    `Permanent
    ~id:"main.snapshots.missing_file_argument"
    ~title:"Missing file argument"
    ~description:"This snapshot command requires a file argument"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Sanpshot command failed: the command requires a path to a snapshot \
         file as a command argument.")
    Data_encoding.(obj1 (req "empty" empty))
    (function Missing_file_argument -> Some () | _ -> None)
    (fun () -> Missing_file_argument) ;
  register_error_kind
    `Permanent
    ~id:"main.snapshots.cannot_locate_file"
    ~title:"Cannot locate file"
    ~description:"The snapshot file cannot be located."
    ~pp:(fun ppf path ->
      Format.fprintf
        ppf
        "Snapshot command failed: the snapshot file %s cannot be located."
        path)
    Data_encoding.(obj1 (req "snapshot_path" string))
    (function Cannot_locate_file s -> Some s | _ -> None)
    (fun s -> Cannot_locate_file s) ;
  register_error_kind
    `Permanent
    ~id:"main.snapshots.data_dir_not_found"
    ~title:"Cannot find the data directory"
    ~description:"Cannot find the data directory when exporting snapshot"
    ~pp:(fun ppf path ->
      Format.fprintf
        ppf
        "Cannot access the data directory when exporting the snapshot: cannot \
         locate data directory '%s'."
        path)
    Data_encoding.(obj1 (req "path" string))
    (function Data_dir_not_found {path} -> Some path | _ -> None)
    (fun path -> Data_dir_not_found {path}) ;
  register_error_kind
    `Permanent
    ~id:"Snapshot.cannot_read_info"
    ~title:"Cannot read info"
    ~description:"Failed to read snasphot info"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Failed to read snapshot info: cannot read info of legacy snapshots.")
    Data_encoding.(obj1 (req "empty" empty))
    (function Cannot_read_info -> Some () | _ -> None)
    (fun () -> Cannot_read_info)

(** Main *)

module Event = struct
  include Internal_event.Simple

  let section = ["node"; "main"]

  let cleaning_up_after_failure =
    Internal_event.Simple.declare_1
      ~section
      ~name:"cleaning_up_after_failure"
      ~msg:"cleaning up directory \"{directory}\" after failure."
      ~level:Internal_event.Notice
      ("directory", Data_encoding.string)

  let export_unspecified_hash =
    Internal_event.Simple.declare_0
      ~section
      ~name:"export_unspecified_hash"
      ~msg:
        "There is no block hash specified with the `--block` option. Using the \
         last checkpoint as the default value"
      ~level:Internal_event.Notice
      ()
end

module Term = struct
  type subcommand = Export | Import | Info

  let check_snapshot_path = function
    | None -> fail Missing_file_argument
    | Some path ->
        if Sys.file_exists path then return path
        else fail (Cannot_locate_file path)

  let process subcommand args snapshot_path block disable_check export_format
      rolling reconstruct sandbox_file =
    let run =
      let open Lwt_tzresult_syntax in
      let*! () = Tezos_base_unix.Internal_event_unix.init () in
      match subcommand with
      | Export ->
          let* data_dir = Node_shared_arg.read_data_dir args in
          let* () =
            fail_unless
              (Sys.file_exists data_dir)
              (Data_dir_not_found {path = data_dir})
          in
          let* node_config = Node_shared_arg.read_and_patch_config_file args in
          let ({genesis; chain_name; _} : Node_config_file.blockchain_network) =
            node_config.blockchain_network
          in
          let* () = Node_data_version.ensure_data_dir data_dir in
          let context_dir = Node_data_version.context_dir data_dir in
          let store_dir = Node_data_version.store_dir data_dir in
          let* block =
            match block with
            | None ->
                let*! () = Event.(emit export_unspecified_hash) () in
                return (`Alias (`Checkpoint, 0))
            | Some block -> (
                match Block_services.parse_block block with
                | Error err -> failwith "%s: %s" block err
                | Ok block -> return block)
          in
          Snapshots.export
            ?snapshot_path
            (Option.value export_format ~default:Snapshots.Tar)
            ~rolling
            ~store_dir
            ~context_dir
            ~chain_name
            ~block
            genesis
      | Import ->
          let data_dir =
            Option.value
              args.data_dir
              ~default:Node_config_file.default_data_dir
          in
          let*! existing_data_dir = Lwt_unix.file_exists data_dir in
          let* node_config = Node_shared_arg.read_and_patch_config_file args in
          let ({genesis; _} : Node_config_file.blockchain_network) =
            node_config.blockchain_network
          in
          let* snapshot_path = check_snapshot_path snapshot_path in
          let dir_cleaner () =
            let*! () = Event.(emit cleaning_up_after_failure) data_dir in
            if existing_data_dir then
              (* Remove only context and store if the import directory
                 was previously existing. *)
              let*! () =
                Lwt_utils_unix.remove_dir (Node_data_version.store_dir data_dir)
              in
              Lwt_utils_unix.remove_dir (Node_data_version.context_dir data_dir)
            else Lwt_utils_unix.remove_dir data_dir
          in
          let* () = Node_config_file.write args.config_file node_config in
          let* () = Node_data_version.ensure_data_dir ~bare:true data_dir in
          (* Lock only on snapshot import *)
          Lwt_lock_file.try_with_lock
            ~when_locked:(fun () ->
              failwith "Data directory is locked by another process")
            ~filename:(Node_data_version.lock_file data_dir)
          @@ fun () ->
          let* sandbox_parameters =
            match
              (node_config.blockchain_network.genesis_parameters, sandbox_file)
            with
            | (None, None) -> return_none
            | (Some parameters, None) ->
                return_some (parameters.context_key, parameters.values)
            | (_, Some filename) -> (
                let*! r = Lwt_utils_unix.Json.read_file filename in
                match r with
                | Error _err ->
                    fail (Node_run_command.Invalid_sandbox_file filename)
                | Ok json -> return_some ("sandbox_parameter", json))
          in
          let context_root = Node_data_version.context_dir data_dir in
          let store_root = Node_data_version.store_dir data_dir in
          let patch_context =
            Patch_context.patch_context genesis sandbox_parameters
          in
          let* () =
            protect
              ~on_error:(fun err ->
                let*! () = dir_cleaner () in
                Lwt.return (Error err))
              (fun () ->
                let* block =
                  match block with
                  | Some s -> (
                      match Block_hash.of_b58check_opt s with
                      | Some bh -> return_some bh
                      | None -> failwith "%s is not a valid block identifier." s
                      )
                  | None -> return_none
                in
                let check_consistency = not disable_check in
                Snapshots.import
                  ~snapshot_path
                  ~patch_context
                  ?block
                  ~check_consistency
                  ~dst_store_dir:store_root
                  ~dst_context_dir:context_root
                  ~chain_name:node_config.blockchain_network.chain_name
                  ~user_activated_upgrades:
                    node_config.blockchain_network.user_activated_upgrades
                  ~user_activated_protocol_overrides:
                    node_config.blockchain_network
                      .user_activated_protocol_overrides
                  genesis)
          in
          if reconstruct then
            Reconstruction.reconstruct
              ~patch_context
              ~store_dir:store_root
              ~context_dir:context_root
              genesis
              ~user_activated_upgrades:
                node_config.blockchain_network.user_activated_upgrades
              ~user_activated_protocol_overrides:
                node_config.blockchain_network.user_activated_protocol_overrides
          else return_unit
      | Info ->
          let* snapshot_path = check_snapshot_path snapshot_path in
          let* snapshot_header =
            Snapshots.read_snapshot_header ~snapshot_path
          in
          Format.printf
            "@[<v 2>Snapshot information:@ %a@]@."
            Snapshots.pp_snapshot_header
            snapshot_header ;
          return_unit
    in
    match Lwt_main.run @@ Lwt_exit.wrap_and_exit run with
    | Ok () -> `Ok ()
    | Error err -> `Error (false, Format.asprintf "%a" pp_print_trace err)

  open Cmdliner.Arg

  let subcommand_arg =
    let parser = function
      | "export" -> `Ok Export
      | "import" -> `Ok Import
      | "info" -> `Ok Info
      | s -> `Error ("invalid argument: " ^ s)
    and printer ppf = function
      | Export -> Format.fprintf ppf "export"
      | Import -> Format.fprintf ppf "import"
      | Info -> Format.fprintf ppf "info"
    in
    let doc =
      "Operation to perform. Possible values: $(b,export), $(b,import)."
    in
    required
    & pos 0 (some (parser, printer)) None
    & info [] ~docv:"OPERATION" ~doc

  let file_arg =
    let doc =
      "The name of the snapshot file to export. If no provided, it will be \
       automatically generated using the target block and following pattern: \
       $(i,./<NETWORK>-<BLOCK_HASH>-<BLOCK_LEVEL>.<SNAPSHOT_KIND>). Otherwise, \
       it must be given as a positional argument, just after the $(b,export) \
       hint."
    in
    value & pos 1 (some string) None & info [] ~doc ~docv:"FILE"

  let block =
    let open Cmdliner.Arg in
    let doc =
      "The block to export/import. When exporting, either the block_hash, the \
       level or an alias (such as $(i,caboose), $(i,checkpoint), \
       $(i,savepoint) or $(i,head) in combination with ~ and + operators) can \
       be used. When importing, only the block hash you are expected to \
       restore is allowed."
    in
    value
    & opt (some string) None
    & info ~docv:"<block_hash, level, alias>" ~doc ["block"]

  let disable_check =
    let open Cmdliner.Arg in
    let doc =
      "Setting this flag disables the consistency check after importing a \
       full-mode snapshot. This improves performances but is strongly \
       discouraged because the snapshot could contain a corrupted chain."
    in
    value & flag & info ~doc ["no-check"]

  let export_format =
    let parser = function
      | "tar" -> `Ok Snapshots.Tar
      | "raw" -> `Ok Raw
      | s -> `Error ("invalid argument: " ^ s)
    and printer ppf = function
      | Snapshots.Tar -> Format.fprintf ppf "tar"
      | Raw -> Format.fprintf ppf "raw"
    in
    let open Cmdliner.Arg in
    let doc =
      "Setting this flag selects the format of the exported snasphot. When \
       $(i, tar) (used by default) is selected, the snapshot is exported as a \
       single file tar archive. $(i, raw) exports the snapshot as a directory \
       populated with all the data -- this mode is suitable for IPFS snapshot \
       sharing."
    in
    value
    & opt (some (parser, printer)) None
    & info ~docv:"tar (default) | raw" ~doc ["export-format"]

  let export_rolling =
    let open Cmdliner in
    let doc =
      "Force export command to dump a minimal snapshot based on the rolling \
       mode."
    in
    Arg.(
      value & flag
      & info ~docs:Node_shared_arg.Manpage.misc_section ~doc ["rolling"])

  let reconstruct =
    let open Cmdliner in
    let doc =
      "Start a storage reconstruction from a full mode snapshot to an archive \
       storage. This operation can be quite long."
    in
    Arg.(
      value & flag
      & info ~docs:Node_shared_arg.Manpage.misc_section ~doc ["reconstruct"])

  let sandbox =
    let open Cmdliner in
    let doc =
      "Run the snapshot import in sandbox mode. P2P to non-localhost addresses \
       are disabled, and constants of the economic protocol can be altered \
       with an optional JSON file. $(b,IMPORTANT): Using sandbox mode affects \
       the node state and subsequent runs of Tezos node must also use sandbox \
       mode. In order to run the node in normal mode afterwards, a full reset \
       must be performed (by removing the node's data directory)."
    in
    Arg.(
      value
      & opt (some non_dir_file) None
      & info
          ~docs:Node_shared_arg.Manpage.misc_section
          ~doc
          ~docv:"FILE.json"
          ["sandbox"])

  let term =
    let open Cmdliner.Term in
    ret
      (const process $ subcommand_arg $ Node_shared_arg.Term.args $ file_arg
     $ block $ disable_check $ export_format $ export_rolling $ reconstruct
     $ sandbox)
end

module Manpage = struct
  let command_description =
    "The $(b,snapshot) command is meant to export and import snapshots files."

  let description =
    [
      `S "DESCRIPTION";
      `P (command_description ^ " Several operations are possible: ");
      `P
        "$(b,export) allows to export a snapshot of the current node state \
         into a file.";
      `P "$(b,import) allows to import a snapshot from a given file.";
      `P "$(b,info) displays information about the snapshot file.";
    ]

  let options = [`S "OPTIONS"]

  let examples =
    [
      `S "EXAMPLES";
      `I
        ( "$(b,Export a snapshot using the rolling mode)",
          "$(mname) snapshot export latest.rolling --rolling" );
      `I
        ( "$(b,Export a snapshot up to and including the given block hash)",
          "$(mname) snapshot export file.full --block <BLOCK_HASH>" );
      `I
        ( "$(b,Export a snapshot up to and including the 10th predecessor of \
           the current head)",
          "$(mname) snapshot export file.full --block head~10" );
      `I
        ( "$(b,Import a snapshot located in file.full)",
          "$(mname) snapshot import file.full" );
      `I
        ( "$(b,Import a snapshot and ensure that the imported data targets the \
           given block hash (recommended))",
          "$(mname) snapshot import file.full --block <BLOCK_HASH>" );
      `I
        ( "$(b,Import a full mode snapshot and then reconstruct the whole \
           storage to obtain an archive mode storage)",
          "$(mname) snapshot import file.full --reconstruct" );
    ]

  let man = description @ options @ examples @ Node_shared_arg.Manpage.bugs

  let info = Cmdliner.Term.info ~doc:"Manage snapshots" ~man "snapshot"
end

let cmd = (Term.term, Manpage.info)
