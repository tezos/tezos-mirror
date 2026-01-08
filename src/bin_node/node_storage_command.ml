(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Tarides <contact@tarides.com>                          *)
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

let ( // ) = Filename.concat

type error +=
  | Existing_index_dir of string
  | Cannot_resolve_block_alias of string * string

module Event = struct
  include Internal_event.Simple

  let section = ["node"; "storage"]

  let integrity_check =
    declare_3
      ~section
      ~name:"integrity_check"
      ~msg:
        "running integrity check on inodes for block {block_hash} (level \
         {block_level}) with context hash {context_hash}"
      ~level:Notice
      ~pp1:Block_hash.pp
      ("block_hash", Block_hash.encoding)
      ("block_level", Data_encoding.int32)
      ~pp3:Context_hash.pp
      ("context_hash", Context_hash.encoding)

  let integrity_check_inodes =
    declare_3
      ~section
      ~name:"integrity_check_inodes"
      ~msg:
        "running integrity check on inodes for block {block_hash} (level \
         {block_level}) with context hash {context_hash}"
      ~level:Notice
      ~pp1:Block_hash.pp
      ("block_hash", Block_hash.encoding)
      ("block_level", Data_encoding.int32)
      ~pp3:Context_hash.pp
      ("context_hash", Context_hash.encoding)

  let stat_metadata_report_generated =
    declare_1
      ~section
      ~name:"stat_metadata_report_generated"
      ~msg:"the stat-metadata report was generated in {path}"
      ~level:Notice
      ("path", Data_encoding.string)
end

let () =
  register_error_kind
    `Permanent
    ~id:"main.storage.existing_index_dir"
    ~title:"Existing context/index directory"
    ~description:"The index directory cannot be overwritten"
    ~pp:(fun ppf path ->
      Format.fprintf ppf "Existing index directory '%s'." path)
    Data_encoding.(obj1 (req "indexdir_path" string))
    (function Existing_index_dir path -> Some path | _ -> None)
    (fun path -> Existing_index_dir path) ;
  register_error_kind
    `Permanent
    ~id:"main.storage.cannot_resolve_block_alias"
    ~title:"Cannot resolve block alias"
    ~description:"Cannot resolve block alias"
    ~pp:(fun ppf (alias, error) ->
      Format.fprintf ppf "Failed to resolve the block alias %s: %s" alias error)
    Data_encoding.(obj2 (req "alias" string) (req "error" string))
    (function
      | Cannot_resolve_block_alias (alias, error) -> Some (alias, error)
      | _ -> None)
    (fun (alias, error) -> Cannot_resolve_block_alias (alias, error))

module Term = struct
  open Cmdliner

  let ( let+ ) t f = Term.(const f $ t)

  let ( and+ ) a b = Term.(const (fun x y -> (x, y)) $ a $ b)

  type resolved_parameters = {
    genesis : Genesis.t;
    chain_id : Chain_id.t;
    data_dir : string;
    context_dir : string;
    store_dir : string;
  }

  (* This is actually a weak check. The irmin command should take care
     of checking whether or not the directory is valid/initialized or
     not. *)
  let ensure_context_dir context_dir =
    let open Lwt_result_syntax in
    Lwt.catch
      (fun () ->
        let*! b = Lwt_unix.file_exists context_dir in
        if not b then
          tzfail
            (Data_version.Invalid_data_dir
               {data_dir = context_dir; msg = Some "invalid directory"})
        else return_unit)
      (function
        | Unix.Unix_error _ ->
            tzfail
              (Data_version.Invalid_data_dir
                 {data_dir = context_dir; msg = None})
        | exc -> Lwt.reraise exc)

  (* Returns the adequate config arguments given a config_file and a
     data_dir. *)
  let resolve_config config_file data_dir =
    let open Lwt_result_syntax in
    let* data_dir, node_config =
      Shared_arg.resolve_data_dir_and_config_file ?data_dir ?config_file ()
    in
    let ({genesis; _} : Config_file.blockchain_network) =
      node_config.blockchain_network
    in
    let chain_id = Chain_id.of_block_hash genesis.block in
    let* () = Data_version.ensure_data_dir genesis data_dir in
    let context_dir = Tezos_context_ops.Context_ops.context_dir data_dir in
    let* () = ensure_context_dir context_dir in
    let store_dir = Data_version.store_dir data_dir in
    return {genesis; chain_id; data_dir; context_dir; store_dir}

  let resolve_block chain_store block =
    let open Lwt_result_syntax in
    match block with
    | Some block -> (
        match Block_services.parse_block block with
        | Error err -> tzfail (Cannot_resolve_block_alias (block, err))
        | Ok block -> Store.Chain.block_of_identifier chain_store block)
    | None ->
        let*! current_head = Store.Chain.current_head chain_store in
        return current_head

  let integrity_check config_file data_dir auto_repair block =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
       let*! () = Tezos_base_unix.Internal_event_unix.init () in
       let* {genesis; chain_id; store_dir; data_dir; _} =
         resolve_config config_file data_dir
       in
       let* store =
         Store.init ~store_dir ~data_dir ~allow_testchains:false genesis
       in
       let* chain_store = Store.get_chain_store store chain_id in
       let* block = resolve_block chain_store block in
       let context_hash = Store.Block.context_hash block in
       let context_hash_str = Context_hash.to_b58check context_hash in
       let*! exists = Store.Block.context_exists chain_store block in
       let* () =
         when_ (not exists) (fun () ->
             tzfail
               (Data_version.Invalid_data_dir
                  {
                    data_dir = Tezos_context_ops.Context_ops.context_dir data_dir;
                    msg =
                      Some
                        (Format.sprintf
                           "cannot find context hash %s"
                           context_hash_str);
                  }))
       in
       let*! () = Store.close_store store in
       let*! () =
         Event.(
           emit
             integrity_check
             (Store.Block.hash block, Store.Block.level block, context_hash))
       in
       let*! () =
         Tezos_context.Context.Checks.Pack.Integrity_check.run
           ~ppf:Format.std_formatter
           ~root:(Tezos_context_ops.Context_ops.context_dir data_dir)
           ~auto_repair
           ~always:false
           ~heads:(Some [context_hash_str])
           ()
       in
       return_unit)

  let stat_index config_file data_dir =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
       let* {data_dir; _} = resolve_config config_file data_dir in
       Tezos_context.Context.Checks.Index.Stat.run
         ~root:(Tezos_context_ops.Context_ops.context_dir data_dir) ;
       return_unit)

  let stat_pack config_file data_dir =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
       let* {data_dir; _} = resolve_config config_file data_dir in
       let*! () =
         Tezos_context.Context.Checks.Pack.Stat.run
           ~root:(Tezos_context_ops.Context_ops.context_dir data_dir)
       in
       return_unit)

  let index_dir_exists context_dir output =
    let open Lwt_result_syntax in
    let index_dir = Option.value output ~default:(context_dir // "index") in
    let*! b = Lwt_unix.file_exists index_dir in
    if not b then return_unit else tzfail (Existing_index_dir index_dir)

  let reconstruct_index config_file data_dir output index_log_size =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
       let* {data_dir; _} = resolve_config config_file data_dir in
       let context_dir = Tezos_context_ops.Context_ops.context_dir data_dir in
       let* () = index_dir_exists context_dir output in
       Tezos_context.Context.Checks.Pack.Reconstruct_index.run
         ~root:context_dir
         ~output
         ~index_log_size
         () ;
       return_unit)

  let integrity_check_inodes config_file data_dir block =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
       let*! () = Tezos_base_unix.Internal_event_unix.init () in
       let* {genesis; chain_id; data_dir; store_dir; _} =
         resolve_config config_file data_dir
       in
       let* store =
         Store.init ~store_dir ~data_dir ~allow_testchains:false genesis
       in
       let* chain_store = Store.get_chain_store store chain_id in
       let* block = resolve_block chain_store block in
       let*! () = Store.close_store store in
       let context_hash = Store.Block.context_hash block in
       let context_hash_str = Context_hash.to_b58check context_hash in
       let*! () =
         Event.(
           emit
             integrity_check_inodes
             (Store.Block.hash block, Store.Block.level block, context_hash))
       in
       let*! () =
         Tezos_context.Context.Checks.Pack.Integrity_check_inodes.run
           ~root:(Tezos_context_ops.Context_ops.context_dir data_dir)
           ~heads:(Some [context_hash_str])
       in
       return_unit)

  let check_index config_file data_dir auto_repair =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
       let* {data_dir; _} = resolve_config config_file data_dir in
       Tezos_context.Context.Checks.Pack.Integrity_check_index.run
         ~root:(Tezos_context_ops.Context_ops.context_dir data_dir)
         ~auto_repair
         () ;
       return_unit)

  let find_head config_file data_dir =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
       let*! () = Tezos_base_unix.Internal_event_unix.init () in
       let* {genesis; chain_id; data_dir; store_dir; _} =
         resolve_config config_file data_dir
       in
       let* store =
         Store.init ~store_dir ~data_dir ~allow_testchains:false genesis
       in
       let* chain_store = Store.get_chain_store store chain_id in
       let*! head = Store.Chain.current_head chain_store in
       let*! () = Store.close_store store in
       let head_context_hash = Store.Block.context_hash head in
       (* This output isn't particularly useful for most users,
           it will typically be used to inspect context
           directories using Irmin tooling *)
       let () = Format.printf "%a@." Context_hash.pp head_context_hash in
       return_unit)

  type block_stats = {
    level : Int32.t;
    block_metadata_size : Int64.t;
    manager_operation_metadata_sizes : int list;
    too_large_operation_metadata : bool;
  }

  let block_stats_encoding =
    let open Data_encoding in
    conv
      (fun {
             level;
             block_metadata_size;
             manager_operation_metadata_sizes;
             too_large_operation_metadata;
           }
         ->
        ( level,
          block_metadata_size,
          manager_operation_metadata_sizes,
          too_large_operation_metadata ))
      (fun ( level,
             block_metadata_size,
             manager_operation_metadata_sizes,
             too_large_operation_metadata )
         ->
        {
          level;
          block_metadata_size;
          manager_operation_metadata_sizes;
          too_large_operation_metadata;
        })
      (obj4
         (req "level" int32)
         (req "block_metadata_size" int64)
         (req "manager_operation_metadata_sizes" (list int31))
         (req "too_large_operation_metadata" bool))

  type cycle_stats = {
    name : string;
    block_count : int;
    block_stats : block_stats list;
    block_metadata_size : Int64.t;
    operation_metadata_size : Int64.t;
    too_large_operation_metadata_count : Int64.t;
  }

  let cycle_stats_encoding =
    let open Data_encoding in
    conv
      (fun {
             name;
             block_count;
             block_stats;
             block_metadata_size;
             operation_metadata_size;
             too_large_operation_metadata_count;
           }
         ->
        ( name,
          block_count,
          block_stats,
          block_metadata_size,
          operation_metadata_size,
          too_large_operation_metadata_count ))
      (fun ( name,
             block_count,
             block_stats,
             block_metadata_size,
             operation_metadata_size,
             too_large_operation_metadata_count )
         ->
        {
          name;
          block_count;
          block_stats;
          block_metadata_size;
          operation_metadata_size;
          too_large_operation_metadata_count;
        })
      (obj6
         (req "name" string)
         (req "block_count" int31)
         (req "block_stats" (list block_stats_encoding))
         (req "block_metadata_size" int64)
         (req "operation_metadata_size" int64)
         (req "too_large_operation_metadata_count" int64))

  type overall_stats = {
    cycle_stats : cycle_stats list;
    block_count : int;
    block_metadata_size : Int64.t;
    operation_metadata_size : Int64.t;
    too_large_operation_metadata_count : Int64.t;
  }

  let overall_stats_encoding =
    let open Data_encoding in
    conv
      (fun {
             cycle_stats;
             block_count;
             block_metadata_size;
             operation_metadata_size;
             too_large_operation_metadata_count;
           }
         ->
        ( cycle_stats,
          block_count,
          block_metadata_size,
          operation_metadata_size,
          too_large_operation_metadata_count ))
      (fun ( cycle_stats,
             block_count,
             block_metadata_size,
             operation_metadata_size,
             too_large_operation_metadata_count )
         ->
        {
          cycle_stats;
          block_count;
          block_metadata_size;
          operation_metadata_size;
          too_large_operation_metadata_count;
        })
      (obj5
         (req "cycle_stats" (list cycle_stats_encoding))
         (req "block_count" int31)
         (req "block_metadata_size" int64)
         (req "operation_metadata_size" int64)
         (req "too_large_operation_metadata_count" int64))

  let stat_metadata config_file data_dir report_path =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
       let*! () = Tezos_base_unix.Internal_event_unix.init () in
       let* {genesis; data_dir; store_dir; _} =
         resolve_config config_file data_dir
       in
       let* store =
         Store.init
           ~readonly:true
           ~store_dir
           ~data_dir
           ~allow_testchains:false
           genesis
       in
       let* res = Store.Utilities.stat_metadata_cycles store in
       let overall_stats =
         List.fold_left
           (fun {
                  cycle_stats;
                  block_count;
                  block_metadata_size;
                  operation_metadata_size;
                  too_large_operation_metadata_count;
                }
                (metadata_file, cycle)
              ->
             let new_cycle_stats =
               List.fold_left
                 (fun {
                        name;
                        block_count;
                        block_stats;
                        block_metadata_size;
                        operation_metadata_size;
                        too_large_operation_metadata_count;
                      }
                      (stats : Tezos_store_shared.Store_types.metadata_stat)
                    ->
                   let new_block_stats =
                     {
                       level = stats.block_level;
                       block_metadata_size = stats.operation_metadata_size;
                       manager_operation_metadata_sizes =
                         stats.manager_operation_metadata_sizes;
                       too_large_operation_metadata =
                         not
                           (Int64.equal
                              stats.too_large_operation_metadata_count
                              0L);
                     }
                   in
                   Tezos_store_shared.Store_types.
                     {
                       name;
                       block_count;
                       block_stats = new_block_stats :: block_stats;
                       block_metadata_size =
                         Int64.add block_metadata_size stats.block_metadata_size;
                       operation_metadata_size =
                         Int64.add
                           operation_metadata_size
                           stats.operation_metadata_size;
                       too_large_operation_metadata_count =
                         Int64.add
                           too_large_operation_metadata_count
                           stats.too_large_operation_metadata_count;
                     })
                 {
                   name = metadata_file;
                   block_count = List.length cycle;
                   block_stats = [];
                   block_metadata_size = 0L;
                   operation_metadata_size = 0L;
                   too_large_operation_metadata_count = 0L;
                 }
                 cycle
             in
             {
               cycle_stats = new_cycle_stats :: cycle_stats;
               block_count = block_count + new_cycle_stats.block_count;
               block_metadata_size =
                 Int64.add
                   block_metadata_size
                   new_cycle_stats.block_metadata_size;
               operation_metadata_size =
                 Int64.add
                   operation_metadata_size
                   new_cycle_stats.operation_metadata_size;
               too_large_operation_metadata_count =
                 Int64.add
                   too_large_operation_metadata_count
                   new_cycle_stats.too_large_operation_metadata_count;
             })
           {
             cycle_stats = [];
             block_count = 0;
             block_metadata_size = 0L;
             operation_metadata_size = 0L;
             too_large_operation_metadata_count = 0L;
           }
           res
       in
       let* () =
         Animation.three_dots
           ~progress_display_mode:Always
           ~msg:"Generating report"
           (fun () ->
             let report =
               Data_encoding.Json.construct overall_stats_encoding overall_stats
             in
             Tezos_stdlib_unix.Lwt_utils_unix.Json.write_file report_path report)
       in
       let*! () = Event.(emit stat_metadata_report_generated) report_path in
       let*! () = Store.close_store store in
       return_unit)

  let auto_repair =
    let open Cmdliner.Arg in
    value
    & flag
      @@ info
           ~doc:
             "Automatically repair issues; option for integrity-check and \
              integrity-check-index."
           ["auto-repair"]

  let dest =
    let open Cmdliner.Arg in
    value
    & opt (some string) None
      @@ info
           ~doc:"Path to the new index file; option for reconstruct-index."
           ~docv:"DEST"
           ["output"; "o"]

  let index_log_size =
    let open Cmdliner.Arg in
    value
    & opt int 10_000_000
      @@ info
           ~doc:
             "Size of the index write-ahead log; option for reconstruct-index. \
              Increasing the log size will reduce the total time necessary to \
              reconstruct the index, at the cost of increased memory usage."
           ~docv:"ENTRIES"
           ["index-log-size"]

  let block =
    let open Cmdliner.Arg in
    value
    & opt (some string) None
      @@ info
           ~doc:"Block; option for integrity-check-inodes."
           ~docv:"BLOCK"
           ["block"; "b"]

  let setup_logs =
    let+ style_renderer = Fmt_cli.style_renderer ()
    and+ level =
      let+ vopts =
        let doc =
          "Increase verbosity. Repeatable, but more than twice does not bring \
           more."
        in
        Arg.(value & flag_all & info ["v"; "verbose"] ~doc)
      in
      match List.length vopts with
      | 0 -> Some Logs.Error
      | 1 -> Some Logs.Info
      | _ -> Some Logs.Debug
    in
    Fmt_tty.setup_std_outputs ?style_renderer () ;
    Logs.set_level level ;
    Logs.set_reporter (Logs_fmt.reporter ())

  let integrity_check =
    Cmd.v
      (Cmd.info
         ~doc:"search the store for integrity faults and corruption"
         "integrity-check")
      Term.(
        ret
          (const (fun () -> integrity_check)
          $ setup_logs $ Shared_arg.Term.config_file $ Shared_arg.Term.data_dir
          $ auto_repair $ block))

  let stat_index =
    Cmd.v
      (Cmd.info
         ~doc:"print high-level statistics about the index store"
         "stat-index")
      Term.(
        ret
          (const (fun () -> stat_index)
          $ setup_logs $ Shared_arg.Term.config_file $ Shared_arg.Term.data_dir
          ))

  let stat_pack =
    Cmd.v
      (Cmd.info
         ~doc:"print high-level statistics about the pack file"
         "stat-pack")
      Term.(
        ret
          (const (fun () -> stat_pack)
          $ setup_logs $ Shared_arg.Term.config_file $ Shared_arg.Term.data_dir
          ))

  let reconstruct_index =
    Cmd.v
      (Cmd.info ~doc:"reconstruct index from pack file" "reconstruct-index")
      Term.(
        ret
          (const (fun () -> reconstruct_index)
          $ setup_logs $ Shared_arg.Term.config_file $ Shared_arg.Term.data_dir
          $ dest $ index_log_size))

  let integrity_check_inodes =
    Cmd.v
      (Cmd.info
         ~doc:
           "search the store for corrupted inodes. If no block hash is \
            provided (through the $(b,--head) argument) then the current head \
            is chosen as the default context to start with"
         "integrity-check-inodes")
      Term.(
        ret
          (const (fun () -> integrity_check_inodes)
          $ setup_logs $ Shared_arg.Term.config_file $ Shared_arg.Term.data_dir
          $ block))

  let integrity_check_index =
    Cmd.v
      (Cmd.info ~doc:"checks the index for corruptions" "integrity-check-index")
      Term.(
        ret
          (const (fun () -> check_index)
          $ setup_logs $ Shared_arg.Term.config_file $ Shared_arg.Term.data_dir
          $ auto_repair))

  let head_commit =
    Cmd.v
      (Cmd.info
         ~doc:"prints the current head's context commit hash"
         "head-commit")
      Term.(
        ret
          (const (fun () -> find_head)
          $ setup_logs $ Shared_arg.Term.config_file $ Shared_arg.Term.data_dir
          ))

  let report_path =
    let open Cmdliner.Arg in
    let default_path = "./report.json" in
    value
    & opt string default_path
      @@ info
           ~doc:
             (Format.sprintf
                "Path to the report generated by the command. Default is \"%s\""
                default_path)
           ~docv:"REPORT_PATH"
           ["report-path"]

  let stat_metadata =
    Cmd.v
      (Cmd.info
         ~doc:
           "displays the status of the metadata, giving useful information \
            such as count, size, too large."
         "stat-metadata")
      Term.(
        ret
          (const (fun () -> stat_metadata)
          $ setup_logs $ Shared_arg.Term.config_file $ Shared_arg.Term.data_dir
          $ report_path))

  let commands =
    [
      integrity_check;
      stat_index;
      stat_pack;
      reconstruct_index;
      integrity_check_inodes;
      integrity_check_index;
      head_commit;
      stat_metadata;
    ]
end

module Manpage = struct
  let command_description =
    "The $(b,storage) command provides tools for introspecting and debugging \
     the storage layer."

  let commands =
    [
      `P
        "$(b,WARNING): this API is experimental and may change in future \
         versions.";
    ]

  let man = commands @ Shared_arg.Manpage.bugs

  let info =
    Cmdliner.Cmd.info
      ~doc:"Query the storage layer (EXPERIMENTAL)"
      ~man
      "storage"
end

let cmd = Cmdliner.Cmd.group Manpage.info Term.commands
