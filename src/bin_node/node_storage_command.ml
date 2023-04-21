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

  let integrity_info =
    declare_3
      ~section
      ~name:"integrity_info"
      ~msg:
        "running integrity check on inodes for block {block_hash} (level \
         {block_level}) with context hash {context_hash}"
      ~level:Notice
      ~pp1:Block_hash.pp
      ("block_hash", Block_hash.encoding)
      ("block_level", Data_encoding.int32)
      ~pp3:Context_hash.pp
      ("context_hash", Context_hash.encoding)
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

  let read_config_file config_file =
    let open Lwt_result_syntax in
    let+ config =
      Option.filter Sys.file_exists config_file
      |> Option.map_es Config_file.read
    in
    Option.value ~default:Config_file.default_config config

  let ensure_context_dir context_dir =
    let open Lwt_result_syntax in
    Lwt.catch
      (fun () ->
        let*! b = Lwt_unix.file_exists context_dir in
        if not b then
          tzfail
            (Data_version.Invalid_data_dir {data_dir = context_dir; msg = None})
        else
          let pack = context_dir // "store.0.suffix" in
          let*! b = Lwt_unix.file_exists pack in
          if not b then
            tzfail
              (Data_version.Invalid_data_dir
                 {data_dir = context_dir; msg = None})
          else return_unit)
      (function
        | Unix.Unix_error _ ->
            tzfail
              (Data_version.Invalid_data_dir
                 {data_dir = context_dir; msg = None})
        | exc -> raise exc)

  let root config_file data_dir =
    let open Lwt_result_syntax in
    let* cfg = read_config_file config_file in
    let data_dir = Option.value ~default:cfg.data_dir data_dir in
    let context_dir = Data_version.context_dir data_dir in
    let* () = ensure_context_dir context_dir in
    return context_dir

  let integrity_check config_file data_dir auto_repair =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
      let* root = root config_file data_dir in
      let*! () =
        Tezos_context.Context.Checks.Pack.Integrity_check.run
          ~root
          ~auto_repair
          ~always:false
          ~heads:None
      in
      return_unit)

  let stat_index config_file data_dir =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
      let* root = root config_file data_dir in
      Tezos_context.Context.Checks.Index.Stat.run ~root ;
      return_unit)

  let stat_pack config_file data_dir =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
      let* root = root config_file data_dir in
      let*! () = Tezos_context.Context.Checks.Pack.Stat.run ~root in
      return_unit)

  let index_dir_exists context_dir output =
    let open Lwt_result_syntax in
    let index_dir = Option.value output ~default:(context_dir // "index") in
    let*! b = Lwt_unix.file_exists index_dir in
    if not b then return_unit else tzfail (Existing_index_dir index_dir)

  let reconstruct_index config_file data_dir output index_log_size =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
      let* root = root config_file data_dir in
      let* () = index_dir_exists root output in
      Tezos_context.Context.Checks.Pack.Reconstruct_index.run
        ~root
        ~output
        ~index_log_size
        () ;
      return_unit)

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

  let integrity_check_inodes config_file data_dir block =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
      let*! () = Tezos_base_unix.Internal_event_unix.init_with_defaults () in
      let* data_dir, node_config =
        Shared_arg.resolve_data_dir_and_config_file ?data_dir ?config_file ()
      in
      let ({genesis; _} : Config_file.blockchain_network) =
        node_config.blockchain_network
      in
      let chain_id = Chain_id.of_block_hash genesis.block in
      let* () = Data_version.ensure_data_dir genesis data_dir in
      let context_dir = Data_version.context_dir data_dir in
      let store_dir = Data_version.store_dir data_dir in
      let* store =
        Store.init ~store_dir ~context_dir ~allow_testchains:false genesis
      in
      let* chain_store = Store.get_chain_store store chain_id in
      let* block = resolve_block chain_store block in
      let*! () = Store.close_store store in
      let context_hash = Store.Block.context_hash block in
      let context_hash_str = Context_hash.to_b58check context_hash in
      let*! () =
        Event.(
          emit
            integrity_info
            (Store.Block.hash block, Store.Block.level block, context_hash))
      in
      let*! () =
        Tezos_context.Context.Checks.Pack.Integrity_check_inodes.run
          ~root:context_dir
          ~heads:(Some [context_hash_str])
      in
      return_unit)

  let check_index config_file data_dir auto_repair =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
      let* root = root config_file data_dir in
      Tezos_context.Context.Checks.Pack.Integrity_check_index.run
        ~root
        ~auto_repair
        () ;
      return_unit)

  let find_head config_file data_dir =
    Shared_arg.process_command
      (let open Lwt_result_syntax in
      let*! () = Tezos_base_unix.Internal_event_unix.init_with_defaults () in
      let* data_dir, node_config =
        Shared_arg.resolve_data_dir_and_config_file ?data_dir ?config_file ()
      in
      let ({genesis; _} : Config_file.blockchain_network) =
        node_config.blockchain_network
      in
      let chain_id = Chain_id.of_block_hash genesis.block in
      let* () = Data_version.ensure_data_dir genesis data_dir in
      let context_dir = Data_version.context_dir data_dir in
      let store_dir = Data_version.store_dir data_dir in
      let* store =
        Store.init ~store_dir ~context_dir ~allow_testchains:false genesis
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

  let commands =
    [
      Cmd.v
        (Cmd.info
           ~doc:"search the store for integrity faults and corruption"
           "integrity-check")
        Term.(
          ret
            (const (fun () -> integrity_check)
            $ setup_logs $ Shared_arg.Term.config_file
            $ Shared_arg.Term.data_dir $ auto_repair));
      Cmd.v
        (Cmd.info
           ~doc:"print high-level statistics about the index store"
           "stat-index")
        Term.(
          ret
            (const (fun () -> stat_index)
            $ setup_logs $ Shared_arg.Term.config_file
            $ Shared_arg.Term.data_dir));
      Cmd.v
        (Cmd.info
           ~doc:"print high-level statistics about the pack file"
           "stat-pack")
        Term.(
          ret
            (const (fun () -> stat_pack)
            $ setup_logs $ Shared_arg.Term.config_file
            $ Shared_arg.Term.data_dir));
      Cmd.v
        (Cmd.info ~doc:"reconstruct index from pack file" "reconstruct-index")
        Term.(
          ret
            (const (fun () -> reconstruct_index)
            $ setup_logs $ Shared_arg.Term.config_file
            $ Shared_arg.Term.data_dir $ dest $ index_log_size));
      Cmd.v
        (Cmd.info
           ~doc:
             "search the store for corrupted inodes. If no block hash is \
              provided (through the $(b,--head) argument) then the current \
              head is chosen as the default context to start with"
           "integrity-check-inodes")
        Term.(
          ret
            (const (fun () -> integrity_check_inodes)
            $ setup_logs $ Shared_arg.Term.config_file
            $ Shared_arg.Term.data_dir $ block));
      Cmd.v
        (Cmd.info
           ~doc:"checks the index for corruptions"
           "integrity-check-index")
        Term.(
          ret
            (const (fun () -> check_index)
            $ setup_logs $ Shared_arg.Term.config_file
            $ Shared_arg.Term.data_dir $ auto_repair));
      Cmd.v
        (Cmd.info
           ~doc:"prints the current head's context commit hash"
           "head-commit")
        Term.(
          ret
            (const (fun () -> find_head)
            $ setup_logs $ Shared_arg.Term.config_file
            $ Shared_arg.Term.data_dir));
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
