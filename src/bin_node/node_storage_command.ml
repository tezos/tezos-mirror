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

type error += Existing_index_dir of string

let () =
  register_error_kind
    `Permanent
    ~id:"existingIndexDir"
    ~title:"Existing context/index directory"
    ~description:"The index directory cannot be overwritten"
    ~pp:(fun ppf path ->
      Format.fprintf ppf "Existing index directory '%s'." path)
    Data_encoding.(obj1 (req "indexdir_path" string))
    (function Existing_index_dir path -> Some path | _ -> None)
    (fun path -> Existing_index_dir path)

module Term = struct
  open Cmdliner
  open Node_shared_arg.Term

  let ( let+ ) t f = Term.(const f $ t)

  let ( and+ ) a b = Term.(const (fun x y -> (x, y)) $ a $ b)

  type subcommand =
    | Stat_index
    | Stat_pack
    | Integrity_check
    | Reconstruct_index
    | Integrity_check_inodes
    | Integrity_check_index
    | Head_commit

  let read_config_file config_file =
    let open Lwt_result_syntax in
    let+ config =
      Option.filter Sys.file_exists config_file
      |> Option.map_es Node_config_file.read
    in
    Option.value ~default:Node_config_file.default_config config

  let ensure_context_dir context_dir =
    let open Lwt_result_syntax in
    Lwt.catch
      (fun () ->
        let*! b = Lwt_unix.file_exists context_dir in
        if not b then
          tzfail
            (Node_data_version.Invalid_data_dir
               {data_dir = context_dir; msg = None})
        else
          let pack = context_dir // "store.pack" in
          let*! b = Lwt_unix.file_exists pack in
          if not b then
            tzfail
              (Node_data_version.Invalid_data_dir
                 {data_dir = context_dir; msg = None})
          else return_unit)
      (function
        | Unix.Unix_error _ ->
            tzfail
              (Node_data_version.Invalid_data_dir
                 {data_dir = context_dir; msg = None})
        | exc -> raise exc)

  let root config_file data_dir =
    let open Lwt_result_syntax in
    let* cfg = read_config_file config_file in
    let data_dir = Option.value ~default:cfg.data_dir data_dir in
    let context_dir = Node_data_version.context_dir data_dir in
    let* () = ensure_context_dir context_dir in
    return context_dir

  let integrity_check config_file data_dir auto_repair =
    let open Lwt_result_syntax in
    let* root = root config_file data_dir in
    let*! () = Context.Checks.Pack.Integrity_check.run ~root ~auto_repair in
    return_unit

  let stat_index config_file data_dir =
    let open Lwt_result_syntax in
    let* root = root config_file data_dir in
    Context.Checks.Index.Stat.run ~root ;
    return_unit

  let stat_pack config_file data_dir =
    let open Lwt_result_syntax in
    let* root = root config_file data_dir in
    let*! () = Context.Checks.Pack.Stat.run ~root in
    return_unit

  let index_dir_exists context_dir output =
    let open Lwt_result_syntax in
    let index_dir = Option.value output ~default:(context_dir // "index") in
    let*! b = Lwt_unix.file_exists index_dir in
    if not b then return_unit else tzfail (Existing_index_dir index_dir)

  let reconstruct_index config_file data_dir output index_log_size =
    let open Lwt_result_syntax in
    let* root = root config_file data_dir in
    let* () = index_dir_exists root output in
    Context.Checks.Pack.Reconstruct_index.run ~root ~output ~index_log_size () ;
    return_unit

  let to_context_hash chain_store (hash : Block_hash.t) =
    let open Lwt_result_syntax in
    let* block = Store.Block.read_block chain_store hash in
    return (Store.Block.context_hash block)

  let current_head config_file data_dir block =
    let open Lwt_result_syntax in
    let* cfg = read_config_file config_file in
    let ({genesis; _} : Node_config_file.blockchain_network) =
      cfg.blockchain_network
    in
    let data_dir = Option.value ~default:cfg.data_dir data_dir in
    let store_dir = Node_data_version.store_dir data_dir in
    let context_dir = Node_data_version.context_dir data_dir in
    let* store =
      Store.init ~store_dir ~context_dir ~allow_testchains:false genesis
    in
    let genesis = cfg.blockchain_network.genesis in
    let chain_id = Chain_id.of_block_hash genesis.block in
    let* chain_store = Store.get_chain_store store chain_id in
    let to_context_hash = to_context_hash chain_store in
    let* block_hash =
      match block with
      | Some block -> Lwt.return (Block_hash.of_b58check block)
      | None ->
          let*! head = Store.Chain.current_head chain_store in
          return (Store.Block.hash head)
    in
    let* context_hash = to_context_hash block_hash in
    let*! () = Store.close_store store in
    return (Context_hash.to_b58check context_hash)

  let integrity_check_inodes config_file data_dir block =
    let open Lwt_result_syntax in
    let* root = root config_file data_dir in
    let* head = current_head config_file data_dir block in
    let*! () =
      Context.Checks.Pack.Integrity_check_inodes.run ~root ~heads:(Some [head])
    in
    return_unit

  let check_index config_file data_dir auto_repair =
    let open Lwt_result_syntax in
    let* root = root config_file data_dir in
    Context.Checks.Pack.Integrity_check_index.run ~root ~auto_repair () ;
    return_unit

  let find_head config_file data_dir head =
    let open Lwt_result_syntax in
    let* head = current_head config_file data_dir head in
    (* This output isn't particularly useful for most users,
       it will typically be used to inspect context
       directories using Irmin tooling *)
    let () = print_endline head in
    return_unit

  let dispatch_subcommand subcommand config_file data_dir auto_repair dest head
      log_size =
    let run =
      match subcommand with
      | Stat_index -> stat_index config_file data_dir
      | Stat_pack -> stat_pack config_file data_dir
      | Integrity_check -> integrity_check config_file data_dir auto_repair
      | Reconstruct_index ->
          reconstruct_index config_file data_dir dest log_size
      | Integrity_check_inodes ->
          integrity_check_inodes config_file data_dir head
      | Integrity_check_index -> check_index config_file data_dir auto_repair
      | Head_commit -> find_head config_file data_dir head
    in
    match Lwt_main.run @@ Lwt_exit.wrap_and_exit run with
    | Ok () -> `Ok ()
    | Error err -> `Error (false, Format.asprintf "%a" pp_print_trace err)

  let subcommand_arg =
    let parser = function
      | "stat-index" -> `Ok Stat_index
      | "stat-pack" -> `Ok Stat_pack
      | "integrity-check" -> `Ok Integrity_check
      | "reconstruct-index" -> `Ok Reconstruct_index
      | "integrity-check-inodes" -> `Ok Integrity_check_inodes
      | "integrity-check-index" -> `Ok Integrity_check_index
      | "head-commit" -> `Ok Head_commit
      | s -> `Error ("invalid argument: " ^ s)
    and printer ppf = function
      | Stat_index -> Format.fprintf ppf "stat-index"
      | Stat_pack -> Format.fprintf ppf "stat-pack"
      | Integrity_check -> Format.fprintf ppf "integrity-check"
      | Reconstruct_index -> Format.fprintf ppf "reconstruct-index"
      | Integrity_check_inodes -> Format.fprintf ppf "integrity-check-inodes"
      | Integrity_check_index -> Format.fprintf ppf "integrity-check-index"
      | Head_commit -> Format.fprintf ppf "head-commit"
    in
    let open Cmdliner.Arg in
    let doc =
      "Operation to perform. Possible values: $(b,stat-index), $(b,stat-pack), \
       $(b,integrity-check), $(b,reconstruct-index), \
       $(b,integrity-check-inodes), $(b,integrity-check-index), $(b,head)."
    in
    required
    & pos 0 (some (parser, printer)) None
    & info [] ~docv:"OPERATION" ~doc

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

  let head =
    let open Cmdliner.Arg in
    value
    & opt (some string) None
      @@ info
           ~doc:"Head; option for integrity-check-inodes."
           ~docv:"HEAD"
           ["head"; "h"]

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

  let term =
    Term.(
      ret
        (const (fun () -> dispatch_subcommand)
        $ setup_logs $ subcommand_arg $ config_file $ data_dir $ auto_repair
        $ dest $ head $ index_log_size))
end

module Manpage = struct
  let command_description =
    "The $(b,storage) command provides tools for introspecting and debugging \
     the storage layer."

  let commands =
    [
      `S Cmdliner.Manpage.s_commands;
      `P "The following subcommands are available:";
      `P
        "$(b,integrity-check) search the store for integrity faults and \
         corruption.";
      `P "$(b,stat-index) print high-level statistics about the index store.";
      `P "$(b,stat-pack) print high-level statistics about the pack file.";
      `P "$(b,reconstruct-index) reconstruct index from pack file.";
      `P
        "$(b,integrity-check-inodes) search the store for corrupted inodes. If \
         no block hash is provided (through the $(b,--head) argument) then the \
         current head is chosen as the default context to start with.";
      `P
        "$(b,integrity-check-index) checks the index for corruptions. If \
         $(b,--auto-repair) flag is set it also tries to repair the index.";
      `P "$(b,head-commit) prints the current head's context commit hash.";
      `P
        "$(b,WARNING): this API is experimental and may change in future \
         versions.";
    ]

  let man = commands @ Node_shared_arg.Manpage.bugs

  let info =
    Cmdliner.Term.info
      ~doc:"Query the storage layer (EXPERIMENTAL)"
      ~man
      "storage"
end

let cmd = (Term.term, Manpage.info)
