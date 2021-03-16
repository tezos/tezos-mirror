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

  type subcommand =
    | Stat_index
    | Stat_pack
    | Integrity_check
    | Reconstruct_index
    | Integrity_check_inodes

  let read_config_file config_file =
    match config_file with
    | Some config_file when Sys.file_exists config_file ->
        Node_config_file.read config_file
    | _ ->
        return Node_config_file.default_config

  let ensure_context_dir context_dir =
    Lwt.catch
      (fun () ->
        Lwt_unix.file_exists context_dir
        >>= function
        | false ->
            fail (Node_data_version.Invalid_data_dir context_dir)
        | true -> (
            let pack = context_dir // "store.pack" in
            Lwt_unix.file_exists pack
            >>= function
            | false ->
                fail (Node_data_version.Invalid_data_dir context_dir)
            | true ->
                return_unit ))
      (function
        | Unix.Unix_error _ ->
            fail (Node_data_version.Invalid_data_dir context_dir)
        | exc ->
            raise exc)

  let root config_file data_dir =
    read_config_file config_file
    >>=? fun cfg ->
    let data_dir = Option.value ~default:cfg.data_dir data_dir in
    let context_dir = Node_data_version.context_dir data_dir in
    ensure_context_dir context_dir >>=? fun () -> return context_dir

  let integrity_check config_file data_dir auto_repair =
    root config_file data_dir
    >>=? fun root ->
    Context.Checks.Pack.Integrity_check.run ~root ~auto_repair
    >>= fun () -> return_unit

  let stat_index config_file data_dir =
    root config_file data_dir
    >>=? fun root ->
    Context.Checks.Index.Stat.run ~root ;
    return_unit

  let stat_pack config_file data_dir =
    root config_file data_dir
    >>=? fun root ->
    Context.Checks.Pack.Stat.run ~root >>= fun () -> return_unit

  let index_dir_exists context_dir output =
    let index_dir = Option.value output ~default:(context_dir // "index") in
    Lwt_unix.file_exists index_dir
    >>= function
    | false -> return_unit | true -> fail (Existing_index_dir index_dir)

  let reconstruct_index config_file data_dir output =
    root config_file data_dir
    >>=? fun root ->
    index_dir_exists root output
    >>=? fun () ->
    Context.Checks.Pack.Reconstruct_index.run ~root ~output ;
    return_unit

  let to_context_hash chain_store (hash : Block_hash.t) =
    let block_store = Store.Block.get chain_store in
    Store.Block.Contents.read (block_store, hash)
    >>=? fun block_header -> return block_header.context

  let current_head config_file data_dir block =
    read_config_file config_file
    >>=? fun cfg ->
    let data_dir = Option.value ~default:cfg.data_dir data_dir in
    let store_root = Node_data_version.store_dir data_dir in
    Store.init store_root
    >>=? fun store ->
    let genesis = cfg.blockchain_network.genesis in
    let chain_id = Chain_id.of_block_hash genesis.block in
    let chain_store = Store.Chain.get store chain_id in
    let to_context_hash = to_context_hash chain_store in
    ( match block with
    | Some block ->
        Lwt.return (Block_hash.of_b58check block)
    | None ->
        let chain_data = Store.Chain_data.get chain_store in
        Store.Chain_data.Current_head.read chain_data )
    >>=? fun block_hash ->
    to_context_hash block_hash
    >>=? fun context_hash ->
    Store.close store ;
    return (Context_hash.to_b58check context_hash)

  let integrity_check_inodes config_file data_dir block =
    root config_file data_dir
    >>=? fun root ->
    current_head config_file data_dir block
    >>=? fun head ->
    Context.Checks.Pack.Integrity_check_inodes.run ~root ~heads:(Some [head])
    >>= fun () -> return_unit

  let dispatch_subcommand subcommand config_file data_dir auto_repair dest head
      =
    let run =
      match subcommand with
      | Stat_index ->
          stat_index config_file data_dir
      | Stat_pack ->
          stat_pack config_file data_dir
      | Integrity_check ->
          integrity_check config_file data_dir auto_repair
      | Reconstruct_index ->
          reconstruct_index config_file data_dir dest
      | Integrity_check_inodes ->
          integrity_check_inodes config_file data_dir head
    in
    match Lwt_main.run @@ Lwt_exit.wrap_and_exit run with
    | Ok () ->
        `Ok ()
    | Error err ->
        `Error (false, Format.asprintf "%a" pp_print_error err)

  let subcommand_arg =
    let parser = function
      | "stat-index" ->
          `Ok Stat_index
      | "stat-pack" ->
          `Ok Stat_pack
      | "integrity-check" ->
          `Ok Integrity_check
      | "reconstruct-index" ->
          `Ok Reconstruct_index
      | "integrity-check-inodes" ->
          `Ok Integrity_check_inodes
      | s ->
          `Error ("invalid argument: " ^ s)
    and printer ppf = function
      | Stat_index ->
          Format.fprintf ppf "stat-index"
      | Stat_pack ->
          Format.fprintf ppf "stat-pack"
      | Integrity_check ->
          Format.fprintf ppf "integrity-check"
      | Reconstruct_index ->
          Format.fprintf ppf "reconstruct-index"
      | Integrity_check_inodes ->
          Format.fprintf ppf "integrity-check-inodes"
    in
    let open Cmdliner.Arg in
    let doc =
      "Operation to perform. Possible values: $(b,stat-index), \
       $(b,stat-pack), $(b,integrity-check), $(b,reconstruct-index), \
       $(b,integrity-check-inodes)."
    in
    required
    & pos 0 (some (parser, printer)) None
    & info [] ~docv:"OPERATION" ~doc

  let auto_repair =
    let open Cmdliner.Arg in
    value
    & flag
      @@ info
           ~doc:"Automatically repair issues; option for integrity-check"
           ["auto-repair"]

  let dest =
    let open Cmdliner.Arg in
    value
    & opt (some string) None
      @@ info
           ~doc:"Path to the new index file; option for reconstruct-index"
           ~docv:"DEST"
           ["output"; "o"]

  let head =
    let open Cmdliner.Arg in
    value
    & opt (some string) None
      @@ info
           ~doc:"Head; option for integrity-check-inodes"
           ~docv:"HEAD"
           ["head"; "h"]

  let term =
    Term.(
      ret
        ( const dispatch_subcommand $ subcommand_arg $ config_file $ data_dir
        $ auto_repair $ dest $ head ))
end

module Manpage = struct
  let command_description =
    "The $(b,storage) command provides tools for introspecting and debugging \
     the storage layer."

  let commands =
    [ `S Cmdliner.Manpage.s_commands;
      `P "The following subcommands are available:";
      `P
        "$(b,integrity-check) search the store for integrity faults and \
         corruption.";
      `P "$(b,stat-index) print high-level statistics about the index store.";
      `P "$(b,stat-pack) print high-level statistics about the pack file.";
      `P "$(b,reconstruct-index) reconstruct index from pack file.";
      `P
        "$(b,integrity-check-inodes) search the store for corrupted inodes. \
         If no block hash is provided (through the $(b,--head) argument) then \
         the current head is chosen as the default context to start with.";
      `P
        "$(b,WARNING): this API is experimental and may change in future \
         versions." ]

  let man = commands @ Node_shared_arg.Manpage.bugs

  let info =
    Cmdliner.Term.info
      ~doc:"Query the storage layer (EXPERIMENTAL)"
      ~man
      "storage"
end

let cmd = (Term.term, Manpage.info)
