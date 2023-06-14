(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Event = struct
  include Internal_event.Simple

  let section = ["node"; "replay"]

  let block_validation_start =
    declare_3
      ~section
      ~name:"block_validation_start"
      ~msg:"replaying block {alias}{hash} ({level})"
      ~level:Notice
      ~pp1:
        (fun fmt -> function
          | None -> ()
          | Some alias -> Format.fprintf fmt "%s: " alias)
      ("alias", Data_encoding.(option string))
      ~pp2:Block_hash.pp
      ("hash", Block_hash.encoding)
      ~pp3:(fun fmt level -> Format.fprintf fmt "%ld" level)
      ("level", Data_encoding.int32)

  let block_validation_end =
    declare_1
      ~section
      ~name:"block_validation_end"
      ~msg:"block validated in {duration}"
      ~level:Notice
      ~pp1:Time.System.Span.pp_hum
      ("duration", Time.System.Span.encoding)

  let inconsistent_context_hash =
    declare_2
      ~section
      ~name:"inconsistent_context_hash"
      ~msg:
        "inconsistent context hash - expected: {expected}, replay result: \
         {replay_result}"
      ~level:Error
      ~pp1:Context_hash.pp
      ("expected", Context_hash.encoding)
      ~pp2:Context_hash.pp
      ("replay_result", Context_hash.encoding)

  let pp_json_metadata ppf json =
    Format.fprintf
      ppf
      "%s"
      (Data_encoding.Json.to_string ~newline:true ~minify:false json)

  let pp_json_metadata_opt ppf = function
    | None -> Format.fprintf ppf "no metadata available"
    | Some json -> pp_json_metadata ppf json

  let inconsistent_block_receipt =
    declare_2
      ~section
      ~name:"inconsistent_block_receipt"
      ~msg:
        "inconsistent block receipt - expected: {expected}, replay result: \
         {replay_result}"
      ~level:Error
      ~pp1:pp_json_metadata
      ("expected", Data_encoding.json)
      ~pp2:pp_json_metadata
      ("replay_result", Data_encoding.json)

  let inconsistent_operation_receipt =
    declare_5
      ~section
      ~name:"inconsistent_operation_receipt"
      ~msg:
        "inconsistent operation receipt at {operation_index}:\n\
         expected {expected_hash}: {expected}\n\
         replayed {result_hash}: {result}"
      ~level:Error
      ~pp1:(fun ppf (l, o) -> Format.fprintf ppf "(list %d, index %d)" l o)
      ("operation_index", Data_encoding.(tup2 int31 int31))
      ~pp2:Operation_metadata_hash.pp
      ("expected_hash", Operation_metadata_hash.encoding)
      ~pp3:pp_json_metadata_opt
      ("expected", Data_encoding.(obj1 (opt "receipt" json)))
      ~pp4:Operation_metadata_hash.pp
      ("result_hash", Operation_metadata_hash.encoding)
      ~pp5:pp_json_metadata_opt
      ("result", Data_encoding.(obj1 (opt "receipt" json)))

  let unexpected_receipts_layout =
    let pp_list_lengths fmt =
      Format.fprintf
        fmt
        "%a"
        Format.(
          pp_print_list
            ~pp_sep:(fun fmt () -> fprintf fmt ",")
            Format.pp_print_int)
    in
    declare_3
      ~section
      ~name:"unexpected_receipts_layout"
      ~msg:
        "unexpected receipts layout for block {hash} - expected: {expected}, \
         replay result: {replay_result}"
      ~level:Error
      ~pp1:Block_hash.pp
      ("hash", Block_hash.encoding)
      ~pp2:pp_list_lengths
      ("expected", Data_encoding.(list int31))
      ~pp3:pp_list_lengths
      ("replay_result", Data_encoding.(list int31))

  let strict_emit ~strict e v =
    let open Lwt_syntax in
    let* () = emit e v in
    if strict then Lwt_exit.exit_and_raise 1 ;
    return_unit
end

type error += Cannot_replay_orphan

type error += Block_not_found

type error += Cannot_replay_below_savepoint

let () =
  register_error_kind
    ~id:"main.replay.block_not_found"
    ~title:"Cannot replay unknown block"
    ~description:"Replay of an unknown block was requested."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Replay of an unknown block was requested.")
    `Permanent
    Data_encoding.empty
    (function Block_not_found -> Some () | _ -> None)
    (fun () -> Block_not_found) ;
  register_error_kind
    ~id:"main.replay.cannot_replay_orphan"
    ~title:"Cannot replay orphan block (genesis or caboose)"
    ~description:"Replay of a block that has no ancestor was requested."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Replay of a block that has no ancestor was requested. This is an \
         impossible operation. This can happen if the block is a genesis (main \
         chain or test chain) or if it's the oldest non purged block in \
         rolling mode.")
    `Permanent
    Data_encoding.empty
    (function Cannot_replay_orphan -> Some () | _ -> None)
    (fun () -> Cannot_replay_orphan) ;
  register_error_kind
    ~id:"main.replay.cannot_replay_below_savepoint"
    ~title:"Cannot replay below savepoint"
    ~description:
      "Replay of a block below or at the savepoint was requested, this \
       operation requires data that have been purged (or were never there if \
       the state has been imported from a snapshot)."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Replay of a block below the savepoint was requested, this operation \
         requires data that have been purged (or were never there if the state \
         has been imported from a snapshot).")
    `Permanent
    Data_encoding.empty
    (function Cannot_replay_below_savepoint -> Some () | _ -> None)
    (fun () -> Cannot_replay_below_savepoint)

(* Iterator over receipts, that are, list of lists of bytes. The index
   given to [f] corresponds to the dimension [x][y] of the list being
   inspected. *)
let receipts_iteri_2 ~when_different_lengths l r f =
  let open Lwt_result_syntax in
  let rec aux l r ri fi =
    match (l, r) with
    | [], [] -> return_unit
    | [], _ :: _ | _ :: _, [] -> when_different_lengths l r
    | [] :: exps, [] :: gots -> aux exps gots (succ ri) 0
    | (_ :: _) :: _, [] :: _ | [] :: _, (_ :: _) :: _ ->
        when_different_lengths l r
    | (exp :: exps) :: expss, (got :: gots) :: gotss ->
        let* () = f exp got ri fi in
        aux (exps :: expss) (gots :: gotss) ri (succ fi)
  in
  aux l r 0 0

let replay_one_block strict main_chain_store validator_process block =
  let open Lwt_result_syntax in
  let* block_alias =
    match block with
    | `Head _ | `Alias _ -> return_some (Block_services.to_string block)
    | `Genesis -> tzfail Cannot_replay_orphan
    | _ -> return_none
  in
  let* block =
    protect
      ~on_error:(fun _ -> tzfail Block_not_found)
      (fun () ->
        let*! o = Store.Chain.block_of_identifier_opt main_chain_store block in
        match o with
        | None -> tzfail Block_not_found
        | Some block -> return block)
  in
  let predecessor_hash = Store.Block.predecessor block in
  let*! predecessor_opt =
    Store.Block.read_block_opt main_chain_store predecessor_hash
  in
  let*? predecessor =
    Result.of_option ~error:(TzTrace.make Cannot_replay_orphan) predecessor_opt
  in
  let*! _, savepoint_level = Store.Chain.savepoint main_chain_store in
  if Store.Block.level block <= savepoint_level then
    tzfail Cannot_replay_below_savepoint
  else
    let* protocol = Store.Block.protocol_hash main_chain_store block in
    let* (module Proto) = Registered_protocol.get_result protocol in
    let* expected_context_hash =
      if Proto.expected_context_hash = Resulting_context then
        return (Store.Block.context_hash block)
      else Store.Block.resulting_context_hash main_chain_store block
    in
    let* metadata = Store.Block.get_block_metadata main_chain_store block in
    let expected_block_receipt_bytes = Store.Block.block_metadata metadata in
    let expected_operation_receipts =
      let operation_metadata = Store.Block.operations_metadata metadata in
      match Store.Block.operations_metadata_hashes block with
      | None -> Block_validation.No_metadata_hash operation_metadata
      | Some op_metadata_hash_ll ->
          Metadata_hash
            (Stdlib.List.map2
               (fun l l' -> Stdlib.List.combine l l')
               operation_metadata
               op_metadata_hash_ll)
    in
    let operations = Store.Block.operations block in
    let header = Store.Block.header block in
    let start_time = Time.System.now () in
    let*! () =
      Event.(emit block_validation_start)
        (block_alias, Store.Block.hash block, Store.Block.level block)
    in
    let* result =
      Block_validator_process.apply_block
        ~simulate:true
        validator_process
        main_chain_store
        ~predecessor
        header
        operations
    in
    let now = Time.System.now () in
    let*! () = Event.(emit block_validation_end) (Ptime.diff now start_time) in
    let* () =
      when_
        (not
           (Context_hash.equal
              expected_context_hash
              result.validation_store.resulting_context_hash))
        (fun () ->
          let*! () =
            Event.(strict_emit ~strict inconsistent_context_hash)
              ( expected_context_hash,
                result.validation_store.resulting_context_hash )
          in
          return_unit)
    in
    let block_metadata_bytes = fst result.block_metadata in
    let* () =
      (* Check that the block metadata bytes are equal.
         If not, decode and print them as Json to ease debugging. *)
      when_
        (not (Bytes.equal expected_block_receipt_bytes block_metadata_bytes))
        (fun () ->
          let to_json block =
            Data_encoding.Json.construct Proto.block_header_metadata_encoding
            @@ Data_encoding.Binary.of_bytes_exn
                 Proto.block_header_metadata_encoding
                 block
          in
          let exp = to_json expected_block_receipt_bytes in
          let got = to_json block_metadata_bytes in
          let*! () =
            Event.(strict_emit ~strict inconsistent_block_receipt) (exp, got)
          in
          return_unit)
    in
    (* Check that the operations metadata are equal.
       If not, decode and print them as Json to ease debugging. *)
    let open Block_validation in
    let check_receipt (exp_m, exp_mh) (got_m, got_mh) i j =
      let equal_hash = Operation_metadata_hash.equal exp_mh got_mh in
      let equal_content =
        match (exp_m, got_m) with
        | Metadata a, Metadata b -> Bytes.equal a b
        | Too_large_metadata, Too_large_metadata
        | Metadata _, Too_large_metadata
        | Too_large_metadata, Metadata _ ->
            true
      in
      (* Check that the operations metadata bytes are equal.
         If not, decode and print them as Json to ease debugging. *)
      when_ ((not equal_hash) || not equal_content) (fun () ->
          let* protocol = Store.Block.protocol_hash main_chain_store block in
          let* (module Proto) = Registered_protocol.get_result protocol in
          let op =
            operations
            |> (fun l -> List.nth_opt l i)
            |> WithExceptions.Option.get ~loc:__LOC__
            |> (fun l -> List.nth_opt l j)
            |> WithExceptions.Option.get ~loc:__LOC__
            |> fun {proto; _} -> proto
          in
          let to_json metadata_bytes =
            Data_encoding.Json.construct
              Proto
              .operation_data_and_receipt_encoding_with_legacy_attestation_name
              Data_encoding.Binary.
                ( of_bytes_exn
                    Proto.operation_data_encoding_with_legacy_attestation_name
                    op,
                  of_bytes_exn
                    Proto
                    .operation_receipt_encoding_with_legacy_attestation_name
                    metadata_bytes )
          in
          let exp_json_opt, got_json_opt =
            match (exp_m, got_m) with
            | Too_large_metadata, Metadata b -> (None, Some (to_json b))
            | Metadata b, Too_large_metadata -> (Some (to_json b), None)
            | Too_large_metadata, Too_large_metadata -> (None, None)
            | Metadata b, Metadata b' -> (Some (to_json b), Some (to_json b'))
          in
          let*! () =
            Event.(strict_emit ~strict inconsistent_operation_receipt)
              ((i, j), exp_mh, exp_json_opt, got_mh, got_json_opt)
          in
          return_unit)
    in
    match (expected_operation_receipts, result.ops_metadata) with
    | Metadata_hash expected, Metadata_hash result ->
        receipts_iteri_2
          ~when_different_lengths:(fun exp got ->
            let*! () =
              Event.(strict_emit ~strict unexpected_receipts_layout)
                ( Store.Block.hash block,
                  List.(map length exp),
                  List.(map length got) )
            in
            return_unit)
          expected
          result
          check_receipt
    | No_metadata_hash _, _ | _, No_metadata_hash _ ->
        (* Nothing to compare *) return_unit

let replay ~internal_events ~singleprocess ~strict
    ~operation_metadata_size_limit (config : Config_file.t) blocks =
  let open Lwt_result_syntax in
  let store_root = Data_version.store_dir config.data_dir in
  let context_root = Data_version.context_dir config.data_dir in
  let protocol_root = Data_version.protocol_dir config.data_dir in
  let genesis = config.blockchain_network.genesis in
  let (validator_env : Block_validator_process.validator_environment) =
    {
      user_activated_upgrades = config.blockchain_network.user_activated_upgrades;
      user_activated_protocol_overrides =
        config.blockchain_network.user_activated_protocol_overrides;
      operation_metadata_size_limit;
    }
  in
  let readonly = true in
  let* validator_process, store =
    if singleprocess then
      let* store =
        Store.init
          ~store_dir:store_root
          ~context_dir:context_root
          ~allow_testchains:false
          ~readonly
          genesis
      in
      let main_chain_store = Store.main_chain_store store in
      let* validator_process =
        Block_validator_process.init validator_env (Internal main_chain_store)
      in
      return (validator_process, store)
    else
      let* validator_process =
        Block_validator_process.init
          validator_env
          (External
             {
               data_dir = config.data_dir;
               readonly;
               genesis;
               context_root;
               protocol_root;
               process_path = Sys.executable_name;
               sandbox_parameters = None;
               dal_config = Tezos_crypto_dal.Cryptobox.Config.default;
               internal_events;
             })
      in
      let commit_genesis =
        Block_validator_process.commit_genesis validator_process
      in
      let* store =
        Store.init
          ~store_dir:store_root
          ~context_dir:context_root
          ~allow_testchains:false
          ~readonly
          ~commit_genesis
          genesis
      in
      return (validator_process, store)
  in
  let main_chain_store = Store.main_chain_store store in
  Lwt.finalize
    (fun () ->
      List.iter_es
        (function
          | Block_services.Block b ->
              replay_one_block strict main_chain_store validator_process b
          | Range (`Level starts, `Level ends) ->
              if Int32.compare starts ends > 0 then return_unit
              else if Int32.compare starts ends = 0 then
                replay_one_block
                  strict
                  main_chain_store
                  validator_process
                  (`Level starts)
              else
                Seq.ES.iter
                  (replay_one_block strict main_chain_store validator_process)
                  (Seq.unfold
                     (fun l ->
                       if l <= ends then Some (`Level l, Int32.succ l) else None)
                     starts))
        blocks)
    (fun () ->
      let*! () = Block_validator_process.close validator_process in
      Store.close_store store)

let run ?verbosity ~singleprocess ~strict ~operation_metadata_size_limit
    (config : Config_file.t) blocks =
  let open Lwt_result_syntax in
  let* () =
    Data_version.ensure_data_dir
      config.blockchain_network.genesis
      config.data_dir
  in
  Lwt_lock_file.try_with_lock
    ~when_locked:(fun () ->
      failwith "Data directory is locked by another process")
    ~filename:(Data_version.lock_file config.data_dir)
  @@ fun () ->
  (* Main loop *)
  let internal_events =
    Tezos_base_unix.Internal_event_unix.make_with_defaults
      ?verbosity
      ~log_cfg:config.log
      ()
  in
  let*! () =
    Tezos_base_unix.Internal_event_unix.init ~config:internal_events ()
  in
  Updater.init (Data_version.protocol_dir config.data_dir) ;
  Lwt_exit.(
    wrap_and_exit
    @@ let*! res =
         protect (fun () ->
             replay
               ~internal_events
               ~singleprocess
               ~strict
               ~operation_metadata_size_limit
               config
               blocks)
       in
       let*! () = Tezos_base_unix.Internal_event_unix.close () in
       Lwt.return res)

let check_data_dir dir =
  let open Lwt_syntax in
  let* dir_exists = Lwt_unix.file_exists dir in
  fail_unless
    dir_exists
    (Data_version.Invalid_data_dir
       {
         data_dir = dir;
         msg = Some (Format.sprintf "directory '%s' does not exists" dir);
       })

let process verbosity singleprocess strict blocks data_dir config_file
    operation_metadata_size_limit =
  let verbosity =
    let open Internal_event in
    match verbosity with [] -> None | [_] -> Some Info | _ -> Some Debug
  in
  let run =
    let open Lwt_result_syntax in
    let* config =
      let* data_dir, config =
        Shared_arg.resolve_data_dir_and_config_file ?data_dir ?config_file ()
      in
      return {config with data_dir}
    in
    let operation_metadata_size_limit =
      Option.value
        operation_metadata_size_limit
        ~default:
          config.shell.block_validator_limits.operation_metadata_size_limit
    in
    let* () = check_data_dir config.data_dir in
    run
      ?verbosity
      ~singleprocess
      ~strict
      ~operation_metadata_size_limit
      config
      blocks
  in
  match Lwt_main.run run with
  | Ok () -> `Ok ()
  | Error err -> `Error (false, Format.asprintf "%a" pp_print_trace err)

module Term = struct
  let verbosity =
    let open Cmdliner in
    let doc =
      "Increase log level. Using $(b,-v) is equivalent to using \
       $(b,TEZOS_LOG='* -> info'), and $(b,-vv) is equivalent to using \
       $(b,TEZOS_LOG='* -> debug')."
    in
    Arg.(
      value & flag_all & info ~docs:Shared_arg.Manpage.misc_section ~doc ["v"])

  let blocks =
    let open Cmdliner in
    let doc =
      "A sequence of blocks to replay. It may either be a b58 encoded block \
       hash, a level as an integer or an alias such as $(i,caboose), \
       $(i,checkpoint), $(i,savepoint), $(i,head), or a range (bounds \
       included) of the form $(i,level..level)."
    in
    let block =
      Arg.conv
        ( (fun s ->
            match
              Tezos_shell_services.Block_services.parse_block_or_range s
            with
            | Ok b -> Ok b
            | Error _ -> Error (`Msg "cannot decode block argument")),
          fun ppf b ->
            match b with
            | Block_services.Block b ->
                Format.fprintf
                  ppf
                  "%s"
                  (Tezos_shell_services.Block_services.to_string b)
            | Range (starts, ends) ->
                Format.fprintf
                  ppf
                  "%s..%s"
                  (Tezos_shell_services.Block_services.to_string
                     (starts :> Tezos_shell_services.Block_services.block))
                  (Tezos_shell_services.Block_services.to_string
                     (ends :> Tezos_shell_services.Block_services.block)) )
    in
    Arg.(
      value
      & pos_all block [Block_services.Block (`Head 0)]
      & info
          ~docs:Shared_arg.Manpage.misc_section
          ~doc
          ~docv:"<level>|<block_hash>|<alias>"
          [])

  let strict =
    let open Cmdliner in
    let doc =
      "If set, the command stops immediately when encoutering any error. \
       Otherwise the replay continues and errors are logged."
    in
    Arg.(value & flag & info ~doc ["strict"])

  let singleprocess =
    let open Cmdliner in
    let doc =
      "When enabled, it deactivates block validation using an external \
       process. Thus, the validation procedure is done in the same process as \
       the node and might not be responding when doing extensive I/Os."
    in
    Arg.(
      value & flag
      & info ~docs:Shared_arg.Manpage.misc_section ~doc ["singleprocess"])

  let term =
    Cmdliner.Term.(
      ret
        (const process $ verbosity $ singleprocess $ strict $ blocks
       $ Shared_arg.Term.data_dir $ Shared_arg.Term.config_file
       $ Shared_arg.Term.operation_metadata_size_limit))
end

module Manpage = struct
  let command_description =
    "The $(b,replay) command is meant to replay a previously validated block \
     for debugging purposes."

  let description = [`S "DESCRIPTION"; `P command_description]

  let debug =
    let log_sections =
      String.concat
        " "
        (List.of_seq (Internal_event.get_registered_sections ()))
    in
    [
      `S "DEBUG";
      `P
        ("The environment variable $(b,TEZOS_LOG) is used to fine-tune what is \
          going to be logged. The syntax is \
          $(b,TEZOS_LOG='<section> -> <level> [ ; ...]') where section is one \
          of $(i," ^ log_sections
       ^ ") and level is one of $(i,fatal), $(i,error), $(i,warn), \
          $(i,notice), $(i,info) or $(i,debug). A $(b,*) can be used as a \
          wildcard in sections, i.e. $(b, client* -> debug). The rules are \
          matched left to right, therefore the leftmost rule is highest \
          priority .");
    ]

  let examples =
    [
      `S "EXAMPLE";
      `I
        ( "$(b,Replay the last three blocks)",
          "$(mname) replay head-2 head-1 head" );
      `I ("$(b,Replay block at level twelve)", "$(mname) replay 12");
    ]

  let man =
    description @ Shared_arg.Manpage.args @ debug @ examples
    @ Shared_arg.Manpage.bugs

  let info =
    Cmdliner.Cmd.info
      ~doc:"Replay a set of previously validated blocks"
      ~man
      "replay"
end

let cmd = Cmdliner.Cmd.v Manpage.info Term.term
