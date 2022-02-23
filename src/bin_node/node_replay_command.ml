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

  let pp_json ppf json =
    Format.fprintf
      ppf
      "%s"
      (Data_encoding.Json.to_string ~newline:false ~minify:true json)

  let inconsistent_block_receipt =
    declare_2
      ~section
      ~name:"inconsistent_block_receipt"
      ~msg:
        "inconsistent block receipt - expected: {expected}, replay result: \
         {replay_result}"
      ~level:Error
      ~pp1:pp_json
      ("expected", Data_encoding.json)
      ~pp2:pp_json
      ("replay_result", Data_encoding.json)

  let inconsistent_operation_receipt =
    declare_3
      ~section
      ~name:"inconsistent_operation_receipt"
      ~msg:
        "inconsistent operation receipt at {operation_index} - expected: \
         {expected}, replay result: {replay_result}"
      ~level:Notice
      ~pp1:(fun ppf (l, o) -> Format.fprintf ppf "(list %d, index %d)" l o)
      ("operation_index", Data_encoding.(tup2 int31 int31))
      ~pp2:pp_json
      ("expected", Data_encoding.json)
      ~pp3:pp_json
      ("replay_result", Data_encoding.json)
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
         impossible operation. This can happpen if the block is a genesis \
         (main chain or test chain) or if it's the oldest non purged block in \
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

let replay ~singleprocess (config : Node_config_file.t) blocks =
  let open Lwt_tzresult_syntax in
  let store_root = Node_data_version.store_dir config.data_dir in
  let context_root = Node_data_version.context_dir config.data_dir in
  let protocol_root = Node_data_version.protocol_dir config.data_dir in
  let genesis = config.blockchain_network.genesis in
  let (validator_env : Block_validator_process.validator_environment) =
    {
      user_activated_upgrades = config.blockchain_network.user_activated_upgrades;
      user_activated_protocol_overrides =
        config.blockchain_network.user_activated_protocol_overrides;
    }
  in
  let* (validator_process, store) =
    if singleprocess then
      let* store =
        Store.init
          ~store_dir:store_root
          ~context_dir:context_root
          ~allow_testchains:false
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
               genesis;
               context_root;
               protocol_root;
               process_path = Sys.executable_name;
               sandbox_parameters = None;
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
          ~commit_genesis
          genesis
      in
      return (validator_process, store)
  in
  let main_chain_store = Store.main_chain_store store in
  Lwt.finalize
    (fun () ->
      List.iter_es
        (fun block ->
          let block_alias =
            match block with
            | `Head _ | `Genesis | `Alias _ ->
                Some (Block_services.to_string block)
            | _ -> None
          in
          let* block =
            protect
              ~on_error:(fun _ -> fail Block_not_found)
              (fun () ->
                let*! o =
                  Store.Chain.block_of_identifier_opt main_chain_store block
                in
                match o with
                | None -> fail Block_not_found
                | Some block -> return block)
          in
          let predecessor_hash = Store.Block.predecessor block in
          let*! predecessor_opt =
            Store.Block.read_block_opt main_chain_store predecessor_hash
          in
          match predecessor_opt with
          | None -> fail Cannot_replay_orphan
          | Some predecessor ->
              let*! (_, savepoint_level) =
                Store.Chain.savepoint main_chain_store
              in
              if Store.Block.level block <= savepoint_level then
                fail Cannot_replay_below_savepoint
              else
                let expected_context_hash = Store.Block.context_hash block in
                let* metadata =
                  Store.Block.get_block_metadata main_chain_store block
                in
                let expected_block_receipt =
                  Store.Block.block_metadata metadata
                in
                let expected_operation_receipts =
                  Store.Block.operations_metadata metadata
                in
                let operations = Store.Block.operations block in
                let header = Store.Block.header block in
                let start_time = Time.System.now () in
                let*! () =
                  Event.(emit block_validation_start)
                    ( block_alias,
                      Store.Block.hash block,
                      Store.Block.level block )
                in
                let* result =
                  Block_validator_process.apply_block
                    validator_process
                    main_chain_store
                    ~predecessor
                    header
                    operations
                in
                let now = Time.System.now () in
                let*! () =
                  Event.(emit block_validation_end) (Ptime.diff now start_time)
                in
                let*! () =
                  if
                    not
                      (Context_hash.equal
                         expected_context_hash
                         result.validation_store.context_hash)
                  then
                    Event.(emit inconsistent_context_hash)
                      ( expected_context_hash,
                        result.validation_store.context_hash )
                  else Lwt.return_unit
                in
                let* () =
                  if
                    not
                      (Bytes.equal expected_block_receipt result.block_metadata)
                  then
                    let* protocol =
                      Store.Block.protocol_hash main_chain_store block
                    in
                    let* (module Proto) =
                      Registered_protocol.get_result protocol
                    in
                    let to_json block =
                      Data_encoding.Json.construct
                        Proto.block_header_metadata_encoding
                      @@ Data_encoding.Binary.of_bytes_exn
                           Proto.block_header_metadata_encoding
                           block
                    in
                    let exp = to_json expected_block_receipt in
                    let got = to_json result.block_metadata in
                    let*! () =
                      Event.(emit inconsistent_block_receipt) (exp, got)
                    in
                    return_unit
                  else return_unit
                in
                let rec check_receipts i j exp got =
                  match (exp, got) with
                  | ([], []) -> return_unit
                  | ([], _ :: _) | (_ :: _, []) -> assert false
                  | ([] :: exps, [] :: gots) ->
                      check_receipts (succ i) 0 exps gots
                  | ((_ :: _) :: _, [] :: _) | ([] :: _, (_ :: _) :: _) ->
                      assert false
                  | ((exp :: exps) :: expss, (got :: gots) :: gotss) ->
                      let* () =
                        if not (Bytes.equal exp got) then
                          let* protocol =
                            Store.Block.protocol_hash main_chain_store block
                          in
                          let* (module Proto) =
                            Registered_protocol.get_result protocol
                          in
                          let op =
                            operations
                            |> (fun l -> List.nth_opt l i)
                            |> WithExceptions.Option.get ~loc:__LOC__
                            |> (fun l -> List.nth_opt l j)
                            |> WithExceptions.Option.get ~loc:__LOC__
                            |> fun {proto; _} -> proto
                          in
                          let to_json receipt =
                            Data_encoding.Json.construct
                              Proto.operation_data_and_receipt_encoding
                              Data_encoding.Binary.
                                ( of_bytes_exn Proto.operation_data_encoding op,
                                  of_bytes_exn
                                    Proto.operation_receipt_encoding
                                    receipt )
                          in
                          let exp = to_json exp in
                          let got = to_json got in
                          let*! () =
                            Event.(emit inconsistent_operation_receipt)
                              ((i, j), exp, got)
                          in
                          return_unit
                        else return_unit
                      in
                      check_receipts i (succ j) (exps :: expss) (gots :: gotss)
                in
                check_receipts
                  0
                  0
                  expected_operation_receipts
                  result.ops_metadata)
        blocks)
    (fun () ->
      let*! () = Block_validator_process.close validator_process in
      Store.close_store store)

let run ?verbosity ~singleprocess (config : Node_config_file.t) block =
  let open Lwt_result_syntax in
  let* () = Node_data_version.ensure_data_dir config.data_dir in
  Lwt_lock_file.try_with_lock
    ~when_locked:(fun () ->
      failwith "Data directory is locked by another process")
    ~filename:(Node_data_version.lock_file config.data_dir)
  @@ fun () ->
  (* Main loop *)
  let log_cfg =
    match verbosity with
    | None -> config.log
    | Some default_level -> {config.log with default_level}
  in
  let*! () =
    Tezos_base_unix.Internal_event_unix.init
      ~lwt_log_sink:log_cfg
      ~configuration:config.internal_events
      ()
  in
  Updater.init (Node_data_version.protocol_dir config.data_dir) ;
  Lwt_exit.(
    wrap_and_exit
    @@ let*! res = protect (fun () -> replay ~singleprocess config block) in
       let*! () = Tezos_base_unix.Internal_event_unix.close () in
       Lwt.return res)

let check_data_dir dir =
  let open Lwt_syntax in
  let* dir_exists = Lwt_unix.file_exists dir in
  fail_unless
    dir_exists
    (Node_data_version.Invalid_data_dir
       {
         data_dir = dir;
         msg = Some (Format.sprintf "directory '%s' does not exists" dir);
       })

let process verbosity singleprocess block args =
  let verbosity =
    let open Internal_event in
    match verbosity with [] -> None | [_] -> Some Info | _ -> Some Debug
  in
  let run =
    let open Lwt_result_syntax in
    let* data_dir = Node_shared_arg.read_data_dir args in
    let* () = check_data_dir data_dir in
    let* config = Node_shared_arg.read_and_patch_config_file args in
    run ?verbosity ~singleprocess config block
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
      value & flag_all
      & info ~docs:Node_shared_arg.Manpage.misc_section ~doc ["v"])

  let block =
    let open Cmdliner in
    let doc =
      "A sequence of blocks to replay. It may either be a b58 encoded block \
       hash, a level as an integer or an alias such as $(i,caboose), \
       $(i,checkpoint), $(i,savepoint) or $(i,head)."
    in
    let block =
      Arg.conv
        ( (fun s ->
            match Tezos_shell_services.Block_services.parse_block s with
            | Ok r -> Ok r
            | Error _ -> Error (`Msg "cannot decode block argument")),
          fun ppf b ->
            Format.fprintf
              ppf
              "%s"
              (Tezos_shell_services.Block_services.to_string b) )
    in
    Arg.(
      value
      & pos_all block [`Head 0]
      & info
          ~docs:Node_shared_arg.Manpage.misc_section
          ~doc
          ~docv:"<level>|<block_hash>|<alias>"
          [])

  let singleprocess =
    let open Cmdliner in
    let doc =
      "When enabled, it deactivates block validation using an external \
       process. Thus, the validation procedure is done in the same process as \
       the node and might not be responding when doing extensive I/Os."
    in
    Arg.(
      value & flag
      & info ~docs:Node_shared_arg.Manpage.misc_section ~doc ["singleprocess"])

  let term =
    Cmdliner.Term.(
      ret
        (const process $ verbosity $ singleprocess $ block
       $ Node_shared_arg.Term.args))
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
        (TzString.Set.elements (Internal_event.get_registered_sections ()))
    in
    [
      `S "DEBUG";
      `P
        ("The environment variable $(b,TEZOS_LOG) is used to fine-tune what is \
          going to be logged. The syntax is \
          $(b,TEZOS_LOG='<section> -> <level> [ ; ...]') where section is \
          one of $(i," ^ log_sections
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
    description @ Node_shared_arg.Manpage.args @ debug @ examples
    @ Node_shared_arg.Manpage.bugs

  let info =
    Cmdliner.Term.info
      ~doc:"Replay a set of previously validated blocks"
      ~man
      "replay"
end

let cmd = (Term.term, Manpage.info)
