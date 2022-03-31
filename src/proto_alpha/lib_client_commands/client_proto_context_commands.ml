(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Protocol
open Alpha_context
open Client_proto_context
open Client_proto_contracts
open Client_keys
open Client_proto_args

let encrypted_switch =
  Clic.switch ~long:"encrypted" ~doc:"encrypt the key on-disk" ()

let normalize_types_switch =
  Clic.switch
    ~long:"normalize-types"
    ~doc:
      "Whether types should be normalized (annotations removed, combs \
       flattened) or kept as they appeared in the original script."
    ()

let report_michelson_errors ?(no_print_source = false) ~msg
    (cctxt : #Client_context.full) = function
  | Error errs ->
      Michelson_v1_error_reporter.enrich_runtime_errors
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        ~parsed:None
        errs
      >>= fun errs ->
      cctxt#warning
        "%a"
        (Michelson_v1_error_reporter.report_errors
           ~details:(not no_print_source)
           ~show_source:(not no_print_source)
           ?parsed:None)
        errs
      >>= fun () ->
      cctxt#error "%s" msg >>= fun () -> Lwt.return_none
  | Ok data -> Lwt.return_some data

let parse_file parse path =
  Lwt_utils_unix.read_file path >>= fun contents -> parse contents

let file_or_text_parameter ~from_text
    ?(from_path = parse_file (from_text ~heuristic:false)) () =
  Clic.parameter @@ fun _ p ->
  match String.split ~limit:1 ':' p with
  | ["text"; text] -> from_text ~heuristic:false text
  | ["file"; path] -> from_path path
  | _ -> if Sys.file_exists p then from_path p else from_text ~heuristic:true p

let json_file_or_text_parameter =
  let from_text ~heuristic s =
    try return (Ezjsonm.from_string s)
    with Ezjsonm.Parse_error _ when heuristic ->
      failwith "Neither an existing file nor valid JSON: '%s'" s
  in
  let from_path = Lwt_utils_unix.Json.read_file in
  file_or_text_parameter ~from_text ~from_path ()

let non_negative_param =
  Clic.parameter (fun _ s ->
      match int_of_string_opt s with
      | Some i when i >= 0 -> return i
      | _ -> failwith "Parameter should be a non-negative integer literal")

let block_hash_param =
  Clic.parameter (fun _ s ->
      try return (Block_hash.of_b58check_exn s)
      with _ -> failwith "Parameter '%s' is an invalid block hash" s)

let tx_rollup_parameter =
  Clic.parameter (fun _ s ->
      match Tx_rollup.of_b58check_opt s with
      | Some c -> return c
      | None -> failwith "Parameter '%s' is an invalid tx rollup address" s)

let tx_rollup_param =
  Clic.param
    ~name:"tx_rollup address"
    ~desc:"The tx rollup address that we are sending this batch to."
    tx_rollup_parameter

let tx_rollup_proof_param =
  Clic.param
    ~name:"tx_rollup rejection proof"
    ~desc:"The proof associated to the rejection operation"
    string_parameter

let rollup_kind_param =
  Clic.parameter (fun _ name ->
      match Sc_rollups.from ~name with
      | None ->
          failwith
            "Parameter '%s' is not a valid rollup name (must be one of %s)"
            name
            (String.concat ", " Sc_rollups.all_names)
      | Some k -> return k)

let boot_sector_param =
  let from_text ~heuristic:_ s =
    return (fun (module R : Sc_rollups.PVM.S) ->
        R.parse_boot_sector s |> function
        | None -> failwith "Invalid boot sector"
        | Some boot_sector -> return boot_sector)
  in
  file_or_text_parameter ~from_text ()

let messages_param =
  let from_path path =
    Lwt_utils_unix.Json.read_file path >>=? fun json -> return (`Json json)
  in
  let from_text text =
    try return (`Json (Ezjsonm.from_string text))
    with Ezjsonm.Parse_error _ ->
      failwith "Given text is not valid JSON: '%s'" text
  in
  Clic.parameter @@ fun _ p ->
  match String.split ~limit:1 ':' p with
  | ["bin"; path] ->
      Lwt_utils_unix.read_file path >>= fun bin -> return (`Bin bin)
  | ["text"; text] -> from_text text
  | ["file"; path] -> from_path path
  | _ -> if Sys.file_exists p then from_path p else from_text p

let rollup_address_param =
  Clic.parameter (fun _ name ->
      match Sc_rollup.Address.of_b58check_opt name with
      | None ->
          failwith
            "Parameter '%s' is not a valid B58-encoded rollup address"
            name
      | Some addr -> return addr)

let group =
  {
    Clic.name = "context";
    title = "Block contextual commands (see option -block)";
  }

let alphanet = {Clic.name = "alphanet"; title = "Alphanet only commands"}

let binary_description =
  {Clic.name = "description"; title = "Binary Description"}

let tez_of_string_exn index field s =
  match Tez.of_string s with
  | Some t -> return t
  | None ->
      failwith
        "Invalid \xEA\x9C\xA9 notation at entry %i, field \"%s\": %s"
        index
        field
        s

let tez_of_opt_string_exn index field s =
  match s with
  | None -> return None
  | Some s -> tez_of_string_exn index field s >>=? fun s -> return (Some s)

let commands_ro () =
  let open Clic in
  [
    command
      ~group
      ~desc:"Access the timestamp of the block."
      (args1
         (switch ~doc:"output time in seconds" ~short:'s' ~long:"seconds" ()))
      (fixed ["get"; "timestamp"])
      (fun seconds (cctxt : Protocol_client_context.full) ->
        Shell_services.Blocks.Header.shell_header
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ()
        >>=? fun {timestamp = v; _} ->
        (if seconds then cctxt#message "%Ld" (Time.Protocol.to_seconds v)
        else cctxt#message "%s" (Time.Protocol.to_notation v))
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Lists all non empty contracts of the block."
      no_options
      (fixed ["list"; "contracts"])
      (fun () (cctxt : Protocol_client_context.full) ->
        list_contract_labels cctxt ~chain:cctxt#chain ~block:cctxt#block
        >>=? fun contracts ->
        List.iter_s
          (fun (alias, hash, kind) -> cctxt#message "%s%s%s" hash kind alias)
          contracts
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Lists cached contracts and their age in LRU ordering."
      no_options
      (prefixes ["list"; "cached"; "contracts"] @@ stop)
      (fun () (cctxt : Protocol_client_context.full) ->
        cached_contracts cctxt ~chain:cctxt#chain ~block:cctxt#block
        >>=? fun keys ->
        List.iter_s
          (fun (key, size) ->
            cctxt#message "%a %d" Alpha_context.Contract.pp key size)
          keys
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get the key rank of a cache key."
      no_options
      (prefixes ["get"; "cached"; "contract"; "rank"; "for"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Protocol_client_context.full) ->
        contract_rank cctxt ~chain:cctxt#chain ~block:cctxt#block contract
        >>=? fun rank ->
        match rank with
        | None ->
            cctxt#error
              "Invalid contract: %a"
              Alpha_context.Contract.pp
              contract
            >>= fun () -> return_unit
        | Some rank -> cctxt#message "%d" rank >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get cache contract size."
      no_options
      (prefixes ["get"; "cache"; "contract"; "size"] @@ stop)
      (fun () (cctxt : Protocol_client_context.full) ->
        contract_cache_size cctxt ~chain:cctxt#chain ~block:cctxt#block
        >>=? fun t ->
        cctxt#message "%d" t >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get cache contract size limit."
      no_options
      (prefixes ["get"; "cache"; "contract"; "size"; "limit"] @@ stop)
      (fun () (cctxt : Protocol_client_context.full) ->
        contract_cache_size_limit cctxt ~chain:cctxt#chain ~block:cctxt#block
        >>=? fun t ->
        cctxt#message "%d" t >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get the balance of a contract."
      no_options
      (prefixes ["get"; "balance"; "for"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Protocol_client_context.full) ->
        get_balance cctxt ~chain:cctxt#chain ~block:cctxt#block contract
        >>=? fun amount ->
        cctxt#answer "%a %s" Tez.pp amount Client_proto_args.tez_sym
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get the storage of a contract."
      (args1 (unparsing_mode_arg ~default:"Readable"))
      (prefixes ["get"; "contract"; "storage"; "for"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun unparsing_mode (_, contract) (cctxt : Protocol_client_context.full) ->
        get_storage
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~unparsing_mode
          contract
        >>=? function
        | None -> cctxt#error "This is not a smart contract."
        | Some storage ->
            cctxt#answer "%a" Michelson_v1_printer.print_expr_unwrapped storage
            >>= fun () -> return_unit);
    command
      ~group
      ~desc:
        "Get the value associated to a key in the big map storage of a \
         contract (deprecated)."
      no_options
      (prefixes ["get"; "big"; "map"; "value"; "for"]
      @@ Clic.param ~name:"key" ~desc:"the key to look for" data_parameter
      @@ prefixes ["of"; "type"]
      @@ Clic.param ~name:"type" ~desc:"type of the key" data_parameter
      @@ prefix "in"
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () key key_type (_, contract) (cctxt : Protocol_client_context.full) ->
        get_contract_big_map_value
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          contract
          (key.expanded, key_type.expanded)
        >>=? function
        | None -> cctxt#error "No value associated to this key."
        | Some value ->
            cctxt#answer "%a" Michelson_v1_printer.print_expr_unwrapped value
            >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get a value in a big map."
      (args1 (unparsing_mode_arg ~default:"Readable"))
      (prefixes ["get"; "element"]
      @@ Clic.param
           ~name:"key"
           ~desc:"the key to look for"
           (Clic.parameter (fun _ s ->
                return (Script_expr_hash.of_b58check_exn s)))
      @@ prefixes ["of"; "big"; "map"]
      @@ Clic.param
           ~name:"big_map"
           ~desc:"identifier of the big_map"
           int_parameter
      @@ stop)
      (fun unparsing_mode key id (cctxt : Protocol_client_context.full) ->
        get_big_map_value
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~unparsing_mode
          (Big_map.Id.parse_z (Z.of_int id))
          key
        >>=? fun value ->
        cctxt#answer "%a" Michelson_v1_printer.print_expr_unwrapped value
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get the code of a contract."
      (args1 (unparsing_mode_arg ~default:"Readable"))
      (prefixes ["get"; "contract"; "code"; "for"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun unparsing_mode (_, contract) (cctxt : Protocol_client_context.full) ->
        get_script
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~unparsing_mode
          contract
        >>=? function
        | None -> cctxt#error "This is not a smart contract."
        | Some {code; storage = _} -> (
            match Script_repr.force_decode code with
            | Error errs ->
                cctxt#error "%a" Environment.Error_monad.pp_trace errs
            | Ok code ->
                let {Michelson_v1_parser.source; _} =
                  Michelson_v1_printer.unparse_toplevel code
                in
                cctxt#answer "%s" source >>= return));
    command
      ~group
      ~desc:"Get the `BLAKE2B` script hash of a contract."
      no_options
      (prefixes ["get"; "contract"; "script"; "hash"; "for"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Protocol_client_context.full) ->
        get_script_hash cctxt ~chain:cctxt#chain ~block:cctxt#block contract
        >>= function
        | Error errs -> cctxt#error "%a" pp_print_trace errs
        | Ok None -> cctxt#error "This is not a smart contract."
        | Ok (Some hash) -> cctxt#answer "%a" Script_expr_hash.pp hash >|= ok);
    command
      ~group
      ~desc:"Get the type of an entrypoint of a contract."
      (args1 normalize_types_switch)
      (prefixes ["get"; "contract"; "entrypoint"; "type"; "of"]
      @@ Clic.param
           ~name:"entrypoint"
           ~desc:"the entrypoint to describe"
           entrypoint_parameter
      @@ prefixes ["for"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun normalize_types
           entrypoint
           (_, contract)
           (cctxt : Protocol_client_context.full) ->
        Michelson_v1_entrypoints.contract_entrypoint_type
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~contract
          ~entrypoint
          ~normalize_types
        >>= Michelson_v1_entrypoints.print_entrypoint_type
              cctxt
              ~emacs:false
              ~contract
              ~entrypoint);
    command
      ~group
      ~desc:"Get the entrypoint list of a contract."
      (args1 normalize_types_switch)
      (prefixes ["get"; "contract"; "entrypoints"; "for"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun normalize_types (_, contract) (cctxt : Protocol_client_context.full) ->
        Michelson_v1_entrypoints.list_contract_entrypoints
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~contract
          ~normalize_types
        >>= Michelson_v1_entrypoints.print_entrypoints_list
              cctxt
              ~emacs:false
              ~contract);
    command
      ~group
      ~desc:"Get the list of unreachable paths in a contract's parameter type."
      no_options
      (prefixes ["get"; "contract"; "unreachable"; "paths"; "for"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Protocol_client_context.full) ->
        Michelson_v1_entrypoints.list_contract_unreachables
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~contract
        >>= Michelson_v1_entrypoints.print_unreachables
              cctxt
              ~emacs:false
              ~contract);
    command
      ~group
      ~desc:"Get the delegate of a contract."
      no_options
      (prefixes ["get"; "delegate"; "for"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Protocol_client_context.full) ->
        Client_proto_contracts.get_delegate
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          contract
        >>=? function
        | None -> cctxt#message "none" >>= fun () -> return_unit
        | Some delegate ->
            Public_key_hash.rev_find cctxt delegate >>=? fun mn ->
            Public_key_hash.to_source delegate >>=? fun m ->
            cctxt#message
              "%s (%s)"
              m
              (match mn with None -> "unknown" | Some n -> "known as " ^ n)
            >>= fun () -> return_unit);
    command
      ~desc:"Get receipt for past operation"
      (args1
         (default_arg
            ~long:"check-previous"
            ~placeholder:"num_blocks"
            ~doc:"number of previous blocks to check"
            ~default:"10"
            non_negative_param))
      (prefixes ["get"; "receipt"; "for"]
      @@ param
           ~name:"operation"
           ~desc:"Operation to be looked up"
           (parameter (fun _ x ->
                match Operation_hash.of_b58check_opt x with
                | None -> Error_monad.failwith "Invalid operation hash: '%s'" x
                | Some hash -> return hash))
      @@ stop)
      (fun predecessors operation_hash (ctxt : Protocol_client_context.full) ->
        display_receipt_for_operation
          ctxt
          ~chain:ctxt#chain
          ~predecessors
          operation_hash
        >>=? fun _ -> return_unit);
    command
      ~group
      ~desc:"Summarize the current voting period"
      no_options
      (fixed ["show"; "voting"; "period"])
      (fun () (cctxt : Protocol_client_context.full) ->
        get_period_info ~chain:cctxt#chain ~block:cctxt#block cctxt
        >>=? fun info ->
        cctxt#message
          "Current period: %a\nBlocks remaining until end of period: %ld"
          Data_encoding.Json.pp
          (Data_encoding.Json.construct
             Alpha_context.Voting_period.kind_encoding
             info.current_period_kind)
          info.remaining
        >>= fun () ->
        Shell_services.Protocol.list cctxt >>=? fun known_protos ->
        get_proposals ~chain:cctxt#chain ~block:cctxt#block cctxt
        >>=? fun props ->
        let ranks =
          Environment.Protocol_hash.Map.bindings props
          |> List.sort (fun (_, v1) (_, v2) -> Int64.(compare v2 v1))
        in
        let print_proposal = function
          | None ->
              cctxt#message "The current proposal has already been cleared."
          (* The proposal is cleared on the last block of adoption period, and
             also on the last block of the exploration and promotion
             periods when the proposal is not approved *)
          | Some proposal ->
              cctxt#message "Current proposal: %a" Protocol_hash.pp proposal
        in
        match info.current_period_kind with
        | Proposal ->
            (* the current proposals are cleared on the last block of the
               proposal period *)
            if info.remaining <> 0l then
              cctxt#answer
                "Current proposals:%t"
                Format.(
                  fun ppf ->
                    pp_print_cut ppf () ;
                    pp_open_vbox ppf 0 ;
                    List.iter
                      (fun (p, w) ->
                        fprintf
                          ppf
                          "* %a %a %s (%sknown by the node)@."
                          Protocol_hash.pp
                          p
                          Tez.pp
                          (Tez.of_mutez_exn w)
                          Client_proto_args.tez_sym
                          (if List.mem ~equal:Protocol_hash.equal p known_protos
                          then ""
                          else "not "))
                      ranks ;
                    pp_close_box ppf ())
              >>= fun () -> return_unit
            else
              cctxt#message "The proposals have already been cleared."
              >>= fun () -> return_unit
        | Exploration | Promotion ->
            print_proposal info.current_proposal >>= fun () ->
            (* the ballots are cleared on the last block of these periods *)
            if info.remaining <> 0l then
              get_ballots_info ~chain:cctxt#chain ~block:cctxt#block cctxt
              >>=? fun ballots_info ->
              cctxt#answer
                "@[<v>Ballots:@,\
                \  Yay: %a %s@,\
                \  Nay: %a %s@,\
                \  Pass: %a %s@,\
                 Current participation %.2f%%, necessary quorum %.2f%%@,\
                 Current in favor %a %s, needed supermajority %a %s@]"
                Tez.pp
                (Tez.of_mutez_exn ballots_info.ballots.yay)
                Client_proto_args.tez_sym
                Tez.pp
                (Tez.of_mutez_exn ballots_info.ballots.nay)
                Client_proto_args.tez_sym
                Tez.pp
                (Tez.of_mutez_exn ballots_info.ballots.pass)
                Client_proto_args.tez_sym
                (Int32.to_float ballots_info.participation /. 100.)
                (Int32.to_float ballots_info.current_quorum /. 100.)
                Tez.pp
                (Tez.of_mutez_exn ballots_info.ballots.yay)
                Client_proto_args.tez_sym
                Tez.pp
                (Tez.of_mutez_exn ballots_info.supermajority)
                Client_proto_args.tez_sym
              >>= fun () -> return_unit
            else
              cctxt#message "The ballots have already been cleared."
              >>= fun () -> return_unit
        | Cooldown ->
            print_proposal info.current_proposal >>= fun () -> return_unit
        | Adoption ->
            print_proposal info.current_proposal >>= fun () -> return_unit);
    command
      ~group:binary_description
      ~desc:"Describe unsigned block header"
      no_options
      (fixed ["describe"; "unsigned"; "block"; "header"])
      (fun () (cctxt : Protocol_client_context.full) ->
        cctxt#message
          "%a"
          Data_encoding.Binary_schema.pp
          (Data_encoding.Binary.describe
             Alpha_context.Block_header.unsigned_encoding)
        >>= fun () -> return_unit);
    command
      ~group:binary_description
      ~desc:"Describe unsigned operation"
      no_options
      (fixed ["describe"; "unsigned"; "operation"])
      (fun () (cctxt : Protocol_client_context.full) ->
        cctxt#message
          "%a"
          Data_encoding.Binary_schema.pp
          (Data_encoding.Binary.describe
             Alpha_context.Operation.unsigned_encoding)
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get the frozen deposits limit of a delegate."
      no_options
      (prefixes ["get"; "deposits"; "limit"; "for"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source delegate"
      @@ stop)
      (fun () (_, contract) (cctxt : Protocol_client_context.full) ->
        match Contract.is_implicit contract with
        | None ->
            cctxt#error
              "Cannot change deposits limit on contract %a. This operation is \
               invalid on originated contracts."
              Contract.pp
              contract
        | Some delegate -> (
            get_frozen_deposits_limit
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              delegate
            >>=? function
            | None -> cctxt#answer "unlimited" >>= return
            | Some limit ->
                cctxt#answer "%a %s" Tez.pp limit Client_proto_args.tez_sym
                >>= return));
  ]

(* ----------------------------------------------------------------------------*)
(* After the activation of a new version of the protocol, the older protocols
 are only kept in the code base to replay the history of the chain and to query
 old states.

 The commands that are not useful anymore in the old protocols are removed,
 this is called protocol freezing. The commands below are those that can be
 removed during protocol freezing.

 The rule of thumb to know if a command should be kept at freezing is that all
 commands that modify the state of the chain should be removed and conversely
 all commands that are used to query the context should be kept. For this
 reason, we call read-only (or RO for short) the commands that are kept and
 read-write (or RW for short) the commands that are removed.

 There are some exceptions to this rule however, for example the command
 "tezos-client wait for <op> to be included" is classified as RW despite having
 no effect on the context because it has no use case once all RW commands are
 removed.

 Keeping this in mind, the developer should decide where to add a new command.
 At the end of the file, RO and RW commands are concatenated into one list that
 is then exported in the mli file.  *)
(* ----------------------------------------------------------------------------*)

let dry_run_switch =
  Clic.switch
    ~long:"dry-run"
    ~short:'D'
    ~doc:"don't inject the operation, just display it"
    ()

let verbose_signing_switch =
  Clic.switch
    ~long:"verbose-signing"
    ~doc:"display extra information before signing the operation"
    ()

let simulate_switch =
  Clic.switch
    ~long:"simulation"
    ~doc:
      "Simulate the execution of the command, without needing any signatures."
    ()

let force_switch =
  Clic.switch
    ~long:"force"
    ~doc:
      "Inject the operation even if the simulation results in a failure. This \
       switch requires --gas-limit, --storage-limit, and --fee."
    ()

let transfer_command amount source destination (cctxt : #Client_context.printer)
    ( fee,
      dry_run,
      verbose_signing,
      simulation,
      force,
      gas_limit,
      storage_limit,
      counter,
      arg,
      no_print_source,
      minimal_fees,
      minimal_nanotez_per_byte,
      minimal_nanotez_per_gas_unit,
      force_low_fee,
      fee_cap,
      burn_cap,
      entrypoint,
      replace_by_fees,
      successor_level ) =
  let fee_parameter =
    {
      Injection.minimal_fees;
      minimal_nanotez_per_byte;
      minimal_nanotez_per_gas_unit;
      force_low_fee;
      fee_cap;
      burn_cap;
    }
  in
  (* When --force is used we want to inject the transfer even if it fails.
     In that case we cannot rely on simulation to compute limits and fees
     so we require the corresponding options to be set. *)
  let check_force_dependency name = function
    | None ->
        cctxt#error
          "When the --force switch is used, the %s option is required."
          name
    | _ -> Lwt.return_unit
  in
  (if force then
   check_force_dependency "--gas-limit" gas_limit >>= fun () ->
   check_force_dependency "--storage-limit" storage_limit >>= fun () ->
   check_force_dependency "--fee" fee
  else Lwt.return_unit)
  >>= fun () ->
  (match Contract.is_implicit source with
  | None ->
      let contract = source in
      Managed_contract.get_contract_manager cctxt source >>=? fun source ->
      Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
      Managed_contract.transfer
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        ?confirmations:cctxt#confirmations
        ~dry_run
        ~verbose_signing
        ~simulation
        ~force
        ~fee_parameter
        ?fee
        ~contract
        ~source
        ~src_pk
        ~src_sk
        ~destination
        ?entrypoint
        ?arg
        ~amount
        ?gas_limit
        ?storage_limit
        ?counter
        ()
  | Some source ->
      Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
      let destination : Alpha_context.Destination.t = Contract destination in
      transfer
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        ?confirmations:cctxt#confirmations
        ~dry_run
        ~simulation
        ~force
        ~verbose_signing
        ~fee_parameter
        ~source
        ?fee
        ~src_pk
        ~src_sk
        ~destination
        ?entrypoint
        ?arg
        ~amount
        ?gas_limit
        ?storage_limit
        ?counter
        ~replace_by_fees
        ~successor_level
        ())
  >>= report_michelson_errors
        ~no_print_source
        ~msg:"transfer simulation failed"
        cctxt
  >>= function
  | None -> return_unit
  | Some (_res, _contracts) -> return_unit

let prepare_batch_operation cctxt ?arg ?fee ?gas_limit ?storage_limit
    ?entrypoint source index batch =
  Client_proto_contracts.ContractAlias.find_destination cctxt batch.destination
  >>=? fun (_, destination) ->
  tez_of_string_exn index "amount" batch.amount >>=? fun amount ->
  tez_of_opt_string_exn index "fee" batch.fee >>=? fun batch_fee ->
  let fee = Option.either batch_fee fee in
  let arg = Option.either batch.arg arg in
  let gas_limit = Option.either batch.gas_limit gas_limit in
  let storage_limit = Option.either batch.storage_limit storage_limit in
  let entrypoint = Option.either batch.entrypoint entrypoint in
  parse_arg_transfer arg >>=? fun parameters ->
  (match Contract.is_implicit source with
  | None ->
      Managed_contract.build_transaction_operation
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        ~contract:source
        ~destination
        ?entrypoint
        ?arg
        ~amount
        ?fee
        ?gas_limit
        ?storage_limit
        ()
  | Some _ ->
      return
        (build_transaction_operation
           ~amount
           ~parameters
           ?entrypoint
           ?fee
           ?gas_limit
           ?storage_limit
           (Contract destination)))
  >>=? fun operation ->
  return (Annotated_manager_operation.Annotated_manager_operation operation)

let commands_network network () =
  let open Clic in
  match network with
  | Some `Testnet | None ->
      [
        command
          ~group
          ~desc:"Register and activate an Alphanet/Zeronet faucet account."
          (args2 (Secret_key.force_switch ()) encrypted_switch)
          (prefixes ["activate"; "account"]
          @@ Secret_key.fresh_alias_param
          @@ prefixes ["with"]
          @@ param
               ~name:"activation_key"
               ~desc:
                 "Activate an Alphanet/Zeronet faucet account from the JSON \
                  (file or directly inlined)."
               json_file_or_text_parameter
          @@ stop)
          (fun (force, encrypted) name activation_json cctxt ->
            Secret_key.of_fresh cctxt force name >>=? fun name ->
            match
              Data_encoding.Json.destruct
                Client_proto_context.activation_key_encoding
                activation_json
            with
            | exception (Data_encoding.Json.Cannot_destruct _ as exn) ->
                Format.kasprintf
                  (fun s -> failwith "%s" s)
                  "Invalid activation file: %a %a"
                  (fun ppf -> Data_encoding.Json.print_error ppf)
                  exn
                  Data_encoding.Json.pp
                  activation_json
            | key ->
                activate_account
                  cctxt
                  ~chain:cctxt#chain
                  ~block:cctxt#block
                  ?confirmations:cctxt#confirmations
                  ~encrypted
                  ~force
                  key
                  name
                >>=? fun _res -> return_unit);
      ]
  | Some `Mainnet ->
      [
        command
          ~group
          ~desc:"Activate a fundraiser account."
          (args1 dry_run_switch)
          (prefixes ["activate"; "fundraiser"; "account"]
          @@ Public_key_hash.alias_param
          @@ prefixes ["with"]
          @@ param
               ~name:"code"
               (Clic.parameter (fun _ctx code ->
                    match
                      Blinded_public_key_hash.activation_code_of_hex code
                    with
                    | Some c -> return c
                    | None -> failwith "Hexadecimal parsing failure"))
               ~desc:"Activation code obtained from the Tezos foundation."
          @@ stop)
          (fun dry_run (name, _pkh) code cctxt ->
            activate_existing_account
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              name
              code
            >>=? fun _res -> return_unit);
      ]

let commands_rw () =
  let open Client_proto_programs in
  let open Tezos_micheline in
  let open Clic in
  [
    command
      ~group
      ~desc:"Set the delegate of a contract."
      (args10
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["set"; "delegate"; "for"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ prefix "to"
      @@ Public_key_hash.source_param
           ~name:"dlgt"
           ~desc:"new delegate of the contract"
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap )
           (_, contract)
           delegate
           (cctxt : Protocol_client_context.full) ->
        let fee_parameter =
          {
            Injection.minimal_fees;
            minimal_nanotez_per_byte;
            minimal_nanotez_per_gas_unit;
            force_low_fee;
            fee_cap;
            burn_cap;
          }
        in
        match Contract.is_implicit contract with
        | None ->
            Managed_contract.get_contract_manager cctxt contract
            >>=? fun source ->
            Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            Managed_contract.set_delegate
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              ~verbose_signing
              ~simulation
              ~fee_parameter
              ?fee
              ~source
              ~src_pk
              ~src_sk
              contract
              (Some delegate)
            >>= fun errors ->
            report_michelson_errors
              ~no_print_source:true
              ~msg:"Setting delegate through entrypoints failed."
              cctxt
              errors
            >>= fun _ -> return_unit
        | Some mgr ->
            Client_keys.get_key cctxt mgr >>=? fun (_, src_pk, manager_sk) ->
            set_delegate
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              ~verbose_signing
              ~simulation
              ~fee_parameter
              ?fee
              mgr
              (Some delegate)
              ~src_pk
              ~manager_sk
            >>=? fun _ -> return_unit);
    command
      ~group
      ~desc:"Withdraw the delegate from a contract."
      (args9
         fee_arg
         dry_run_switch
         verbose_signing_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["withdraw"; "delegate"; "from"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap )
           (_, contract)
           (cctxt : Protocol_client_context.full) ->
        let fee_parameter =
          {
            Injection.minimal_fees;
            minimal_nanotez_per_byte;
            minimal_nanotez_per_gas_unit;
            force_low_fee;
            fee_cap;
            burn_cap;
          }
        in
        match Contract.is_implicit contract with
        | None ->
            Managed_contract.get_contract_manager cctxt contract
            >>=? fun source ->
            Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            Managed_contract.set_delegate
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              ~verbose_signing
              ~fee_parameter
              ?fee
              ~source
              ~src_pk
              ~src_sk
              contract
              None
            >>= fun errors ->
            report_michelson_errors
              ~no_print_source:true
              ~msg:"Withdrawing delegate through entrypoints failed."
              cctxt
              errors
            >>= fun _ -> return_unit
        | Some mgr ->
            Client_keys.get_key cctxt mgr >>=? fun (_, src_pk, manager_sk) ->
            set_delegate
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              ~verbose_signing
              ~fee_parameter
              mgr
              None
              ?fee
              ~src_pk
              ~manager_sk
            >>= fun _ -> return_unit);
    command
      ~group
      ~desc:"Launch a smart contract on the blockchain."
      (args15
         fee_arg
         dry_run_switch
         verbose_signing_switch
         gas_limit_arg
         storage_limit_arg
         delegate_arg
         (Client_keys.force_switch ())
         init_arg
         no_print_source_flag
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["originate"; "contract"]
      @@ RawContractAlias.fresh_alias_param
           ~name:"new"
           ~desc:"name of the new contract"
      @@ prefix "transferring"
      @@ tez_param ~name:"qty" ~desc:"amount taken from source"
      @@ prefix "from"
      @@ ContractAlias.destination_param
           ~name:"src"
           ~desc:"name of the source contract"
      @@ prefix "running"
      @@ Program.source_param
           ~name:"prg"
           ~desc:
             "script of the account\n\
              Combine with -init if the storage type is not unit."
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             gas_limit,
             storage_limit,
             delegate,
             force,
             initial_storage,
             no_print_source,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap )
           alias_name
           balance
           (_, source)
           program
           (cctxt : Protocol_client_context.full) ->
        RawContractAlias.of_fresh cctxt force alias_name >>=? fun alias_name ->
        Lwt.return (Micheline_parser.no_parsing_error program)
        >>=? fun {expanded = code; _} ->
        match Contract.is_implicit source with
        | None ->
            failwith
              "only implicit accounts can be the source of an origination"
        | Some source -> (
            Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            let fee_parameter =
              {
                Injection.minimal_fees;
                minimal_nanotez_per_byte;
                minimal_nanotez_per_gas_unit;
                force_low_fee;
                fee_cap;
                burn_cap;
              }
            in
            originate_contract
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              ~verbose_signing
              ?fee
              ?gas_limit
              ?storage_limit
              ~delegate
              ~initial_storage
              ~balance
              ~source
              ~src_pk
              ~src_sk
              ~code
              ~fee_parameter
              ()
            >>= fun errors ->
            report_michelson_errors
              ~no_print_source
              ~msg:"origination simulation failed"
              cctxt
              errors
            >>= function
            | None -> return_unit
            | Some (_res, contract) ->
                if dry_run then return_unit
                else
                  save_contract ~force cctxt alias_name contract >>=? fun () ->
                  return_unit));
    command
      ~group
      ~desc:
        "Execute multiple transfers from a single source account.\n\
         If one of the transfers fails, none of them get executed."
      (args17
         default_fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         default_gas_limit_arg
         default_storage_limit_arg
         counter_arg
         default_arg_arg
         no_print_source_flag
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg
         default_entrypoint_arg
         replace_by_fees_arg)
      (prefixes ["multiple"; "transfers"; "from"]
      @@ ContractAlias.destination_param
           ~name:"src"
           ~desc:"name of the source contract"
      @@ prefix "using"
      @@ param
           ~name:"transfers.json"
           ~desc:
             "List of operations originating from the source contract in JSON \
              format (from a file or directly inlined). The input JSON must be \
              an array of objects of the form: '[ {\"destination\": dst, \
              \"amount\": qty (, <field>: <val> ...) } (, ...) ]', where an \
              optional <field> can either be \"fee\", \"gas-limit\", \
              \"storage-limit\", \"arg\", or \"entrypoint\"."
           json_file_or_text_parameter
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             gas_limit,
             storage_limit,
             counter,
             arg,
             no_print_source,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap,
             entrypoint,
             replace_by_fees )
           (_, source)
           operations_json
           cctxt ->
        let fee_parameter =
          {
            Injection.minimal_fees;
            minimal_nanotez_per_byte;
            minimal_nanotez_per_gas_unit;
            force_low_fee;
            fee_cap;
            burn_cap;
          }
        in
        let prepare i =
          prepare_batch_operation
            cctxt
            ?arg
            ?fee
            ?gas_limit
            ?storage_limit
            ?entrypoint
            source
            i
        in
        match
          Data_encoding.Json.destruct
            (Data_encoding.list
               Client_proto_context.batch_transfer_operation_encoding)
            operations_json
        with
        | [] -> failwith "Empty operation list"
        | operations ->
            (match Contract.is_implicit source with
            | None ->
                Managed_contract.get_contract_manager cctxt source
                >>=? fun source ->
                Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
                return (source, src_pk, src_sk)
            | Some source ->
                Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
                return (source, src_pk, src_sk))
            >>=? fun (source, src_pk, src_sk) ->
            List.mapi_ep prepare operations >>=? fun contents ->
            let (Manager_list contents) =
              Annotated_manager_operation.manager_of_list contents
            in
            Injection.inject_manager_operation
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              ~verbose_signing
              ~simulation
              ~source
              ~fee:(Limit.of_option fee)
              ~gas_limit:(Limit.of_option gas_limit)
              ~storage_limit:(Limit.of_option storage_limit)
              ?counter
              ~src_pk
              ~src_sk
              ~replace_by_fees
              ~fee_parameter
              contents
            >>= report_michelson_errors
                  ~no_print_source
                  ~msg:"multiple transfers simulation failed"
                  cctxt
            >>= fun _ -> return_unit
        | exception (Data_encoding.Json.Cannot_destruct (path, exn2) as exn)
          -> (
            match (path, operations_json) with
            | ([`Index n], `A lj) -> (
                match List.nth_opt lj n with
                | Some j ->
                    failwith
                      "Invalid transfer at index %i: %a %a"
                      n
                      (fun ppf -> Data_encoding.Json.print_error ppf)
                      exn2
                      Data_encoding.Json.pp
                      j
                | _ ->
                    failwith
                      "Invalid transfer at index %i: %a"
                      n
                      (fun ppf -> Data_encoding.Json.print_error ppf)
                      exn2)
            | _ ->
                failwith
                  "Invalid transfer file: %a %a"
                  (fun ppf -> Data_encoding.Json.print_error ppf)
                  exn
                  Data_encoding.Json.pp
                  operations_json));
    command
      ~group
      ~desc:"Transfer tokens / call a smart contract."
      (args19
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         force_switch
         gas_limit_arg
         storage_limit_arg
         counter_arg
         arg_arg
         no_print_source_flag
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg
         entrypoint_arg
         replace_by_fees_arg
         successor_level_arg)
      (prefixes ["transfer"]
      @@ tez_param ~name:"qty" ~desc:"amount taken from source"
      @@ prefix "from"
      @@ ContractAlias.destination_param
           ~name:"src"
           ~desc:"name of the source contract"
      @@ prefix "to"
      @@ ContractAlias.destination_param
           ~name:"dst"
           ~desc:"name/literal of the destination contract"
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             force,
             gas_limit,
             storage_limit,
             counter,
             arg,
             no_print_source,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap,
             entrypoint,
             replace_by_fees,
             successor_level )
           amount
           (_, source)
           (_, destination)
           cctxt ->
        transfer_command
          amount
          source
          destination
          cctxt
          ( fee,
            dry_run,
            verbose_signing,
            simulation,
            force,
            gas_limit,
            storage_limit,
            counter,
            arg,
            no_print_source,
            minimal_fees,
            minimal_nanotez_per_byte,
            minimal_nanotez_per_gas_unit,
            force_low_fee,
            fee_cap,
            burn_cap,
            entrypoint,
            replace_by_fees,
            successor_level ));
    command
      ~group
      ~desc:"Register a global constant"
      (args12
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         storage_limit_arg
         counter_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["register"; "global"; "constant"]
      @@ global_constant_param
           ~name:"expression"
           ~desc:
             "Michelson expression to register. Note the value is not \
              typechecked before registration."
      @@ prefix "from"
      @@ ContractAlias.destination_param
           ~name:"src"
           ~desc:"name of the account registering the global constant"
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             storage_limit,
             counter,
             force_low_fee,
             fee_cap,
             burn_cap )
           global_constant_str
           (_, source)
           cctxt ->
        match Contract.is_implicit source with
        | None ->
            failwith "Only implicit accounts can register global constants"
        | Some source ->
            Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            let fee_parameter =
              {
                Injection.minimal_fees;
                minimal_nanotez_per_byte;
                minimal_nanotez_per_gas_unit;
                force_low_fee;
                fee_cap;
                burn_cap;
              }
            in
            register_global_constant
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ~dry_run
              ~verbose_signing
              ?fee
              ?storage_limit
              ?counter
              ?confirmations:cctxt#confirmations
              ~simulation
              ~source
              ~src_pk
              ~src_sk
              ~fee_parameter
              ~constant:global_constant_str
              ()
            >>= fun errors ->
            report_michelson_errors
              ~no_print_source:false
              ~msg:"register global constant simulation failed"
              cctxt
              errors
            >>= fun _ -> return_unit);
    command
      ~group
      ~desc:"Call a smart contract (same as 'transfer 0')."
      (args19
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         force_switch
         gas_limit_arg
         storage_limit_arg
         counter_arg
         arg_arg
         no_print_source_flag
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg
         entrypoint_arg
         replace_by_fees_arg
         successor_level_arg)
      (prefixes ["call"]
      @@ ContractAlias.destination_param
           ~name:"dst"
           ~desc:"name/literal of the destination contract"
      @@ prefix "from"
      @@ ContractAlias.destination_param
           ~name:"src"
           ~desc:"name of the source contract"
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             force,
             gas_limit,
             storage_limit,
             counter,
             arg,
             no_print_source,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap,
             entrypoint,
             replace_by_fees,
             successor_level )
           (_, destination)
           (_, source)
           cctxt ->
        let amount = Tez.zero in
        transfer_command
          amount
          source
          destination
          cctxt
          ( fee,
            dry_run,
            verbose_signing,
            simulation,
            force,
            gas_limit,
            storage_limit,
            counter,
            arg,
            no_print_source,
            minimal_fees,
            minimal_nanotez_per_byte,
            minimal_nanotez_per_gas_unit,
            force_low_fee,
            fee_cap,
            burn_cap,
            entrypoint,
            replace_by_fees,
            successor_level ));
    command
      ~group
      ~desc:"Reveal the public key of the contract manager."
      (args9
         fee_arg
         dry_run_switch
         verbose_signing_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["reveal"; "key"; "for"]
      @@ ContractAlias.alias_param
           ~name:"src"
           ~desc:"name of the source contract"
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap )
           (_, source)
           cctxt ->
        match Contract.is_implicit source with
        | None -> failwith "only implicit accounts can be revealed"
        | Some source ->
            Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            let fee_parameter =
              {
                Injection.minimal_fees;
                minimal_nanotez_per_byte;
                minimal_nanotez_per_gas_unit;
                force_low_fee;
                fee_cap;
                burn_cap;
              }
            in
            reveal
              cctxt
              ~dry_run
              ~verbose_signing
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~source
              ?fee
              ~src_pk
              ~src_sk
              ~fee_parameter
              ()
            >>=? fun _res -> return_unit);
    command
      ~group
      ~desc:"Register the public key hash as a delegate."
      (args9
         fee_arg
         dry_run_switch
         verbose_signing_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["register"; "key"]
      @@ Public_key_hash.source_param ~name:"mgr" ~desc:"the delegate key"
      @@ prefixes ["as"; "delegate"]
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap )
           src_pkh
           cctxt ->
        Client_keys.get_key cctxt src_pkh >>=? fun (_, src_pk, src_sk) ->
        let fee_parameter =
          {
            Injection.minimal_fees;
            minimal_nanotez_per_byte;
            minimal_nanotez_per_gas_unit;
            force_low_fee;
            fee_cap;
            burn_cap;
          }
        in
        register_as_delegate
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ?confirmations:cctxt#confirmations
          ~dry_run
          ~fee_parameter
          ~verbose_signing
          ?fee
          ~manager_sk:src_sk
          src_pk
        >>= function
        | Ok _ -> return_unit
        | Error [Environment.Ecoproto_error Delegate_storage.Active_delegate] ->
            cctxt#message "Delegate already activated." >>= fun () ->
            return_unit
        | Error el -> Lwt.return_error el);
    command
      ~desc:"Wait until an operation is included in a block"
      (args3
         (default_arg
            ~long:"confirmations"
            ~placeholder:"num_blocks"
            ~doc:
              "wait until 'N' additional blocks after the operation appears in \
               the considered chain"
            ~default:"0"
            non_negative_param)
         (default_arg
            ~long:"check-previous"
            ~placeholder:"num_blocks"
            ~doc:"number of previous blocks to check"
            ~default:"10"
            non_negative_param)
         (arg
            ~long:"branch"
            ~placeholder:"block_hash"
            ~doc:
              "hash of the oldest block where we should look for the operation"
            block_hash_param))
      (prefixes ["wait"; "for"]
      @@ param
           ~name:"operation"
           ~desc:"Operation to be included"
           (parameter (fun _ x ->
                match Operation_hash.of_b58check_opt x with
                | None -> Error_monad.failwith "Invalid operation hash: '%s'" x
                | Some hash -> return hash))
      @@ prefixes ["to"; "be"; "included"]
      @@ stop)
      (fun (confirmations, predecessors, branch)
           operation_hash
           (ctxt : Protocol_client_context.full) ->
        Client_confirmations.wait_for_operation_inclusion
          ctxt
          ~chain:ctxt#chain
          ~confirmations
          ~predecessors
          ?branch
          operation_hash
        >>=? fun _ -> return_unit);
    command
      ~group
      ~desc:"Submit protocol proposals"
      (args3
         dry_run_switch
         verbose_signing_switch
         (switch
            ~doc:
              "Do not fail when the checks that try to prevent the user from \
               shooting themselves in the foot do."
            ~long:"force"
            ()))
      (prefixes ["submit"; "proposals"; "for"]
      @@ ContractAlias.destination_param
           ~name:"delegate"
           ~desc:"the delegate who makes the proposal"
      @@ seq_of_param
           (param
              ~name:"proposal"
              ~desc:"the protocol hash proposal to be submitted"
              (parameter (fun _ x ->
                   match Protocol_hash.of_b58check_opt x with
                   | None ->
                       Error_monad.failwith "Invalid proposal hash: '%s'" x
                   | Some hash -> return hash))))
      (fun (dry_run, verbose_signing, force)
           (_name, source)
           proposals
           (cctxt : Protocol_client_context.full) ->
        match Contract.is_implicit source with
        | None -> failwith "only implicit accounts can submit proposals"
        | Some src_pkh -> (
            Client_keys.get_key cctxt src_pkh
            >>=? fun (src_name, _src_pk, src_sk) ->
            get_period_info
            (* Find period info of the successor, because the operation will
               be injected on the next block at the earliest *)
              ~successor:true
              ~chain:cctxt#chain
              ~block:cctxt#block
              cctxt
            >>=? fun info ->
            (match info.current_period_kind with
            | Proposal -> return_unit
            | _ -> cctxt#error "Not in a proposal period")
            >>=? fun () ->
            Shell_services.Protocol.list cctxt >>=? fun known_protos ->
            get_proposals ~chain:cctxt#chain ~block:cctxt#block cctxt
            >>=? fun known_proposals ->
            Alpha_services.Voting.listings cctxt (cctxt#chain, cctxt#block)
            >>=? fun listings ->
            (* for a proposal to be valid it must either a protocol that was already
               proposed by somebody else or a protocol known by the node, because
               the user is the first proposer and just injected it with
               tezos-admin-client *)
            let check_proposals proposals : bool tzresult Lwt.t =
              let errors = ref [] in
              let error ppf =
                Format.kasprintf (fun s -> errors := s :: !errors) ppf
              in
              if proposals = [] then error "Empty proposal list." ;
              if
                Compare.List_length_with.(
                  proposals > Constants.max_proposals_per_delegate)
              then
                error
                  "Too many proposals: %d > %d."
                  (List.length proposals)
                  Constants.max_proposals_per_delegate ;
              (match
                 Base.List.find_all_dups
                   ~compare:Protocol_hash.compare
                   proposals
               with
              | [] -> ()
              | dups ->
                  error
                    "There %s: %a."
                    (if Compare.List_length_with.(dups = 1) then
                     "is a duplicate proposal"
                    else "are duplicate proposals")
                    Format.(
                      pp_print_list
                        ~pp_sep:(fun ppf () -> pp_print_string ppf ", ")
                        Protocol_hash.pp)
                    dups) ;
              List.iter
                (fun (p : Protocol_hash.t) ->
                  if
                    List.mem ~equal:Protocol_hash.equal p known_protos
                    || Environment.Protocol_hash.Map.mem p known_proposals
                  then ()
                  else
                    error
                      "Protocol %a is not a known proposal."
                      Protocol_hash.pp
                      p)
                proposals ;
              if
                not
                  (List.exists
                     (fun (pkh, _) ->
                       Signature.Public_key_hash.equal pkh src_pkh)
                     listings)
              then
                error
                  "Public-key-hash `%a` from account `%s` does not appear to \
                   have voting rights."
                  Signature.Public_key_hash.pp
                  src_pkh
                  src_name ;
              if !errors <> [] then
                cctxt#message
                  "There %s with the submission:%t"
                  (if Compare.List_length_with.(!errors = 1) then "is an issue"
                  else "are issues")
                  Format.(
                    fun ppf ->
                      pp_print_cut ppf () ;
                      pp_open_vbox ppf 0 ;
                      List.iter
                        (fun msg ->
                          pp_open_hovbox ppf 2 ;
                          pp_print_string ppf "* " ;
                          pp_print_text ppf msg ;
                          pp_close_box ppf () ;
                          pp_print_cut ppf ())
                        !errors ;
                      pp_close_box ppf ())
                >>= fun () -> return_false
              else return_true
            in
            check_proposals proposals >>=? fun all_valid ->
            (if all_valid then cctxt#message "All proposals are valid."
            else if force then
              cctxt#message
                "Some proposals are not valid, but `--force` was used."
            else cctxt#error "Submission failed because of invalid proposals.")
            >>= fun () ->
            submit_proposals
              ~dry_run
              ~verbose_signing
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ~src_sk
              src_pkh
              proposals
            >>= function
            | Ok _res -> return_unit
            | Error errs ->
                (match errs with
                | [
                 Unregistered_error
                   (`O [("kind", `String "generic"); ("error", `String msg)]);
                ] ->
                    cctxt#message
                      "Error:@[<hov>@.%a@]"
                      Format.pp_print_text
                      (String.split_on_char ' ' msg
                      |> List.filter (function "" | "\n" -> false | _ -> true)
                      |> String.concat " "
                      |> String.map (function '\n' | '\t' -> ' ' | c -> c))
                | el -> cctxt#message "Error:@ %a" pp_print_trace el)
                >>= fun () -> failwith "Failed to submit proposals"));
    command
      ~group
      ~desc:"Submit a ballot"
      (args2 verbose_signing_switch dry_run_switch)
      (prefixes ["submit"; "ballot"; "for"]
      @@ ContractAlias.destination_param
           ~name:"delegate"
           ~desc:"the delegate who votes"
      @@ param
           ~name:"proposal"
           ~desc:"the protocol hash proposal to vote for"
           (parameter (fun _ x ->
                match Protocol_hash.of_b58check_opt x with
                | None -> failwith "Invalid proposal hash: '%s'" x
                | Some hash -> return hash))
      @@ param
           ~name:"ballot"
           ~desc:"the ballot value (yea/yay, nay, or pass)"
           (parameter
              ~autocomplete:(fun _ -> return ["yea"; "nay"; "pass"])
              (fun _ s ->
                (* We should have [Vote.of_string]. *)
                match String.lowercase_ascii s with
                | "yay" | "yea" -> return Vote.Yay
                | "nay" -> return Vote.Nay
                | "pass" -> return Vote.Pass
                | s -> failwith "Invalid ballot: '%s'" s))
      @@ stop)
      (fun (verbose_signing, dry_run)
           (_name, source)
           proposal
           ballot
           (cctxt : Protocol_client_context.full) ->
        match Contract.is_implicit source with
        | None -> failwith "only implicit accounts can submit ballot"
        | Some src_pkh ->
            Client_keys.get_key cctxt src_pkh
            >>=? fun (_src_name, _src_pk, src_sk) ->
            get_period_info
            (* Find period info of the successor, because the operation will
               be injected on the next block at the earliest *)
              ~successor:true
              ~chain:cctxt#chain
              ~block:cctxt#block
              cctxt
            >>=? fun info ->
            (match info.current_period_kind with
            | Exploration | Promotion -> return_unit
            | _ -> cctxt#error "Not in Exploration or Promotion period")
            >>=? fun () ->
            submit_ballot
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ~src_sk
              src_pkh
              ~verbose_signing
              ~dry_run
              proposal
              ballot
            >>=? fun _res -> return_unit);
    command
      ~group
      ~desc:"Set the deposits limit of a registered delegate."
      (args10
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["set"; "deposits"; "limit"; "for"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ prefix "to"
      @@ tez_param
           ~name:"deposits limit"
           ~desc:"the maximum amount of frozen deposits"
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap )
           (_, contract)
           limit
           (cctxt : Protocol_client_context.full) ->
        let fee_parameter =
          {
            Injection.minimal_fees;
            minimal_nanotez_per_byte;
            minimal_nanotez_per_gas_unit;
            force_low_fee;
            fee_cap;
            burn_cap;
          }
        in
        match Contract.is_implicit contract with
        | None ->
            cctxt#error
              "Cannot change deposits limit on contract %a. This operation is \
               invalid on originated contracts or unregistered delegate \
               contracts."
              Contract.pp
              contract
        | Some mgr ->
            Client_keys.get_key cctxt mgr >>=? fun (_, src_pk, manager_sk) ->
            set_deposits_limit
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              ~verbose_signing
              ~simulation
              ~fee_parameter
              ?fee
              mgr
              ~src_pk
              ~manager_sk
              (Some limit)
            >>=? fun _ -> return_unit);
    command
      ~group
      ~desc:"Remove the deposits limit of a registered delegate."
      (args10
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["unset"; "deposits"; "limit"; "for"]
      @@ ContractAlias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap )
           (_, contract)
           (cctxt : Protocol_client_context.full) ->
        let fee_parameter =
          {
            Injection.minimal_fees;
            minimal_nanotez_per_byte;
            minimal_nanotez_per_gas_unit;
            force_low_fee;
            fee_cap;
            burn_cap;
          }
        in
        match Contract.is_implicit contract with
        | None ->
            cctxt#error
              "Cannot change deposits limit on contract %a. This operation is \
               invalid on originated contracts or unregistered delegate \
               contracts."
              Contract.pp
              contract
        | Some mgr ->
            Client_keys.get_key cctxt mgr >>=? fun (_, src_pk, manager_sk) ->
            set_deposits_limit
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ?confirmations:cctxt#confirmations
              ~dry_run
              ~verbose_signing
              ~simulation
              ~fee_parameter
              ?fee
              mgr
              ~src_pk
              ~manager_sk
              None
            >>=? fun _ -> return_unit);
    command
      ~group
      ~desc:"Launch a new optimistic transaction rollup."
      (args12
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         storage_limit_arg
         counter_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["originate"; "tx"; "rollup"]
      @@ prefix "from"
      @@ ContractAlias.destination_param
           ~name:"src"
           ~desc:"name of the account originating the transaction rollup"
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             storage_limit,
             counter,
             force_low_fee,
             fee_cap,
             burn_cap )
           (_, source)
           cctxt ->
        match Contract.is_implicit source with
        | None ->
            failwith "Only implicit accounts can originate transaction rollups"
        | Some source ->
            Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            let fee_parameter =
              {
                Injection.minimal_fees;
                minimal_nanotez_per_byte;
                minimal_nanotez_per_gas_unit;
                force_low_fee;
                fee_cap;
                burn_cap;
              }
            in
            originate_tx_rollup
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ~dry_run
              ~verbose_signing
              ?fee
              ?storage_limit
              ?counter
              ?confirmations:cctxt#confirmations
              ~simulation
              ~source
              ~src_pk
              ~src_sk
              ~fee_parameter
              ()
            >>=? fun _res -> return_unit);
    command
      ~group
      ~desc:"Submit a batch of optimistic transaction rollup operations."
      (args12
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         storage_limit_arg
         counter_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["submit"; "tx"; "rollup"; "batch"]
      @@ Clic.param
           ~name:"bytes"
           ~desc:"a bytes representation of the batch in hexadecimal form."
           Client_proto_args.string_parameter
      @@ prefix "to" @@ tx_rollup_param @@ prefix "from"
      @@ ContractAlias.destination_param
           ~name:"src"
           ~desc:"name of the account originating the transaction rollup."
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             storage_limit,
             counter,
             force_low_fee,
             fee_cap,
             burn_cap )
           content
           tx_rollup
           (_, source)
           cctxt ->
        match Contract.is_implicit source with
        | None ->
            failwith
              "Only implicit accounts can submit transaction rollup batches"
        | Some source ->
            Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            let fee_parameter =
              {
                Injection.minimal_fees;
                minimal_nanotez_per_byte;
                minimal_nanotez_per_gas_unit;
                force_low_fee;
                fee_cap;
                burn_cap;
              }
            in
            submit_tx_rollup_batch
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ~dry_run
              ~verbose_signing
              ?fee
              ?storage_limit
              ?counter
              ?confirmations:cctxt#confirmations
              ~simulation
              ~source
              ~src_pk
              ~src_sk
              ~fee_parameter
              ~tx_rollup
              ~content
              ()
            >>=? fun _res -> return_unit);
    command
      ~group
      ~desc:"Submit an optimistic transaction rollup commitment operation."
      (args12
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         storage_limit_arg
         counter_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["submit"; "tx"; "rollup"; "commitment"]
      @@ Clic.param
           ~name:"level"
           ~desc:"The level"
           Client_proto_args.int_parameter
      @@ Clic.param
           ~name:"inbox_merkle_root"
           ~desc:"the inbox merkle root to commit to in b58check notation."
           Client_proto_args.string_parameter
      @@ Clic.param
           ~name:"predecessor"
           ~desc:
             "a bytes representation of the predecessor commitment in \
              hexadecimal form, or empty for no predecessor."
           Client_proto_args.string_parameter
      @@ Clic.param
           ~name:"batches"
           ~desc:
             "a bytes representation of the commitment roots in hex and, \
              separated by !."
           Client_proto_args.string_parameter
      @@ prefix "to" @@ tx_rollup_param @@ prefix "from"
      @@ ContractAlias.destination_param
           ~name:"src"
           ~desc:"name of the account submitting the commitment."
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             storage_limit,
             counter,
             force_low_fee,
             fee_cap,
             burn_cap )
           level
           inbox_merkle_root
           predecessor
           batches
           tx_rollup
           (_, source)
           cctxt ->
        let level = Int32.of_int level in
        match Contract.is_implicit source with
        | None ->
            failwith
              "Only implicit accounts can submit transaction rollup commitments"
        | Some source ->
            Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            let fee_parameter =
              {
                Injection.minimal_fees;
                minimal_nanotez_per_byte;
                minimal_nanotez_per_gas_unit;
                force_low_fee;
                fee_cap;
                burn_cap;
              }
            in
            let predecessor =
              if String.equal predecessor "" then None else Some predecessor
            in
            submit_tx_rollup_commitment
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ~dry_run
              ~verbose_signing
              ?fee
              ?storage_limit
              ?counter
              ?confirmations:cctxt#confirmations
              ~simulation
              ~source
              ~src_pk
              ~src_sk
              ~fee_parameter
              ~tx_rollup
              ~level
              ~inbox_merkle_root
              ~batches
              ~predecessor
              ()
            >>=? fun _res -> return_unit);
    command
      ~group
      ~desc:
        "Submit an optimistic transaction rollup finalise commitment operation."
      (args12
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         storage_limit_arg
         counter_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["submit"; "tx"; "rollup"; "finalize"; "commitment"]
      @@ prefix "to" @@ tx_rollup_param @@ prefix "from"
      @@ ContractAlias.destination_param
           ~name:"src"
           ~desc:"name of the account finalizing the commitment."
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             storage_limit,
             counter,
             force_low_fee,
             fee_cap,
             burn_cap )
           tx_rollup
           (_, source)
           cctxt ->
        match Contract.is_implicit source with
        | None -> failwith "Only implicit accounts can finalize commitments"
        | Some source ->
            Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            let fee_parameter =
              {
                Injection.minimal_fees;
                minimal_nanotez_per_byte;
                minimal_nanotez_per_gas_unit;
                force_low_fee;
                fee_cap;
                burn_cap;
              }
            in
            submit_tx_rollup_finalize_commitment
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ~dry_run
              ~verbose_signing
              ?fee
              ?storage_limit
              ?counter
              ?confirmations:cctxt#confirmations
              ~simulation
              ~source
              ~src_pk
              ~src_sk
              ~fee_parameter
              ~tx_rollup
              ()
            >>=? fun _res -> return_unit);
    command
      ~group
      ~desc:"Submit an optimistic transaction rollup operation to recover bond."
      (args12
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         storage_limit_arg
         counter_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["submit"; "tx"; "rollup"; "return"; "bond"]
      @@ prefix "to" @@ tx_rollup_param @@ prefix "from"
      @@ ContractAlias.destination_param
           ~name:"src"
           ~desc:"account that owns the bond."
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             storage_limit,
             counter,
             force_low_fee,
             fee_cap,
             burn_cap )
           tx_rollup
           (_, source)
           cctxt ->
        match Contract.is_implicit source with
        | None -> failwith "Only implicit accounts can deposit/recover bonds"
        | Some source ->
            Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            let fee_parameter =
              {
                Injection.minimal_fees;
                minimal_nanotez_per_byte;
                minimal_nanotez_per_gas_unit;
                force_low_fee;
                fee_cap;
                burn_cap;
              }
            in
            submit_tx_rollup_return_bond
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ~dry_run
              ~verbose_signing
              ?fee
              ?storage_limit
              ?counter
              ?confirmations:cctxt#confirmations
              ~simulation
              ~source
              ~src_pk
              ~src_sk
              ~fee_parameter
              ~tx_rollup
              ()
            >>=? fun _res -> return_unit);
    command
      ~group
      ~desc:
        "Submit an optimistic transaction rollup remove commitment operation."
      (args12
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         storage_limit_arg
         counter_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["submit"; "tx"; "rollup"; "remove"; "commitment"]
      @@ prefix "to" @@ tx_rollup_param @@ prefix "from"
      @@ ContractAlias.destination_param
           ~name:"src"
           ~desc:"name of the account removing the commitment."
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             storage_limit,
             counter,
             force_low_fee,
             fee_cap,
             burn_cap )
           tx_rollup
           (_, source)
           cctxt ->
        match Contract.is_implicit source with
        | None ->
            failwith
              "Only implicit accounts can remove transaction rollup commitments"
        | Some source ->
            Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            let fee_parameter =
              {
                Injection.minimal_fees;
                minimal_nanotez_per_byte;
                minimal_nanotez_per_gas_unit;
                force_low_fee;
                fee_cap;
                burn_cap;
              }
            in
            submit_tx_rollup_remove_commitment
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ~dry_run
              ~verbose_signing
              ?fee
              ?storage_limit
              ?counter
              ?confirmations:cctxt#confirmations
              ~simulation
              ~source
              ~src_pk
              ~src_sk
              ~fee_parameter
              ~tx_rollup
              ()
            >>=? fun _res -> return_unit);
    command
      ~group
      ~desc:"Submit an optimistic transaction rollup rejection operation."
      (args12
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         storage_limit_arg
         counter_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes
         ["submit"; "tx"; "rollup"; "reject"; "commitment"; "at"; "level"]
      @@ Clic.param
           ~name:"level"
           ~desc:"The level"
           Client_proto_args.int_parameter
      @@ prefix "message"
      @@ Clic.param
           ~name:"message"
           ~desc:"the message being rejected"
           Client_proto_args.string_parameter
      @@ prefix "at" @@ prefix "position"
      @@ Clic.param
           ~name:"message_position"
           ~desc:"position of the message being rejected in the inbox"
           int_parameter
      @@ prefix "and" @@ prefix "path"
      @@ Clic.param
           ~name:"message_path"
           ~desc:"merkle path of the message being rejected in the inbox"
           string_parameter
      @@ prefix "with" @@ prefix "proof" @@ tx_rollup_proof_param
      @@ prefixes ["with"; "agreed"; "context"; "hash"]
      @@ Clic.param
           ~name:"context_hash"
           ~desc:"the context hash of the layer 2"
           Client_proto_args.string_parameter
      @@ prefixes ["and"; "withdraw"; "list"]
      @@ Clic.param
           ~name:"withdrawals_merkle_root"
           ~desc:"the hash of the merkelised withdraw list"
           Client_proto_args.string_parameter
      @@ prefix "to" @@ tx_rollup_param @@ prefix "from"
      @@ ContractAlias.destination_param
           ~name:"src"
           ~desc:"name of the account rejecting the commitment."
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             storage_limit,
             counter,
             force_low_fee,
             fee_cap,
             burn_cap )
           level
           message
           message_position
           message_path
           proof
           context_hash
           withdrawals_merkle_root
           tx_rollup
           (_, source)
           cctxt ->
        let level = Int32.of_int level in
        match Contract.is_implicit source with
        | None ->
            failwith
              "Only implicit accounts can reject transaction rollup commitments"
        | Some source ->
            Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            let fee_parameter =
              {
                Injection.minimal_fees;
                minimal_nanotez_per_byte;
                minimal_nanotez_per_gas_unit;
                force_low_fee;
                fee_cap;
                burn_cap;
              }
            in
            submit_tx_rollup_rejection
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ~dry_run
              ~verbose_signing
              ?fee
              ?storage_limit
              ?counter
              ?confirmations:cctxt#confirmations
              ~simulation
              ~source
              ~src_pk
              ~src_sk
              ~fee_parameter
              ~tx_rollup
              ~level
              ~message
              ~message_position
              ~message_path
              ~proof
              ~context_hash
              ~withdrawals_merkle_root
              ()
            >>=? fun _res -> return_unit);
    command
      ~group
      ~desc:"Originate a new smart-contract rollup."
      (args12
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         storage_limit_arg
         counter_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["originate"; "sc"; "rollup"; "from"]
      @@ ContractAlias.destination_param
           ~name:"src"
           ~desc:"name of the account originating the smart-contract rollup"
      @@ prefixes ["of"; "kind"]
      @@ param
           ~name:"sc_rollup_kind"
           ~desc:"kind of the smart-contract rollup to be originated"
           rollup_kind_param
      @@ prefixes ["booting"; "with"]
      @@ param
           ~name:"boot_sector"
           ~desc:"the initialization state for the smart-contract rollup"
           boot_sector_param
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             storage_limit,
             counter,
             force_low_fee,
             fee_cap,
             burn_cap )
           (_, source)
           pvm
           boot_sector
           cctxt ->
        match Contract.is_implicit source with
        | None ->
            failwith
              "Only implicit accounts can originate smart-contract rollups"
        | Some source ->
            Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
            let fee_parameter =
              {
                Injection.minimal_fees;
                minimal_nanotez_per_byte;
                minimal_nanotez_per_gas_unit;
                force_low_fee;
                fee_cap;
                burn_cap;
              }
            in
            let (module R : Sc_rollups.PVM.S) = pvm in
            boot_sector pvm >>=? fun boot_sector ->
            sc_rollup_originate
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              ~dry_run
              ~verbose_signing
              ?fee
              ?storage_limit
              ?counter
              ?confirmations:cctxt#confirmations
              ~simulation
              ~source
              ~src_pk
              ~src_sk
              ~fee_parameter
              ~kind:(Sc_rollups.kind_of pvm)
              ~boot_sector
              ()
            >>=? fun _res -> return_unit);
    command
      ~group
      ~desc:"Send one or more messages to a smart-contract rollup."
      (args12
         fee_arg
         dry_run_switch
         verbose_signing_switch
         simulate_switch
         minimal_fees_arg
         minimal_nanotez_per_byte_arg
         minimal_nanotez_per_gas_unit_arg
         storage_limit_arg
         counter_arg
         force_low_fee_arg
         fee_cap_arg
         burn_cap_arg)
      (prefixes ["send"; "sc"; "rollup"; "message"]
      @@ param
           ~name:"messages"
           ~desc:
             "the message(s) to be sent to the rollup (syntax: \
              bin:<path_to_binary_file>|text:<json list of hex \
              messages>|file:<json_file>)"
           messages_param
      @@ prefixes ["from"]
      @@ ContractAlias.destination_param
           ~name:"src"
           ~desc:"name of the source contract"
      @@ prefixes ["to"]
      @@ param
           ~name:"dst"
           ~desc:"address of the destination rollup"
           rollup_address_param
      @@ stop)
      (fun ( fee,
             dry_run,
             verbose_signing,
             simulation,
             minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             storage_limit,
             counter,
             force_low_fee,
             fee_cap,
             burn_cap )
           messages
           (_, source)
           rollup
           cctxt ->
        (match Contract.is_implicit source with
        | None -> failwith "Only implicit accounts can send messages to rollups"
        | Some source -> return source)
        >>=? fun source ->
        (match messages with
        | `Bin message -> return [message]
        | `Json messages -> (
            match Data_encoding.(Json.destruct (list bytes) messages) with
            | exception _ ->
                failwith
                  "Could not read list of messages (expected list of bytes)"
            | messages -> return (List.map Bytes.to_string messages)))
        >>=? fun messages ->
        Client_keys.get_key cctxt source >>=? fun (_, src_pk, src_sk) ->
        let fee_parameter =
          {
            Injection.minimal_fees;
            minimal_nanotez_per_byte;
            minimal_nanotez_per_gas_unit;
            force_low_fee;
            fee_cap;
            burn_cap;
          }
        in
        sc_rollup_add_messages
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ?dry_run:(Some dry_run)
          ?verbose_signing:(Some verbose_signing)
          ?fee
          ?storage_limit
          ?counter
          ?confirmations:cctxt#confirmations
          ~simulation
          ~source
          ~rollup
          ~messages
          ~src_pk
          ~src_sk
          ~fee_parameter
          ()
        >>=? fun _res -> return_unit);
    command
      ~group
      ~desc:"List originated smart-contract rollups."
      no_options
      (prefixes ["list"; "sc"; "rollups"] @@ stop)
      (fun () (cctxt : Protocol_client_context.full) ->
        Plugin.RPC.Sc_rollup.list cctxt (cctxt#chain, cctxt#block)
        >>=? fun rollups ->
        List.iter_s
          (fun addr -> cctxt#message "%s" (Sc_rollup.Address.to_b58check addr))
          rollups
        >>= fun () -> return_unit);
  ]

let commands network () =
  commands_rw () @ commands_network network () @ commands_ro ()
