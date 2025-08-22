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
open Client_proto_rollups
open Client_keys_v0
open Client_proto_args

let save_tx_rollup ~force (cctxt : #Client_context.full) alias_name rollup
    ~origination_level =
  TxRollupAlias.add ~force cctxt alias_name {rollup; origination_level}
  >>=? fun () ->
  cctxt#message "Transaction rollup memorized as %s" alias_name >>= fun () ->
  return_unit

let dry_run_switch : (_, Protocol_client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"dry-run"
    ~short:'D'
    ~doc:"don't inject the operation, just display it"
    ()

let verbose_signing_switch : (_, Protocol_client_context.full) Tezos_clic.arg =
  Tezos_clic.switch
    ~long:"verbose-signing"
    ~doc:"display extra information before signing the operation"
    ()

let normalize_types_switch =
  Tezos_clic.switch
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

let group =
  {
    Tezos_clic.name = "context";
    title = "Block contextual commands (see option -block)";
  }

let alphanet = {Tezos_clic.name = "alphanet"; title = "Alphanet only commands"}

let binary_description =
  {Tezos_clic.name = "description"; title = "Binary Description"}

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
  let open Tezos_clic in
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
          (fun (key, size) -> cctxt#message "%a %d" Contract_hash.pp key size)
          keys
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get the key rank of a cache key."
      no_options
      (prefixes ["get"; "cached"; "contract"; "rank"; "for"]
      @@ Originated_contract_alias.destination_param
           ~name:"src"
           ~desc:"contract"
      @@ stop)
      (fun () contract (cctxt : Protocol_client_context.full) ->
        contract_rank cctxt ~chain:cctxt#chain ~block:cctxt#block contract
        >>=? fun rank ->
        match rank with
        | None ->
            cctxt#error "Invalid contract: %a" Contract_hash.pp contract
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
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () contract (cctxt : Protocol_client_context.full) ->
        get_balance cctxt ~chain:cctxt#chain ~block:cctxt#block contract
        >>=? fun amount ->
        cctxt#answer "%a %s" Tez.pp amount Operation_result.tez_sym
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get the storage of a contract."
      (args1 (unparsing_mode_arg ~default:"Readable"))
      (prefixes ["get"; "contract"; "storage"; "for"]
      @@ Originated_contract_alias.destination_param
           ~name:"src"
           ~desc:"source contract"
      @@ stop)
      (fun unparsing_mode contract (cctxt : Protocol_client_context.full) ->
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
      @@ Tezos_clic.param ~name:"key" ~desc:"the key to look for" data_parameter
      @@ prefixes ["of"; "type"]
      @@ Tezos_clic.param ~name:"type" ~desc:"type of the key" data_parameter
      @@ prefix "in"
      @@ Originated_contract_alias.destination_param
           ~name:"src"
           ~desc:"source contract"
      @@ stop)
      (fun () key key_type contract (cctxt : Protocol_client_context.full) ->
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
      @@ Tezos_clic.param
           ~name:"key"
           ~desc:"the key to look for"
           (Tezos_clic.parameter (fun _ s ->
                return (Script_expr_hash.of_b58check_exn s)))
      @@ prefixes ["of"; "big"; "map"]
      @@ Tezos_clic.param
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
      (args2 (unparsing_mode_arg ~default:"Readable") normalize_types_switch)
      (prefixes ["get"; "contract"; "code"; "for"]
      @@ Originated_contract_alias.destination_param
           ~name:"src"
           ~desc:"source contract"
      @@ stop)
      (fun (unparsing_mode, normalize_types)
           contract
           (cctxt : Protocol_client_context.full)
         ->
        get_script
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          ~unparsing_mode
          ~normalize_types
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
      @@ Originated_contract_alias.destination_param
           ~name:"src"
           ~desc:"source contract"
      @@ stop)
      (fun () contract (cctxt : Protocol_client_context.full) ->
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
      @@ Tezos_clic.param
           ~name:"entrypoint"
           ~desc:"the entrypoint to describe"
           entrypoint_parameter
      @@ prefixes ["for"]
      @@ Originated_contract_alias.destination_param
           ~name:"src"
           ~desc:"source contract"
      @@ stop)
      (fun normalize_types
           entrypoint
           contract
           (cctxt : Protocol_client_context.full)
         ->
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
      @@ Originated_contract_alias.destination_param
           ~name:"src"
           ~desc:"source contract"
      @@ stop)
      (fun normalize_types contract (cctxt : Protocol_client_context.full) ->
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
      @@ Originated_contract_alias.destination_param
           ~name:"src"
           ~desc:"source contract"
      @@ stop)
      (fun () contract (cctxt : Protocol_client_context.full) ->
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
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () contract (cctxt : Protocol_client_context.full) ->
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
            non_negative_parameter))
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
                          Operation_result.tez_sym
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
                Operation_result.tez_sym
                Tez.pp
                (Tez.of_mutez_exn ballots_info.ballots.nay)
                Operation_result.tez_sym
                Tez.pp
                (Tez.of_mutez_exn ballots_info.ballots.pass)
                Operation_result.tez_sym
                (Int32.to_float ballots_info.participation /. 100.)
                (Int32.to_float ballots_info.current_quorum /. 100.)
                Tez.pp
                (Tez.of_mutez_exn ballots_info.ballots.yay)
                Operation_result.tez_sym
                Tez.pp
                (Tez.of_mutez_exn ballots_info.supermajority)
                Operation_result.tez_sym
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
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source delegate"
      @@ stop)
      (fun () contract (cctxt : Protocol_client_context.full) ->
        match contract with
        | Originated _ ->
            cctxt#error
              "Cannot change deposits limit on contract %a. This operation is \
               invalid on originated contracts."
              Contract.pp
              contract
        | Implicit delegate -> (
            get_frozen_deposits_limit
              cctxt
              ~chain:cctxt#chain
              ~block:cctxt#block
              delegate
            >>=? function
            | None -> cctxt#answer "unlimited" >>= return
            | Some limit ->
                cctxt#answer "%a %s" Tez.pp limit Operation_result.tez_sym
                >>= return));
  ]

let commands _network () = commands_ro ()
