(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
open Tezos_micheline
open Client_proto_context
open Client_proto_contracts
open Client_keys_v0

let report_michelson_errors ?(no_print_source = false) ~msg
    (cctxt : #Client_context.printer) = function
  | Error errs ->
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

let data_parameter =
  Tezos_clic.parameter (fun _ data ->
      Lwt.return
        (Micheline_parser.no_parsing_error
        @@ Michelson_v1_parser.parse_expression data))

let non_negative_param =
  Tezos_clic.parameter (fun _ s ->
      match int_of_string_opt s with
      | Some i when i >= 0 -> return i
      | _ -> failwith "Parameter should be a non-negative integer literal")

let group =
  {
    Tezos_clic.name = "context";
    title = "Block contextual commands (see option -block)";
  }

let binary_description =
  {Tezos_clic.name = "description"; title = "Binary Description"}

let commands () =
  let open Tezos_clic in
  [
    command
      ~group
      ~desc:"Access the timestamp of the block."
      (args1
         (switch ~doc:"output time in seconds" ~short:'s' ~long:"seconds" ()))
      (fixed ["get"; "timestamp"])
      (fun seconds (cctxt : Alpha_client_context.full) ->
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
      (fun () (cctxt : Alpha_client_context.full) ->
        list_contract_labels cctxt ~chain:cctxt#chain ~block:cctxt#block
        >>=? fun contracts ->
        List.iter_s
          (fun (alias, hash, kind) -> cctxt#message "%s%s%s" hash kind alias)
          contracts
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get the balance of a contract."
      no_options
      (prefixes ["get"; "balance"; "for"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Alpha_client_context.full) ->
        get_balance cctxt ~chain:cctxt#chain ~block:cctxt#block contract
        >>=? fun amount ->
        cctxt#answer "%a %s" Tez.pp amount Client_proto_args.tez_sym
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get the storage of a contract."
      no_options
      (prefixes ["get"; "script"; "storage"; "for"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Alpha_client_context.full) ->
        get_storage cctxt ~chain:cctxt#chain ~block:cctxt#block contract
        >>=? function
        | None -> cctxt#error "This is not a smart contract."
        | Some storage ->
            cctxt#answer "%a" Michelson_v1_printer.print_expr_unwrapped storage
            >>= fun () -> return_unit);
    command
      ~group
      ~desc:
        "Get the value associated to a key in the big map storage of a \
         contract."
      no_options
      (prefixes ["get"; "big"; "map"; "value"; "for"]
      @@ Tezos_clic.param ~name:"key" ~desc:"the key to look for" data_parameter
      @@ prefixes ["of"; "type"]
      @@ Tezos_clic.param ~name:"type" ~desc:"type of the key" data_parameter
      @@ prefix "in"
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () key key_type (_, contract) (cctxt : Alpha_client_context.full) ->
        get_big_map_value
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
      ~desc:"Get the storage of a contract."
      no_options
      (prefixes ["get"; "script"; "code"; "for"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Alpha_client_context.full) ->
        get_script cctxt ~chain:cctxt#chain ~block:cctxt#block contract
        >>=? function
        | None -> cctxt#error "This is not a smart contract."
        | Some {code; storage = _} -> (
            match Script_repr.force_decode code with
            | Error errs ->
                cctxt#error
                  "%a"
                  (Format.pp_print_list
                     ~pp_sep:Format.pp_print_newline
                     Environment.Error_monad.pp)
                  errs
            | Ok (code, _) ->
                let {Michelson_v1_parser.source; _} =
                  Michelson_v1_printer.unparse_toplevel code
                in
                cctxt#answer "%s" source >>= return));
    command
      ~group
      ~desc:"Get the manager of a contract."
      no_options
      (prefixes ["get"; "manager"; "for"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Alpha_client_context.full) ->
        Client_proto_contracts.get_manager
          cctxt
          ~chain:cctxt#chain
          ~block:cctxt#block
          contract
        >>=? fun manager ->
        Public_key_hash.rev_find cctxt manager >>=? fun mn ->
        Public_key_hash.to_source manager >>=? fun m ->
        cctxt#message
          "%s (%s)"
          m
          (match mn with None -> "unknown" | Some n -> "known as " ^ n)
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Get the delegate of a contract."
      no_options
      (prefixes ["get"; "delegate"; "for"]
      @@ Contract_alias.destination_param ~name:"src" ~desc:"source contract"
      @@ stop)
      (fun () (_, contract) (cctxt : Alpha_client_context.full) ->
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
      (fun predecessors operation_hash (ctxt : Alpha_client_context.full) ->
        display_receipt_for_operation
          ctxt
          ~chain:ctxt#chain
          ~predecessors
          operation_hash
        >>=? fun _ -> return_unit);
    command
      ~group:binary_description
      ~desc:"Describe unsigned block header"
      no_options
      (fixed ["describe"; "unsigned"; "block"; "header"])
      (fun () (cctxt : Alpha_client_context.full) ->
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
      (fun () (cctxt : Alpha_client_context.full) ->
        cctxt#message
          "%a"
          Data_encoding.Binary_schema.pp
          (Data_encoding.Binary.describe
             Alpha_context.Operation.unsigned_encoding)
        >>= fun () -> return_unit);
    command
      ~group
      ~desc:"Summarize the current voting period"
      no_options
      (fixed ["show"; "voting"; "period"])
      (fun () (cctxt : Alpha_client_context.full) ->
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
          |> List.sort (fun (_, v1) (_, v2) -> Int32.(compare v2 v1))
        in
        let print_proposal = function
          | None -> assert false (* not called during proposal phase *)
          | Some proposal ->
              cctxt#message "Current proposal: %a" Protocol_hash.pp proposal
        in
        match info.current_period_kind with
        | Proposal ->
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
                        "* %a %ld (%sknown by the node)@."
                        Protocol_hash.pp
                        p
                        w
                        (if List.mem ~equal:Protocol_hash.equal p known_protos
                        then ""
                        else "not "))
                    ranks ;
                  pp_close_box ppf ())
            >>= fun () -> return_unit
        | Testing_vote | Promotion_vote ->
            print_proposal info.current_proposal >>= fun () ->
            get_ballots_info ~chain:cctxt#chain ~block:cctxt#block cctxt
            >>=? fun ballots_info ->
            cctxt#answer
              "Ballots: %a@,\
               Current participation %.2f%%, necessary quorum %.2f%%@,\
               Current in favor %ld, needed supermajority %ld"
              Data_encoding.Json.pp
              (Data_encoding.Json.construct
                 Vote.ballots_encoding
                 ballots_info.ballots)
              (Int32.to_float ballots_info.participation /. 100.)
              (Int32.to_float ballots_info.current_quorum /. 100.)
              ballots_info.ballots.yay
              ballots_info.supermajority
            >>= fun () -> return_unit
        | Testing ->
            print_proposal info.current_proposal >>= fun () -> return_unit);
  ]
