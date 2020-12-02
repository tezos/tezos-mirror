(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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
open Client_proto_contracts
open Client_proto_context
open Tezos_micheline

let singlesig_group =
  {
    Clic.name = "single-sig baker";
    title = "Commands for managing a single-signature baker smart contract";
  }

let multisig_group =
  {
    Clic.name = "multi-sig baker";
    title = "Commands for managing a multi-signature baker smart contract";
  }

let set_consensus_key_desc =
  "Similarly to fresh baker registration, the new consensus key change will \
   be set pending and active at the end of cycle preserved_cycles + 2 = 7 \
   after the current cycle. Note that if there already is a pending consensus \
   key change, it will be overridden and the activation cycle will reset."

let transfer_options =
  Clic.args14
    Client_proto_args.fee_arg
    Client_proto_context_commands.dry_run_switch
    Client_proto_args.gas_limit_arg
    Client_proto_args.storage_limit_arg
    Client_proto_args.counter_arg
    Client_proto_args.no_print_source_flag
    Client_proto_args.minimal_fees_arg
    Client_proto_args.minimal_nanotez_per_byte_arg
    Client_proto_args.minimal_nanotez_per_gas_unit_arg
    Client_proto_args.force_low_fee_arg
    Client_proto_args.fee_cap_arg
    Client_proto_args.burn_cap_arg
    Client_proto_args.arg_arg
    Client_proto_args.entrypoint_arg

let generic_options =
  Clic.args12
    Client_proto_args.fee_arg
    Client_proto_context_commands.dry_run_switch
    Client_proto_args.gas_limit_arg
    Client_proto_args.storage_limit_arg
    Client_proto_args.counter_arg
    Client_proto_args.no_print_source_flag
    Client_proto_args.minimal_fees_arg
    Client_proto_args.minimal_nanotez_per_byte_arg
    Client_proto_args.minimal_nanotez_per_gas_unit_arg
    Client_proto_args.force_low_fee_arg
    Client_proto_args.fee_cap_arg
    Client_proto_args.burn_cap_arg

let bytes_only_switch =
  Clic.switch
    ~long:"bytes-only"
    ~doc:"return only the byte sequence to be signed"
    ()

let threshold_param () =
  Clic.param
    ~name:"threshold"
    ~desc:"Number of required signatures"
    Client_proto_args.int_parameter

let public_key_param () =
  Client_keys.Public_key.source_param
    ~name:"key"
    ~desc:"Each signer of the baker contract"

let implicit_source_param () =
  Client_proto_contracts.Contract_alias.destination_param
    ~name:"src"
    ~desc:"source calling the baker contract"

let owner_key_param () =
  Client_keys.Secret_key.alias_param
    ~name:"owner_key"
    ~desc:"the owner key of the baker contract"

let proposal_param () =
  Clic.(
    param
      ~name:"proposal"
      ~desc:"the protocol hash proposal to be submitted"
      (parameter (fun _ x ->
           match Protocol_hash.of_b58check_opt x with
           | None ->
               Error_monad.failwith "Invalid proposal hash: '%s'" x
           | Some hash ->
               return hash)))

let proposals_param () = Clic.seq_of_param @@ proposal_param ()

let active_param () =
  Clic.(
    param
      ~name:"active/inactive"
      ~desc:"active or inactive"
      (parameter
         ~autocomplete:(fun _ -> return ["active"; "inactive"])
         (fun _ s ->
           match String.lowercase_ascii s with
           | "active" ->
               return_true
           | "inactive" ->
               return_false
           | _ ->
               failwith "Invalid active value '%s'" s)))

let accept_param () =
  Clic.(
    param
      ~name:"accepting/declining"
      ~desc:"accepting or declining"
      (parameter
         ~autocomplete:(fun _ -> return ["accepting"; "declining"])
         (fun _ s ->
           match String.lowercase_ascii s with
           | "accepting" ->
               return_true
           | "declining" ->
               return_false
           | _ ->
               failwith "Invalid accept value '%s'" s)))

let consensus_key_param () =
  Client_keys.Public_key.source_param
    ~name:"key"
    ~desc:"the new consensus key of the baker contract"

let signature_param () =
  Clic.param
    ~name:"signature"
    ~desc:"Each signer of the multisig contract"
    Client_proto_args.signature_parameter

let pvss_key_param () =
  Client_keys.PVSS_public_key.source_param
    ~name:"key"
    ~desc:"the new PVSS key of the baker contract"

let parse_expr expr =
  Lwt.return @@ Micheline_parser.no_parsing_error
  @@ Michelson_v1_parser.parse_expression expr

let data_parameter = Clic.parameter (fun _ data -> parse_expr data)

let generic_lambda_param () =
  let desc =
    Format.asprintf
      "lambda expression of type \"%s\""
      Client_proto_baker.generic_lambda_type
  in
  Clic.param ~name:"lambda" ~desc data_parameter

let ballot_param () =
  Clic.(
    param
      ~name:"ballot"
      ~desc:"the ballot value (yea/yay, nay, or pass)"
      (parameter
         ~autocomplete:(fun _ -> return ["yea"; "nay"; "pass"])
         (fun _ s ->
           (* We should have [Vote.of_string]. *)
           match String.lowercase_ascii s with
           | "yay" | "yea" ->
               return Vote.Yay
           | "nay" ->
               return Vote.Nay
           | "pass" ->
               return Vote.Pass
           | s ->
               failwith "Invalid ballot: '%s'" s)))

let singlesig_commands () : #Protocol_client_context.full Clic.command list =
  Clic.
    [ command
        ~group:singlesig_group
        ~desc:"Transfer tokens using a single-signature baker contract"
        transfer_options
        ( prefixes ["from"; "baker"; "contract"]
        @@ Baker_alias.source_param @@ prefix "transfer"
        @@ Client_proto_args.tez_param
             ~name:"qty"
             ~desc:"amount taken from the baker contract"
        @@ prefix "to"
        @@ Client_proto_contracts.Contract_alias.destination_param
             ~name:"dst"
             ~desc:"name/literal of the destination contract"
        @@ stop )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap,
               arg,
               entrypoint )
             baker
             amount
             (_, destination)
             (cctxt : #Protocol_client_context.full) ->
          get_baker_consensus_key cctxt ~chain:cctxt#chain baker
          >>=? fun (_alias, source, src_pk, src_sk) ->
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
          Client_proto_baker.prepare_transfer_action
            ?arg
            ?entrypoint
            cctxt
            ~amount
            ~destination
          >>=? fun action ->
          Client_proto_baker.call_singlesig
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ?confirmations:cctxt#confirmations
            ~dry_run
            ~fee_parameter
            ~source
            ?fee
            ~src_pk
            ~src_sk
            ~baker
            ~action
            ?gas_limit
            ?storage_limit
            ?counter
            ()
          >>= Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"transfer simulation failed"
                cctxt
          >>= function
          | None -> return_unit | Some (_res, _contracts) -> return_unit);
      command
        ~group:singlesig_group
        ~desc:
          "Submit protocol proposals using a single-signature baker contract"
        Clic.(
          args13
            Client_proto_args.fee_arg
            Client_proto_context_commands.dry_run_switch
            Client_proto_args.gas_limit_arg
            Client_proto_args.storage_limit_arg
            Client_proto_args.counter_arg
            Client_proto_args.no_print_source_flag
            Client_proto_args.minimal_fees_arg
            Client_proto_args.minimal_nanotez_per_byte_arg
            Client_proto_args.minimal_nanotez_per_gas_unit_arg
            Client_proto_args.force_low_fee_arg
            Client_proto_args.fee_cap_arg
            Client_proto_args.burn_cap_arg
            (switch
               ~doc:
                 "Do not fail when the checks that try to prevent the user \
                  from shooting themselves in the foot do."
               ~long:"force"
               ()))
        ( prefixes ["from"; "baker"; "contract"]
        @@ Baker_alias.source_param
        @@ prefixes ["submit"; "proposals"; "for"; "protocols"]
        @@ proposals_param () )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap,
               force )
             baker
             proposals
             (cctxt : #Protocol_client_context.full) ->
          get_baker_consensus_key cctxt ~chain:cctxt#chain baker
          >>=? fun (_alias, source, src_pk, src_sk) ->
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
          get_period_info
            ~successor:true
            ~chain:cctxt#chain
            ~block:cctxt#block
            cctxt
          >>=? fun info ->
          ( match info.current_period_kind with
          | Proposal ->
              return_unit
          | _ ->
              cctxt#error "Not in a proposal period" )
          >>=? fun () ->
          Shell_services.Protocol.list cctxt
          >>=? fun known_protos ->
          get_proposals ~chain:cctxt#chain ~block:cctxt#block cctxt
          >>=? fun known_proposals ->
          Alpha_services.Voting.listings cctxt (cctxt#chain, cctxt#block)
          >>=? fun listings ->
          (* for a proposal to be valid it must either a protocol that was already
           proposed by somebody else or a protocol known by the node, because
           the user is the first proposer and just injected it with
           tezos-admin-client *)
          let check_proposals proposals : bool tzresult Lwt.t =
            let n = List.length proposals in
            let errors = ref [] in
            let error ppf =
              Format.kasprintf (fun s -> errors := s :: !errors) ppf
            in
            if n = 0 then error "Empty proposal list." ;
            if n > Constants.fixed.max_proposals_per_delegate then
              error
                "Too many proposals: %d > %d."
                n
                Constants.fixed.max_proposals_per_delegate ;
            ( match
                Base.List.find_all_dups
                  ~compare:Protocol_hash.compare
                  proposals
              with
            | [] ->
                ()
            | dups ->
                error
                  "There %s: %a."
                  ( if List.length dups = 1 then "is a duplicate proposal"
                  else "are duplicate proposals" )
                  Format.(
                    pp_print_list
                      ~pp_sep:(fun ppf () -> pp_print_string ppf ", ")
                      Protocol_hash.pp)
                  dups ) ;
            List.iter
              (fun (p : Protocol_hash.t) ->
                if
                  List.mem p known_protos
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
                   (fun (baker', _) -> Baker_hash.equal baker' baker)
                   listings)
            then
              error
                "Baker `%a` does not appear to have voting rights."
                Baker_hash.pp
                baker ;
            if !errors <> [] then
              cctxt#message
                "There %s with the submission:%t"
                ( if List.length !errors = 1 then "is an issue"
                else "are issues" )
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
          check_proposals proposals
          >>=? fun all_valid ->
          ( if all_valid then cctxt#message "All proposals are valid."
          else if force then
            cctxt#message
              "Some proposals are not valid, but `--force` was used."
          else cctxt#error "Submission failed because of invalid proposals." )
          >>= fun () ->
          Client_proto_baker.call_singlesig
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ?confirmations:cctxt#confirmations
            ~dry_run
            ~fee_parameter
            ~source
            ?fee
            ~src_pk
            ~src_sk
            ~baker
            ~action:(Client_proto_baker.Submit_proposals proposals)
            ?gas_limit
            ?storage_limit
            ?counter
            ()
          >>= Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"transfer simulation failed"
                cctxt
          >>= function
          | None -> return_unit | Some (_res, _contracts) -> return_unit);
      command
        ~group:singlesig_group
        ~desc:"Submit a ballot using a single-signature baker contract"
        generic_options
        ( prefixes ["from"; "baker"; "contract"]
        @@ Baker_alias.source_param
        @@ prefixes ["submit"; "ballot"; "for"; "protocol"]
        @@ proposal_param () @@ ballot_param () @@ stop )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             baker
             proposal
             ballot
             (cctxt : #Protocol_client_context.full) ->
          get_baker_consensus_key cctxt ~chain:cctxt#chain baker
          >>=? fun (_alias, source, src_pk, src_sk) ->
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
          get_period_info
            ~successor:true
            ~chain:cctxt#chain
            ~block:cctxt#block
            cctxt
          >>=? fun info ->
          ( match info.current_period_kind with
          | Exploration | Promotion ->
              return_unit
          | _ ->
              cctxt#error "Not in Exploration or Promotion period" )
          >>=? fun () ->
          Client_proto_baker.call_singlesig
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ?confirmations:cctxt#confirmations
            ~dry_run
            ~fee_parameter
            ~source
            ?fee
            ~src_pk
            ~src_sk
            ~baker
            ~action:(Client_proto_baker.Submit_ballot (proposal, ballot))
            ?gas_limit
            ?storage_limit
            ?counter
            ()
          >>= Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"transfer simulation failed"
                cctxt
          >>= function
          | None -> return_unit | Some (_res, _contracts) -> return_unit);
      command
        ~group:singlesig_group
        ~desc:
          "Set baker active or inactive using a single-signature baker contract"
        generic_options
        ( prefixes ["set"; "baker"]
        @@ Baker_alias.source_param @@ active_param () @@ stop )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             baker
             active
             (cctxt : #Protocol_client_context.full) ->
          get_baker_consensus_key cctxt ~chain:cctxt#chain baker
          >>=? fun (_alias, source, src_pk, src_sk) ->
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
          Client_proto_baker.call_singlesig
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ?confirmations:cctxt#confirmations
            ~dry_run
            ~fee_parameter
            ~source
            ?fee
            ~src_pk
            ~src_sk
            ~baker
            ~action:(Client_proto_baker.Set_active active)
            ?gas_limit
            ?storage_limit
            ?counter
            ()
          >>= Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"transfer simulation failed"
                cctxt
          >>= function
          | None -> return_unit | Some (_res, _contracts) -> return_unit);
      command
        ~group:singlesig_group
        ~desc:
          "Set baker to accept or decline new delegations using a \
           single-signature baker contract"
        generic_options
        ( prefixes ["set"; "baker"]
        @@ Baker_alias.source_param @@ accept_param ()
        @@ prefixes ["new"; "delegations"]
        @@ stop )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             baker
             accept_delegations
             (cctxt : #Protocol_client_context.full) ->
          get_baker_consensus_key cctxt ~chain:cctxt#chain baker
          >>=? fun (_alias, source, src_pk, src_sk) ->
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
          Client_proto_baker.call_singlesig
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ?confirmations:cctxt#confirmations
            ~dry_run
            ~fee_parameter
            ~source
            ?fee
            ~src_pk
            ~src_sk
            ~baker
            ~action:(Client_proto_baker.Toggle_delegations accept_delegations)
            ?gas_limit
            ?storage_limit
            ?counter
            ()
          >>= Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"transfer simulation failed"
                cctxt
          >>= function
          | None -> return_unit | Some (_res, _contracts) -> return_unit);
      command
        ~group:singlesig_group
        ~desc:
          (Format.sprintf
             "%s %s"
             "Set baker consensus key using a single-signature baker contract."
             set_consensus_key_desc)
        generic_options
        ( prefixes ["set"; "baker"]
        @@ Baker_alias.source_param
        @@ prefixes ["consensus"; "key"; "to"]
        @@ consensus_key_param () @@ stop )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             baker
             (new_key_uri, new_key)
             (cctxt : #Protocol_client_context.full) ->
          ( match new_key with
          | None ->
              Client_keys.public_key new_key_uri
          | Some new_key ->
              return new_key )
          >>=? fun new_key ->
          get_baker_consensus_key cctxt ~chain:cctxt#chain baker
          >>=? fun (_alias, source, src_pk, src_sk) ->
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
          Client_proto_baker.call_singlesig
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ?confirmations:cctxt#confirmations
            ~dry_run
            ~fee_parameter
            ~source
            ?fee
            ~src_pk
            ~src_sk
            ~baker
            ~action:(Client_proto_baker.Set_consensus_key new_key)
            ?gas_limit
            ?storage_limit
            ?counter
            ()
          >>= Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"transfer simulation failed"
                cctxt
          >>= function
          | None -> return_unit | Some (_res, _contracts) -> return_unit);
      command
        ~group:singlesig_group
        ~desc:"Set baker PVSS key using a single-signature baker contract"
        generic_options
        ( prefixes ["set"; "baker"]
        @@ Baker_alias.source_param
        @@ prefixes ["pvss"; "to"]
        @@ pvss_key_param () @@ stop )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             baker
             pvss_key
             (cctxt : #Protocol_client_context.full) ->
          get_baker_consensus_key cctxt ~chain:cctxt#chain baker
          >>=? fun (_alias, source, src_pk, src_sk) ->
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
          Client_proto_baker.call_singlesig
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ?confirmations:cctxt#confirmations
            ~dry_run
            ~fee_parameter
            ~source
            ?fee
            ~src_pk
            ~src_sk
            ~baker
            ~action:(Client_proto_baker.Set_pvss_key pvss_key)
            ?gas_limit
            ?storage_limit
            ?counter
            ()
          >>= Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"transfer simulation failed"
                cctxt
          >>= function
          | None -> return_unit | Some (_res, _contracts) -> return_unit);
      command
        ~group:singlesig_group
        ~desc:
          "Set baker owner keys and threshold using a single-signature baker \
           contract"
        generic_options
        ( prefixes ["set"; "baker"]
        @@ Baker_alias.source_param
        @@ prefixes ["threshold"; "to"]
        @@ threshold_param ()
        @@ prefixes ["and"; "owner"; "keys"; "to"]
        @@ seq_of_param (public_key_param ()) )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             baker
             threshold
             keys
             (cctxt : #Protocol_client_context.full) ->
          List.map_es (fun (pk_uri, _) -> Client_keys.public_key pk_uri) keys
          >>=? fun keys ->
          get_baker_consensus_key cctxt ~chain:cctxt#chain baker
          >>=? fun (_alias, source, src_pk, src_sk) ->
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
          Client_proto_baker.call_singlesig
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ?confirmations:cctxt#confirmations
            ~dry_run
            ~fee_parameter
            ~source
            ?fee
            ~src_pk
            ~src_sk
            ~baker
            ~action:
              (Client_proto_baker.Set_owner_keys (Z.of_int threshold, keys))
            ?gas_limit
            ?storage_limit
            ?counter
            ()
          >>= Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"transfer simulation failed"
                cctxt
          >>= function
          | None -> return_unit | Some (_res, _contracts) -> return_unit);
      command
        ~group:singlesig_group
        ~desc:"Call a lambda expression on a single-signature baker contract"
        generic_options
        ( prefixes ["call"; "generic"; "baker"]
        @@ Baker_alias.source_param
        @@ prefixes ["with"; "lambda"]
        @@ generic_lambda_param () @@ stop )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             baker
             lambda
             (cctxt : #Protocol_client_context.full) ->
          get_baker_consensus_key cctxt ~chain:cctxt#chain baker
          >>=? fun (_alias, source, src_pk, src_sk) ->
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
          Client_proto_baker.prepare_generic_action cctxt lambda
          >>=? fun action ->
          Client_proto_baker.call_singlesig
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ?confirmations:cctxt#confirmations
            ~dry_run
            ~fee_parameter
            ~source
            ?fee
            ~src_pk
            ~src_sk
            ~baker
            ~action
            ?gas_limit
            ?storage_limit
            ?counter
            ()
          >>= Client_proto_context_commands.report_michelson_errors
                ~no_print_source
                ~msg:"transfer simulation failed"
                cctxt
          >>= function
          | None -> return_unit | Some (_res, _contracts) -> return_unit) ]

let print_prepared_command :
    bool -> Client_proto_baker.multisig_prepared_action -> unit =
 fun bytes_only prepared_command ->
  if bytes_only then
    Format.printf "0x%a@." Hex.pp (Hex.of_bytes prepared_command.bytes)
  else
    Format.printf
      "%a@.%a@.%a@."
      (fun ppf x ->
        Format.fprintf ppf "Bytes to sign: '0x%a'" Hex.pp (Hex.of_bytes x))
      prepared_command.bytes
      (fun ppf z ->
        Format.fprintf
          ppf
          "Threshold (number of signatures required): %s"
          (Z.to_string z))
      prepared_command.threshold
      (fun ppf ->
        Format.fprintf
          ppf
          "@[<2>Public keys of the signers:@ %a@]"
          (Format.pp_print_list
             ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ ")
             Signature.Public_key.pp))
      prepared_command.keys

let multisig_commands () : #Protocol_client_context.full Clic.command list =
  Clic.
    [ (* prepare multisig commands *)
      command
        ~group:multisig_group
        ~desc:
          "Display the threshold, public keys, and byte sequence to sign to \
           transfer from multi-signature baker contract."
        (args3
           bytes_only_switch
           Client_proto_args.arg_arg
           Client_proto_args.entrypoint_arg)
        ( prefixes ["prepare"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param @@ prefix "transferring"
        @@ Client_proto_args.tez_param
             ~name:"qty"
             ~desc:"amount taken from the baker contract"
        @@ prefix "to"
        @@ Client_proto_contracts.Contract_alias.destination_param
             ~name:"dst"
             ~desc:"name/literal of the destination contract"
        @@ stop )
        (fun (bytes_only, arg, entrypoint)
             baker
             amount
             (_, destination)
             (cctxt : #Protocol_client_context.full) ->
          Client_proto_baker.prepare_transfer_action
            ?arg
            ?entrypoint
            cctxt
            ~amount
            ~destination
          >>=? fun action ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action
            ()
          >|=? print_prepared_command bytes_only);
      command
        ~group:multisig_group
        ~desc:
          "Display the threshold, public keys, and byte sequence to sign to \
           submit protocol proposals from multi-signature baker contract."
        (args1 bytes_only_switch)
        ( prefixes ["prepare"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["submitting"; "proposals"]
        @@ proposals_param () )
        (fun bytes_only baker proposals (cctxt : #Protocol_client_context.full) ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action:(Client_proto_baker.Submit_proposals proposals)
            ()
          >|=? print_prepared_command bytes_only);
      command
        ~group:multisig_group
        ~desc:
          "Display the threshold, public keys, and byte sequence to sign to \
           submit a ballot from multi-signature baker contract."
        (args1 bytes_only_switch)
        ( prefixes ["prepare"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["submitting"; "ballot"]
        @@ proposal_param () @@ ballot_param () @@ stop )
        (fun bytes_only
             baker
             proposal
             ballot
             (cctxt : #Protocol_client_context.full) ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action:(Client_proto_baker.Submit_ballot (proposal, ballot))
            ()
          >|=? print_prepared_command bytes_only);
      command
        ~group:multisig_group
        ~desc:
          "Display the threshold, public keys, and byte sequence to sign to \
           set a multi-signature baker active or inactive."
        (args1 bytes_only_switch)
        ( prefixes ["prepare"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["setting"; "it"]
        @@ active_param () @@ stop )
        (fun bytes_only baker active (cctxt : #Protocol_client_context.full) ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action:(Client_proto_baker.Set_active active)
            ()
          >|=? print_prepared_command bytes_only);
      command
        ~group:multisig_group
        ~desc:
          "Display the threshold, public keys, and byte sequence to sign to \
           set a multi-signature baker to accept or decline new delegations."
        (args1 bytes_only_switch)
        ( prefixes ["prepare"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["setting"; "it"]
        @@ accept_param ()
        @@ prefixes ["new"; "delegations"]
        @@ stop )
        (fun bytes_only
             baker
             accept_delegations
             (cctxt : #Protocol_client_context.full) ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action:(Client_proto_baker.Toggle_delegations accept_delegations)
            ()
          >|=? print_prepared_command bytes_only);
      command
        ~group:multisig_group
        ~desc:
          (Format.sprintf
             "%s %s"
             "Display the threshold, public keys, and byte sequence to sign \
              to set consensus key of a multi-signature baker contract."
             set_consensus_key_desc)
        (args1 bytes_only_switch)
        ( prefixes ["prepare"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["setting"; "consensus"; "key"; "to"]
        @@ consensus_key_param () @@ stop )
        (fun bytes_only
             baker
             (new_key_uri, new_key)
             (cctxt : #Protocol_client_context.full) ->
          ( match new_key with
          | None ->
              Client_keys.public_key new_key_uri
          | Some new_key ->
              return new_key )
          >>=? fun new_key ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action:(Client_proto_baker.Set_consensus_key new_key)
            ()
          >|=? print_prepared_command bytes_only);
      command
        ~group:multisig_group
        ~desc:
          (Format.sprintf
             "%s %s"
             "Display the threshold, public keys, and byte sequence to sign \
              to set PVSS key of a multi-signature baker contract."
             set_consensus_key_desc)
        (args1 bytes_only_switch)
        ( prefixes ["prepare"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["setting"; "pvss"; "to"]
        @@ pvss_key_param () @@ stop )
        (fun bytes_only baker pvss_key (cctxt : #Protocol_client_context.full) ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action:(Client_proto_baker.Set_pvss_key pvss_key)
            ()
          >|=? print_prepared_command bytes_only);
      command
        ~group:multisig_group
        ~desc:
          "Display the threshold, public keys, and byte sequence to sign to \
           set owner keys and threshold of a multi-signature baker contract."
        (args1 bytes_only_switch)
        ( prefixes ["prepare"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["setting"; "threshold"; "to"]
        @@ threshold_param ()
        @@ prefixes ["and"; "owner"; "keys"; "to"]
        @@ seq_of_param (public_key_param ()) )
        (fun bytes_only
             baker
             threshold
             keys
             (cctxt : #Protocol_client_context.full) ->
          List.map_es (fun (pk_uri, _) -> Client_keys.public_key pk_uri) keys
          >>=? fun keys ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action:
              (Client_proto_baker.Set_owner_keys (Z.of_int threshold, keys))
            ()
          >|=? print_prepared_command bytes_only);
      command
        ~group:multisig_group
        ~desc:
          "Display the threshold, public keys, and byte sequence to sign to \
           call a lambda expression on a multi-signature baker contract"
        (args1 bytes_only_switch)
        ( prefixes ["prepare"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["calling"; "generic"; "lambda"]
        @@ generic_lambda_param () @@ stop )
        (fun bytes_only baker lambda (cctxt : #Protocol_client_context.full) ->
          Client_proto_baker.prepare_generic_action cctxt lambda
          >>=? fun action ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action
            ()
          >|=? print_prepared_command bytes_only);
      (* sign multisig commands *)
      command
        ~group:multisig_group
        ~desc:"Sign a transfer from multi-signature baker contract."
        (args2 Client_proto_args.arg_arg Client_proto_args.entrypoint_arg)
        ( prefixes ["sign"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param @@ prefix "transferring"
        @@ Client_proto_args.tez_param
             ~name:"qty"
             ~desc:"amount taken from the baker contract"
        @@ prefix "to"
        @@ Client_proto_contracts.Contract_alias.destination_param
             ~name:"dst"
             ~desc:"name/literal of the destination contract"
        @@ prefixes ["with"; "key"]
        @@ owner_key_param () @@ stop )
        (fun (arg, entrypoint)
             baker
             amount
             (_, destination)
             (_, sk)
             (cctxt : #Protocol_client_context.full) ->
          Client_proto_baker.prepare_transfer_action
            ?arg
            ?entrypoint
            cctxt
            ~amount
            ~destination
          >>=? fun action ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action
            ()
          >>=? fun prepared_command ->
          Client_keys.sign cctxt sk prepared_command.bytes
          >|=? Format.printf "%a@." Signature.pp);
      command
        ~group:multisig_group
        ~desc:
          "Sign submitting protocol proposals from multi-signature baker \
           contract."
        no_options
        ( prefixes ["sign"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["submitting"; "proposals"]
        @@ non_terminal_seq (proposal_param ()) ~suffix:["with"; "key"]
        @@ owner_key_param () @@ stop )
        (fun () baker proposals (_, sk) (cctxt : #Protocol_client_context.full) ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action:(Client_proto_baker.Submit_proposals proposals)
            ()
          >>=? fun prepared_command ->
          Client_keys.sign cctxt sk prepared_command.bytes
          >|=? Format.printf "%a@." Signature.pp);
      command
        ~group:multisig_group
        ~desc:"Sign submitting ballot from multi-signature baker contract."
        no_options
        ( prefixes ["sign"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["submitting"; "ballot"]
        @@ proposal_param () @@ ballot_param ()
        @@ prefixes ["with"; "key"]
        @@ owner_key_param () @@ stop )
        (fun ()
             baker
             proposal
             ballot
             (_, sk)
             (cctxt : #Protocol_client_context.full) ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action:(Client_proto_baker.Submit_ballot (proposal, ballot))
            ()
          >>=? fun prepared_command ->
          Client_keys.sign cctxt sk prepared_command.bytes
          >|=? Format.printf "%a@." Signature.pp);
      command
        ~group:multisig_group
        ~desc:"Sign setting a multi-signature baker active or inactive."
        no_options
        ( prefixes ["sign"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["setting"; "it"]
        @@ active_param ()
        @@ prefixes ["with"; "key"]
        @@ owner_key_param () @@ stop )
        (fun () baker active (_, sk) (cctxt : #Protocol_client_context.full) ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action:(Client_proto_baker.Set_active active)
            ()
          >>=? fun prepared_command ->
          Client_keys.sign cctxt sk prepared_command.bytes
          >|=? Format.printf "%a@." Signature.pp);
      command
        ~group:multisig_group
        ~desc:
          "Sign setting a multi-signature baker to accept or decline new \
           delegations."
        no_options
        ( prefixes ["sign"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["setting"; "it"]
        @@ accept_param ()
        @@ prefixes ["new"; "delegations"; "with"; "key"]
        @@ owner_key_param () @@ stop )
        (fun ()
             baker
             accept_delegations
             (_, sk)
             (cctxt : #Protocol_client_context.full) ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action:(Client_proto_baker.Toggle_delegations accept_delegations)
            ()
          >>=? fun prepared_command ->
          Client_keys.sign cctxt sk prepared_command.bytes
          >|=? Format.printf "%a@." Signature.pp);
      command
        ~group:multisig_group
        ~desc:
          (Format.sprintf
             "%s %s"
             "Sign setting consensus key of a multi-signature baker contract."
             set_consensus_key_desc)
        no_options
        ( prefixes ["sign"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["setting"; "consensus"; "key"; "to"]
        @@ consensus_key_param ()
        @@ prefixes ["with"; "key"]
        @@ owner_key_param () @@ stop )
        (fun ()
             baker
             (new_key_uri, new_key)
             (_, sk)
             (cctxt : #Protocol_client_context.full) ->
          ( match new_key with
          | None ->
              Client_keys.public_key new_key_uri
          | Some new_key ->
              return new_key )
          >>=? fun new_key ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action:(Client_proto_baker.Set_consensus_key new_key)
            ()
          >>=? fun prepared_command ->
          Client_keys.sign cctxt sk prepared_command.bytes
          >|=? Format.printf "%a@." Signature.pp);
      command
        ~group:multisig_group
        ~desc:
          (Format.sprintf
             "%s %s"
             "Sign setting PVSS key of a multi-signature baker contract."
             set_consensus_key_desc)
        no_options
        ( prefixes ["sign"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["setting"; "pvss"; "to"]
        @@ pvss_key_param ()
        @@ prefixes ["with"; "key"]
        @@ owner_key_param () @@ stop )
        (fun () baker pvss_key (_, sk) (cctxt : #Protocol_client_context.full) ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action:(Client_proto_baker.Set_pvss_key pvss_key)
            ()
          >>=? fun prepared_command ->
          Client_keys.sign cctxt sk prepared_command.bytes
          >|=? Format.printf "%a@." Signature.pp);
      command
        ~group:multisig_group
        ~desc:
          "Sign setting owner keys and threshold of a multi-signature baker \
           contract."
        no_options
        ( prefixes ["sign"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["setting"; "threshold"; "to"]
        @@ threshold_param ()
        @@ prefixes ["and"; "owner"; "keys"; "to"]
        @@ non_terminal_seq (public_key_param ()) ~suffix:["with"; "key"]
        @@ owner_key_param () @@ stop )
        (fun ()
             baker
             threshold
             keys
             (_, sk)
             (cctxt : #Protocol_client_context.full) ->
          List.map_es (fun (pk_uri, _) -> Client_keys.public_key pk_uri) keys
          >>=? fun keys ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action:
              (Client_proto_baker.Set_owner_keys (Z.of_int threshold, keys))
            ()
          >>=? fun prepared_command ->
          Client_keys.sign cctxt sk prepared_command.bytes
          >|=? Format.printf "%a@." Signature.pp);
      command
        ~group:multisig_group
        ~desc:
          "Display the threshold, public keys, and byte sequence to sign to \
           call a lambda expression on a multi-signature baker contract"
        no_options
        ( prefixes ["sign"; "baker"; "transaction"; "on"]
        @@ Baker_alias.source_param
        @@ prefixes ["calling"; "generic"; "lambda"]
        @@ generic_lambda_param ()
        @@ prefixes ["with"; "key"]
        @@ owner_key_param () @@ stop )
        (fun () baker lambda (_, sk) (cctxt : #Protocol_client_context.full) ->
          Client_proto_baker.prepare_generic_action cctxt lambda
          >>=? fun action ->
          Client_proto_baker.prepare_multisig_transaction
            cctxt
            ~chain:cctxt#chain
            ~block:cctxt#block
            ~baker
            ~action
            ()
          >>=? fun prepared_command ->
          Client_keys.sign cctxt sk prepared_command.bytes
          >|=? Format.printf "%a@." Signature.pp);
      (* perform multisig commands *)
      command
        ~group:multisig_group
        ~desc:"Transfer tokens using a multi-signature baker contract"
        transfer_options
        ( prefixes ["from"; "multisig"; "baker"]
        @@ Baker_alias.source_param @@ prefix "transfer"
        @@ Client_proto_args.tez_param
             ~name:"qty"
             ~desc:"amount taken from the baker contract"
        @@ prefix "to"
        @@ Client_proto_contracts.Contract_alias.destination_param
             ~name:"dst"
             ~desc:"name/literal of the destination contract"
        @@ prefixes ["on"; "behalf"; "of"]
        @@ implicit_source_param ()
        @@ prefixes ["with"; "signatures"]
        @@ seq_of_param (signature_param ()) )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap,
               arg,
               entrypoint )
             baker
             amount
             (_, destination)
             (_, source)
             signatures
             (cctxt : #Protocol_client_context.full) ->
          get_source_keys cctxt source
          >>=? function
          | None ->
              failwith
                "only implicit and baker accounts can be the source of a \
                 contract call"
          | Some (_name, source, src_pk, src_sk) -> (
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
              Client_proto_baker.prepare_transfer_action
                ?arg
                ?entrypoint
                cctxt
                ~amount
                ~destination
              >>=? fun action ->
              Client_proto_baker.call_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~baker
                ~signatures
                ~action
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit | Some (_res, _contracts) -> return_unit ));
      command
        ~group:multisig_group
        ~desc:
          "Submit protocol proposals using a multi-signature baker contract"
        Clic.(
          args13
            Client_proto_args.fee_arg
            Client_proto_context_commands.dry_run_switch
            Client_proto_args.gas_limit_arg
            Client_proto_args.storage_limit_arg
            Client_proto_args.counter_arg
            Client_proto_args.no_print_source_flag
            Client_proto_args.minimal_fees_arg
            Client_proto_args.minimal_nanotez_per_byte_arg
            Client_proto_args.minimal_nanotez_per_gas_unit_arg
            Client_proto_args.force_low_fee_arg
            Client_proto_args.fee_cap_arg
            Client_proto_args.burn_cap_arg
            (switch
               ~doc:
                 "Do not fail when the checks that try to prevent the user \
                  from shooting themselves in the foot do."
               ~long:"force"
               ()))
        ( prefixes ["from"; "multisig"; "baker"]
        @@ Baker_alias.source_param
        @@ prefixes ["submit"; "proposals"; "on"; "behalf"; "of"]
        @@ implicit_source_param ()
        @@ prefixes ["with"; "signatures"]
        @@ non_terminal_seq (signature_param ()) ~suffix:["for"; "protocols"]
        @@ seq_of_param (proposal_param ()) )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap,
               force )
             baker
             (_, source)
             signatures
             proposals
             (cctxt : #Protocol_client_context.full) ->
          get_source_keys cctxt source
          >>=? function
          | None ->
              failwith
                "only implicit and baker accounts can be the source of a \
                 contract call"
          | Some (_name, source, src_pk, src_sk) -> (
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
              get_period_info
                ~successor:true
                ~chain:cctxt#chain
                ~block:cctxt#block
                cctxt
              >>=? fun info ->
              ( match info.current_period_kind with
              | Proposal ->
                  return_unit
              | _ ->
                  cctxt#error "Not in a proposal period" )
              >>=? fun () ->
              Shell_services.Protocol.list cctxt
              >>=? fun known_protos ->
              get_proposals ~chain:cctxt#chain ~block:cctxt#block cctxt
              >>=? fun known_proposals ->
              Alpha_services.Voting.listings cctxt (cctxt#chain, cctxt#block)
              >>=? fun listings ->
              (* for a proposal to be valid it must either a protocol that was already
           proposed by somebody else or a protocol known by the node, because
           the user is the first proposer and just injected it with
           tezos-admin-client *)
              let check_proposals proposals : bool tzresult Lwt.t =
                let n = List.length proposals in
                let errors = ref [] in
                let error ppf =
                  Format.kasprintf (fun s -> errors := s :: !errors) ppf
                in
                if n = 0 then error "Empty proposal list." ;
                if n > Constants.fixed.max_proposals_per_delegate then
                  error
                    "Too many proposals: %d > %d."
                    n
                    Constants.fixed.max_proposals_per_delegate ;
                ( match
                    Base.List.find_all_dups
                      ~compare:Protocol_hash.compare
                      proposals
                  with
                | [] ->
                    ()
                | dups ->
                    error
                      "There %s: %a."
                      ( if List.length dups = 1 then "is a duplicate proposal"
                      else "are duplicate proposals" )
                      Format.(
                        pp_print_list
                          ~pp_sep:(fun ppf () -> pp_print_string ppf ", ")
                          Protocol_hash.pp)
                      dups ) ;
                List.iter
                  (fun (p : Protocol_hash.t) ->
                    if
                      List.mem p known_protos
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
                       (fun (baker', _) -> Baker_hash.equal baker' baker)
                       listings)
                then
                  error
                    "Baker `%a` does not appear to have voting rights."
                    Baker_hash.pp
                    baker ;
                if !errors <> [] then
                  cctxt#message
                    "There %s with the submission:%t"
                    ( if List.length !errors = 1 then "is an issue"
                    else "are issues" )
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
              check_proposals proposals
              >>=? fun all_valid ->
              ( if all_valid then cctxt#message "All proposals are valid."
              else if force then
                cctxt#message
                  "Some proposals are not valid, but `--force` was used."
              else
                cctxt#error "Submission failed because of invalid proposals."
              )
              >>= fun () ->
              Client_proto_baker.call_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~baker
                ~signatures
                ~action:(Client_proto_baker.Submit_proposals proposals)
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit | Some (_res, _contracts) -> return_unit ));
      command
        ~group:multisig_group
        ~desc:"Submit a ballot using a multi-signature baker contract"
        generic_options
        ( prefixes ["from"; "multisig"; "baker"]
        @@ Baker_alias.source_param
        @@ prefixes ["submit"; "ballot"; "on"; "behalf"; "of"]
        @@ implicit_source_param ()
        @@ prefixes ["with"; "signatures"]
        @@ non_terminal_seq (signature_param ()) ~suffix:["for"; "protocol"]
        @@ proposal_param () @@ ballot_param () @@ stop )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             baker
             (_, source)
             signatures
             proposal
             ballot
             (cctxt : #Protocol_client_context.full) ->
          get_source_keys cctxt source
          >>=? function
          | None ->
              failwith
                "only implicit and baker accounts can be the source of a \
                 contract call"
          | Some (_name, source, src_pk, src_sk) -> (
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
              get_period_info
                ~successor:true
                ~chain:cctxt#chain
                ~block:cctxt#block
                cctxt
              >>=? fun info ->
              ( match info.current_period_kind with
              | Exploration | Promotion ->
                  return_unit
              | _ ->
                  cctxt#error "Not in Exploration or Promotion period" )
              >>=? fun () ->
              Client_proto_baker.call_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~baker
                ~signatures
                ~action:(Client_proto_baker.Submit_ballot (proposal, ballot))
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit | Some (_res, _contracts) -> return_unit ));
      command
        ~group:multisig_group
        ~desc:
          "Set baker active or inactive using a multi-signature baker contract"
        generic_options
        ( prefixes ["set"; "multisig"; "baker"]
        @@ Baker_alias.source_param @@ active_param ()
        @@ prefixes ["on"; "behalf"; "of"]
        @@ implicit_source_param ()
        @@ prefixes ["with"; "signatures"]
        @@ seq_of_param (signature_param ()) )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             baker
             active
             (_, source)
             signatures
             (cctxt : #Protocol_client_context.full) ->
          get_source_keys cctxt source
          >>=? function
          | None ->
              failwith
                "only implicit and baker accounts can be the source of a \
                 contract call"
          | Some (_name, source, src_pk, src_sk) -> (
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
              Client_proto_baker.call_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~baker
                ~signatures
                ~action:(Client_proto_baker.Set_active active)
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit | Some (_res, _contracts) -> return_unit ));
      command
        ~group:multisig_group
        ~desc:
          "Set baker to accept or decline new delegations using a \
           multi-signature baker contract"
        generic_options
        ( prefixes ["set"; "multisig"; "baker"]
        @@ Baker_alias.source_param @@ accept_param ()
        @@ prefixes ["new"; "delegations"; "on"; "behalf"; "of"]
        @@ implicit_source_param ()
        @@ prefixes ["with"; "signatures"]
        @@ seq_of_param (signature_param ()) )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             baker
             accept_delegations
             (_, source)
             signatures
             (cctxt : #Protocol_client_context.full) ->
          get_source_keys cctxt source
          >>=? function
          | None ->
              failwith
                "only implicit and baker accounts can be the source of a \
                 contract call"
          | Some (_name, source, src_pk, src_sk) -> (
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
              Client_proto_baker.call_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~baker
                ~signatures
                ~action:
                  (Client_proto_baker.Toggle_delegations accept_delegations)
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit | Some (_res, _contracts) -> return_unit ));
      command
        ~group:multisig_group
        ~desc:
          (Format.sprintf
             "%s %s"
             "Set baker consensus key using a multi-signature baker contract."
             set_consensus_key_desc)
        generic_options
        ( prefixes ["set"; "multisig"; "baker"]
        @@ Baker_alias.source_param
        @@ prefixes ["consensus"; "key"; "to"]
        @@ consensus_key_param ()
        @@ prefixes ["on"; "behalf"; "of"]
        @@ implicit_source_param ()
        @@ prefixes ["with"; "signatures"]
        @@ seq_of_param (signature_param ()) )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             baker
             (new_key_uri, new_key)
             (_, source)
             signatures
             (cctxt : #Protocol_client_context.full) ->
          ( match new_key with
          | None ->
              Client_keys.public_key new_key_uri
          | Some new_key ->
              return new_key )
          >>=? fun new_key ->
          get_source_keys cctxt source
          >>=? function
          | None ->
              failwith
                "only implicit and baker accounts can be the source of a \
                 contract call"
          | Some (_name, source, src_pk, src_sk) -> (
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
              Client_proto_baker.call_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~baker
                ~signatures
                ~action:(Client_proto_baker.Set_consensus_key new_key)
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit | Some (_res, _contracts) -> return_unit ));
      command
        ~group:multisig_group
        ~desc:
          (Format.sprintf
             "%s %s"
             "Set baker PVSS key using a multi-signature baker contract."
             set_consensus_key_desc)
        generic_options
        ( prefixes ["set"; "multisig"; "baker"]
        @@ Baker_alias.source_param
        @@ prefixes ["pvss"; "to"]
        @@ pvss_key_param ()
        @@ prefixes ["on"; "behalf"; "of"]
        @@ implicit_source_param ()
        @@ prefixes ["with"; "signatures"]
        @@ seq_of_param (signature_param ()) )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             baker
             pvss_key
             (_, source)
             signatures
             (cctxt : #Protocol_client_context.full) ->
          get_source_keys cctxt source
          >>=? function
          | None ->
              failwith
                "only implicit and baker accounts can be the source of a \
                 contract call"
          | Some (_name, source, src_pk, src_sk) -> (
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
              Client_proto_baker.call_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~baker
                ~signatures
                ~action:(Client_proto_baker.Set_pvss_key pvss_key)
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit | Some (_res, _contracts) -> return_unit ));
      command
        ~group:multisig_group
        ~desc:
          "Set baker owner keys and threshold using a multi-signature baker \
           contract"
        generic_options
        ( prefixes ["set"; "multisig"; "baker"]
        @@ Baker_alias.source_param
        @@ prefixes ["threshold"; "to"]
        @@ threshold_param ()
        @@ prefixes ["and"; "owner"; "keys"; "to"]
        @@ non_terminal_seq (public_key_param ()) ~suffix:["on"; "behalf"; "of"]
        @@ implicit_source_param ()
        @@ prefixes ["with"; "signatures"]
        @@ seq_of_param (signature_param ()) )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             baker
             threshold
             keys
             (_, source)
             signatures
             (cctxt : #Protocol_client_context.full) ->
          get_source_keys cctxt source
          >>=? function
          | None ->
              failwith
                "only implicit and baker accounts can be the source of a \
                 contract call"
          | Some (_name, source, src_pk, src_sk) -> (
              List.map_es
                (fun (pk_uri, _) -> Client_keys.public_key pk_uri)
                keys
              >>=? fun keys ->
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
              Client_proto_baker.call_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ?confirmations:cctxt#confirmations
                ~dry_run
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~baker
                ~signatures
                ~action:
                  (Client_proto_baker.Set_owner_keys (Z.of_int threshold, keys))
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit | Some (_res, _contracts) -> return_unit ));
      command
        ~group:multisig_group
        ~desc:"Call a lambda expression on a multi-signature baker contract"
        generic_options
        ( prefixes ["call"; "generic"; "multisig"; "baker"]
        @@ Baker_alias.source_param
        @@ prefixes ["with"; "lambda"]
        @@ generic_lambda_param ()
        @@ prefixes ["on"; "behalf"; "of"]
        @@ implicit_source_param ()
        @@ prefixes ["with"; "signatures"]
        @@ seq_of_param (signature_param ()) )
        (fun ( fee,
               dry_run,
               gas_limit,
               storage_limit,
               counter,
               no_print_source,
               minimal_fees,
               minimal_nanotez_per_byte,
               minimal_nanotez_per_gas_unit,
               force_low_fee,
               fee_cap,
               burn_cap )
             baker
             lambda
             (_, source)
             signatures
             (cctxt : #Protocol_client_context.full) ->
          get_source_keys cctxt source
          >>=? function
          | None ->
              failwith
                "only implicit accounts can be the source of a contract call"
          | Some (_name, source, src_pk, src_sk) -> (
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
              Client_proto_baker.prepare_generic_action cctxt lambda
              >>=? fun action ->
              Client_proto_baker.call_multisig
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ~dry_run
                ~fee_parameter
                ~source
                ?fee
                ~src_pk
                ~src_sk
                ~baker
                ~signatures
                ~action
                ?gas_limit
                ?storage_limit
                ?counter
                ()
              >>= Client_proto_context_commands.report_michelson_errors
                    ~no_print_source
                    ~msg:"transfer simulation failed"
                    cctxt
              >>= function
              | None -> return_unit | Some (_res, _contracts) -> return_unit ))
    ]

let commands () : #Protocol_client_context.full Clic.command list =
  singlesig_commands () @ multisig_commands ()
