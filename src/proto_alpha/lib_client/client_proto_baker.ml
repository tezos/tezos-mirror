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
open Tezos_micheline
open Michelson_v1_primitives
open Michelson_v1_helpers

type transfer_to_scripted = {
  amount : Tez.t;
  destination : Contract.t;
  entrypoint : string;
  parameter : Script.expr;
  parameter_type : Script.expr;
}

type action =
  | Transfer_to_implicit of Tez.t * Signature.Public_key_hash.t
  | Transfer_to_scripted of transfer_to_scripted
  | Submit_proposals of Protocol_hash.t list
  | Submit_ballot of Protocol_hash.t * Vote.ballot
  | Set_active of bool
  | Toggle_delegations of bool
  | Set_consensus_key of Signature.Public_key.t * Signature.t
  | Set_owner_keys of Z.t * Signature.Public_key.t list
  | Set_pvss_key of Pvss_secp256k1.Public_key.t
  | Generic of Script.expr

type multisig_prepared_action = {
  bytes : Bytes.t;
  threshold : Z.t;
  keys : public_key list;
  counter : Z.t;
}

type error += More_than_one_key

let () =
  Protocol_client_context.register_error_kind
    `Permanent
    ~id:"baker_script.more_than_one_key"
    ~title:"More_than_one_key"
    ~description:
      "The script storage has more than one owner key. Use multi-signature \
       baker commands"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The script storage has more than one owner key. Use multi-signature \
         baker commands")
    Data_encoding.empty
    (function More_than_one_key -> Some () | _ -> None)
    (fun () -> More_than_one_key)

let generic_entrypoint = "main"

let generic_lambda_type =
  "lambda unit (pair (list operation) (list baker_operation))"

let loc : Micheline.canonical_location = 0

let parse_expr expr =
  Lwt.return @@ Micheline_parser.no_parsing_error
  @@ Michelson_v1_parser.parse_expression expr

let prepare_transfer_action ?arg ?(entrypoint = "default")
    (cctxt : #Protocol_client_context.full) ~amount ~destination =
  match Alpha_context.Contract.is_implicit destination with
  | Some pkh when entrypoint = "default" ->
      return @@ Transfer_to_implicit (amount, pkh)
  | Some _ ->
      cctxt#error
        "Implicit accounts have no entrypoints. (targeted entrypoint %%%s on \
         contract %a)"
        entrypoint
        Contract.pp
        destination
  | None ->
      Michelson_v1_entrypoints.contract_entrypoint_type
        cctxt
        ~chain:cctxt#chain
        ~block:cctxt#block
        ~contract:destination
        ~entrypoint
      >>=? (function
             | None ->
                 cctxt#error
                   "Contract %a has no entrypoint named %s"
                   Contract.pp
                   destination
                   entrypoint
             | Some parameter_type ->
                 return @@ parameter_type)
      >>=? fun parameter_type ->
      ( match arg with
      | Some arg ->
          parse_expr arg >>=? fun {expanded = arg; _} -> return_some arg
      | None ->
          return_none )
      >>=? fun parameter ->
      let parameter =
        Option.value
          ~default:
            (Micheline.strip_locations @@ Michelson_v1_helpers.d_unit ~loc)
          parameter
      in
      return
      @@ Transfer_to_scripted
           {amount; destination; entrypoint; parameter; parameter_type}

let prepare_set_consensus_key_proof (cctxt : #Protocol_client_context.full)
    ~chain_id ~baker key =
  let bytes =
    let ({shell; protocol_data = {contents; signature = _}}
          : Kind.failing_noop Operation.t) =
      Apply.set_baker_consensus_key_proof_operation chain_id baker None
    in
    Data_encoding.Binary.to_bytes_exn
      Operation.unsigned_encoding
      (shell, Contents_list contents)
  in
  let pkh = Signature.Public_key.hash key in
  Client_keys.get_key cctxt pkh
  >>=? fun (_, _, sk) ->
  Client_keys.sign cctxt ~watermark:Signature.Generic_operation sk bytes

let prepare_set_consensus_key_action (cctxt : #Protocol_client_context.full)
    ~chain_id ~baker key =
  prepare_set_consensus_key_proof cctxt ~chain_id ~baker key
  >>=? fun proof -> return (Set_consensus_key (key, proof))

let prepare_generic_action cctxt lambda =
  parse_expr generic_lambda_type
  >>=? fun lambda_ty ->
  Client_proto_programs.typecheck_data
    cctxt
    ~chain:cctxt#chain
    ~block:cctxt#block
    ~data:lambda
    ~ty:lambda_ty
    ()
  >>= function
  | Error errs ->
      failwith
        "%a"
        (Michelson_v1_error_reporter.report_errors
           ~details:false
           ~show_source:false
           ?parsed:None)
        errs
  | Ok _gas ->
      (* The lambda has to be unparsed in [Script_ir_translator.Optimised] mode,
         done by passing it through [pack_data]. *)
      Alpha_services.Helpers.Scripts.pack_data
        cctxt
        (cctxt#chain, cctxt#block)
        ?gas:None
        ~data:lambda.expanded
        ~ty:lambda_ty.expanded
      >>= (function
            | Ok (bytes, _remaining_gas) -> (
                (* Remove first byte *)
                let bytes = Bytes.sub bytes 1 (Bytes.length bytes - 1) in
                match
                  Data_encoding.Binary.of_bytes_opt
                    Alpha_context.Script.expr_encoding
                    bytes
                with
                | None ->
                    failwith
                      "Internal error: Could not decode bytes in the packed \
                       lambda expression"
                | Some expr ->
                    return expr )
            | Error errs ->
                failwith
                  "%a"
                  (Michelson_v1_error_reporter.report_errors
                     ~details:false
                     ~show_source:false
                     ?parsed:None)
                  errs)
      >>=? fun lambda -> return @@ Generic lambda

let mk_payload ~stored_counter ~action =
  pair
    ~loc
    (int ~loc stored_counter)
    ( match action with
    (* Expected action parameters structure is:
    [(or :action
       (lambda %operation unit (list operation))
       (pair %change_keys
          (nat %threshold)
          (list %keys key)))]
    *)
    | Transfer_to_implicit (amount, pkh) ->
        let key_hash =
          bytes ~loc
          @@ Data_encoding.Binary.to_bytes_exn
               Signature.Public_key_hash.encoding
               pkh
        in
        let amount = Tez.to_mutez amount |> Z.of_int64 in
        left ~loc
        @@ (* [tezos-client convert data '{ DROP ; NIL baker_operation ; NIL operation ; PUSH key_hash "tz1ivoFEvbfbUNav5FwLvmxzMGcNXWxY9qTD" ; IMPLICIT_ACCOUNT ; PUSH mutez 1 ; UNIT ; TRANSFER_TOKENS ; CONS ; PAIR }' from michelson to ocaml --type 'lambda unit (pair (list operation) (list baker_operation))' --zero-loc] *)
           Seq
             ( 0,
               [ Prim (0, I_DROP, [], []);
                 Prim (0, I_NIL, [Prim (0, T_baker_operation, [], [])], []);
                 Prim (0, I_NIL, [Prim (0, T_operation, [], [])], []);
                 Prim (0, I_PUSH, [Prim (0, T_key_hash, [], []); key_hash], []);
                 Prim (0, I_IMPLICIT_ACCOUNT, [], []);
                 Prim
                   (0, I_PUSH, [Prim (0, T_mutez, [], []); Int (0, amount)], []);
                 Prim (0, I_UNIT, [], []);
                 Prim (0, I_TRANSFER_TOKENS, [], []);
                 Prim (0, I_CONS, [], []);
                 Prim (0, I_PAIR, [], []) ] )
    | Transfer_to_scripted
        {amount; destination; entrypoint; parameter; parameter_type} ->
        let address =
          bytes ~loc
          @@ Data_encoding.Binary.to_bytes_exn
               Data_encoding.(tup2 Contract.encoding Variable.string)
               (destination, "")
        in
        let amount = Tez.to_mutez amount |> Z.of_int64 in
        let entrypoint_annot =
          match entrypoint with "default" -> [] | _ -> ["%" ^ entrypoint]
        in
        let push_parameter =
          if
            parameter_type
            = Micheline.strip_locations @@ Michelson_v1_helpers.t_unit ~loc
          then Micheline.Prim (0, I_UNIT, [], [])
          else
            Prim
              ( 0,
                I_PUSH,
                [Micheline.root parameter_type; Micheline.root parameter],
                [] )
        in
        left ~loc
        @@ (* [tezos-client convert data '{ DROP ; NIL baker_operation ; NIL operation ; PUSH address "KT1DieU51jzXLerQx5AqMCiLC1SsCeM8yRat" ; CONTRACT %entrypoint unit ; IF_NONE { UNIT ; FAILWITH } {} ; PUSH mutez 1 ; UNIT ; TRANSFER_TOKENS ; CONS ; PAIR }' from michelson to ocaml --type 'lambda unit (pair (list operation) (list baker_operation))' --zero-loc] *)
           Seq
             ( 0,
               [ Prim (0, I_DROP, [], []);
                 Prim (0, I_NIL, [Prim (0, T_baker_operation, [], [])], []);
                 Prim (0, I_NIL, [Prim (0, T_operation, [], [])], []);
                 Prim (0, I_PUSH, [Prim (0, T_address, [], []); address], []);
                 Prim
                   ( 0,
                     I_CONTRACT,
                     [Micheline.root parameter_type],
                     entrypoint_annot );
                 Prim
                   ( 0,
                     I_IF_NONE,
                     [ Seq
                         ( 0,
                           [ Prim (0, I_UNIT, [], []);
                             Prim (0, I_FAILWITH, [], []) ] );
                       Seq (0, []) ],
                     [] );
                 Prim
                   (0, I_PUSH, [Prim (0, T_mutez, [], []); Int (0, amount)], []);
                 push_parameter;
                 Prim (0, I_TRANSFER_TOKENS, [], []);
                 Prim (0, I_CONS, [], []);
                 Prim (0, I_PAIR, [], []) ] )
    | Submit_proposals proposals ->
        let proposals = List.map (protocol_hash ~loc) proposals in
        left ~loc
        @@ (* [tezos-client convert data '{ DROP ; NIL baker_operation ; PUSH (list string) { "PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb" } ; SUBMIT_PROPOSALS ; CONS ; NIL operation ; PAIR }' from michelson to ocaml --type 'lambda unit (pair (list operation) (list baker_operation))' --zero-loc] *)
           Seq
             ( 0,
               [ Prim (0, I_DROP, [], []);
                 Prim (0, I_NIL, [Prim (0, T_baker_operation, [], [])], []);
                 Prim
                   ( 0,
                     I_PUSH,
                     [ Prim (0, T_list, [Prim (0, T_string, [], [])], []);
                       Seq (0, proposals) ],
                     [] );
                 Prim (0, I_SUBMIT_PROPOSALS, [], []);
                 Prim (0, I_CONS, [], []);
                 Prim (0, I_NIL, [Prim (0, T_operation, [], [])], []);
                 Prim (0, I_PAIR, [], []) ] )
    | Submit_ballot (proposal, vote) ->
        let vote_str = string ~loc @@ Vote.to_string vote in
        let proposal = protocol_hash ~loc proposal in
        left ~loc
        @@ (* [tezos-client convert data '{ DROP ; NIL baker_operation ; PUSH string "vote" ; PUSH string "PsCARTHAGazKbHtnKfLzQg3kms52kSRpgnDY982a9oYsSXRLQEb" ; SUBMIT_BALLOT ; CONS ; NIL operation ; PAIR }' from michelson to ocaml --type 'lambda unit (pair (list operation) (list baker_operation))' --zero-loc] *)
           Seq
             ( 0,
               [ Prim (0, I_DROP, [], []);
                 Prim (0, I_NIL, [Prim (0, T_baker_operation, [], [])], []);
                 Prim (0, I_PUSH, [Prim (0, T_string, [], []); vote_str], []);
                 Prim (0, I_PUSH, [Prim (0, T_string, [], []); proposal], []);
                 Prim (0, I_SUBMIT_BALLOT, [], []);
                 Prim (0, I_CONS, [], []);
                 Prim (0, I_NIL, [Prim (0, T_operation, [], [])], []);
                 Prim (0, I_PAIR, [], []) ] )
    | Set_active active ->
        let active = bool ~loc active in
        left ~loc
        @@ (* [tezos-client convert data '{ DROP ; NIL baker_operation ; PUSH bool True ; SET_BAKER_ACTIVE ; CONS ; NIL operation ; PAIR }' from michelson to ocaml --type 'lambda unit (pair (list operation) (list baker_operation))' --zero-loc] *)
           Seq
             ( 0,
               [ Prim (0, I_DROP, [], []);
                 Prim (0, I_NIL, [Prim (0, T_baker_operation, [], [])], []);
                 Prim (0, I_PUSH, [Prim (0, T_bool, [], []); active], []);
                 Prim (0, I_SET_BAKER_ACTIVE, [], []);
                 Prim (0, I_CONS, [], []);
                 Prim (0, I_NIL, [Prim (0, T_operation, [], [])], []);
                 Prim (0, I_PAIR, [], []) ] )
    | Toggle_delegations accept ->
        let accept = bool ~loc accept in
        left ~loc
        @@ (* [tezos-client convert data '{ DROP ; NIL baker_operation ; PUSH bool True ; SET_BAKER_ACCEPT_DELEGATIONS ; CONS ; NIL operation ; PAIR }' from michelson to ocaml --type 'lambda unit (pair (list operation) (list baker_operation))' --zero-loc] *)
           Seq
             ( 0,
               [ Prim (0, I_DROP, [], []);
                 Prim (0, I_NIL, [Prim (0, T_baker_operation, [], [])], []);
                 Prim (0, I_PUSH, [Prim (0, T_bool, [], []); accept], []);
                 Prim (0, I_TOGGLE_BAKER_DELEGATIONS, [], []);
                 Prim (0, I_CONS, [], []);
                 Prim (0, I_NIL, [Prim (0, T_operation, [], [])], []);
                 Prim (0, I_PAIR, [], []) ] )
    | Set_consensus_key (key, proof) ->
        let key = public_key ~loc key in
        let proof = signature ~loc proof in
        left ~loc
        @@ (* [tezos-client convert data '{ DROP ; NIL baker_operation ; PUSH signature "..." ; PUSH key "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" ; SET_BAKER_CONSENSUS_KEY ; CONS ; NIL operation ; PAIR }' from michelson to ocaml --type 'lambda unit (pair (list operation) (list baker_operation))' --zero-loc] *)
           Seq
             ( 0,
               [ Prim (0, I_DROP, [], []);
                 Prim (0, I_NIL, [Prim (0, T_baker_operation, [], [])], []);
                 Prim (0, I_PUSH, [Prim (0, T_signature, [], []); proof], []);
                 Prim (0, I_PUSH, [Prim (0, T_key, [], []); key], []);
                 Prim (0, I_SET_BAKER_CONSENSUS_KEY, [], []);
                 Prim (0, I_CONS, [], []);
                 Prim (0, I_NIL, [Prim (0, T_operation, [], [])], []);
                 Prim (0, I_PAIR, [], []) ] )
    | Set_owner_keys (threshold, keys) ->
        let keys = List.map (fun key -> public_key ~loc key) keys in
        let threshold = int ~loc threshold in
        right ~loc @@ pair ~loc threshold (seq ~loc keys)
    | Set_pvss_key key ->
        let key = pvss_public_key ~loc key in
        left ~loc
        @@ (* [tezos-client convert data '{ DROP ; NIL baker_operation ; PUSH pvss_key "GSp8PUBkYJzkg9e3EXHYeWVcC8EPnrjLMRTJmkcQ1iiyxNSXTBtcW6" ; SET_BAKER_PVSS_KEY ; CONS ; NIL operation ; PAIR }' from michelson to ocaml --type 'lambda unit (pair (list operation) (list baker_operation))' --zero-loc] *)
           Seq
             ( 0,
               [ Prim (0, I_DROP, [], []);
                 Prim (0, I_NIL, [Prim (0, T_baker_operation, [], [])], []);
                 Prim (0, I_PUSH, [Prim (0, T_pvss_key, [], []); key], []);
                 Prim (0, I_SET_BAKER_PVSS_KEY, [], []);
                 Prim (0, I_CONS, [], []);
                 Prim (0, I_NIL, [Prim (0, T_operation, [], [])], []);
                 Prim (0, I_PAIR, [], []) ] )
    | Generic lambda ->
        left ~loc (Micheline.root lambda) )

let mk_bytes_to_sign ~chain_id ~payload contract =
  (* Address of self and chain ID are used to ensure that signatures can't be
     used on different contracts or chains *)
  let address =
    bytes ~loc (Data_encoding.Binary.to_bytes_exn Contract.encoding contract)
  in
  let chain_id =
    bytes ~loc (Data_encoding.Binary.to_bytes_exn Chain_id.encoding chain_id)
  in
  (* Expected parameter structure used to sign is:
     [pair
         (pair
            chain_id
            (address @self.address))
         payload]
  *)
  let params = pair ~loc (pair ~loc chain_id address) payload in
  (* Pack bytes *)
  let bytes =
    Data_encoding.Binary.to_bytes_exn Script.expr_encoding
    @@ Micheline.strip_locations params
  in
  Bytes.concat Bytes.empty [Bytes.of_string "\005"; bytes]

let mk_singlesig_script_param ~payload ~signature =
  let signature = string ~loc @@ Signature.to_b58check signature in
  (* Expected parameter structure for [%main] entrypoint is:
     [pair payload (list (option signature))]
  *)
  let signatures = seq ~loc [some ~loc signature] in
  Micheline.strip_locations @@ pair ~loc payload signatures

let mk_multisig_script_param ~payload ~signatures =
  let signatures =
    List.map
      (function
        | Some signature ->
            some ~loc @@ string ~loc @@ Signature.to_b58check signature
        | None ->
            none ~loc ())
      signatures
  in
  (* Expected parameter structure for [%main] entrypoint is:
     [pair payload (list (option signature))]
  *)
  let signatures = seq ~loc signatures in
  Micheline.strip_locations @@ pair ~loc payload signatures

let is_singlesig_compatible (cctxt : #Protocol_client_context.full) ~chain
    ~block contract =
  Client_proto_multisig.multisig_get_information cctxt ~chain ~block contract
  >>=? fun info ->
  match info.keys with
  | [owner_key] ->
      return (info, owner_key)
  | _ ->
      fail More_than_one_key

let check_action ~action () =
  match action with
  | Set_owner_keys (threshold, keys) ->
      Client_proto_multisig.check_threshold ~threshold ~keys ()
  | _ ->
      return_unit

let call_singlesig (cctxt : #Protocol_client_context.full) ~chain ~block
    ?confirmations ?dry_run ?branch ~source ~src_pk ~src_sk ~baker ~action ?fee
    ?gas_limit ?storage_limit ?counter ~fee_parameter () =
  check_action ~action ()
  >>=? fun () ->
  let baker_contract = Contract.baker_contract baker in
  is_singlesig_compatible cctxt ~chain ~block baker_contract
  >>=? fun (info, owner_key) ->
  let payload = mk_payload ~stored_counter:info.counter ~action in
  Chain_services.chain_id cctxt ~chain ()
  >>=? fun chain_id ->
  (* Make parameters bytes, used to create a signature *)
  let bytes = mk_bytes_to_sign ~chain_id ~payload baker_contract in
  Client_keys.get_key cctxt (Signature.Public_key.hash owner_key)
  >>=? fun (_, _src_pk, owner_sk) ->
  (* Sign the parameter bytes with the owner key *)
  Client_keys.sign cctxt owner_sk bytes
  >>=? fun signature ->
  (* Turn action into transaction parameters *)
  let anon = Tezos_micheline.Micheline_printer.{comment = None} in
  let arg =
    mk_singlesig_script_param ~payload ~signature
    |> Michelson_v1_primitives.strings_of_prims
    |> Tezos_micheline.Micheline.inject_locations (fun _ -> anon)
    |> Format.asprintf "%a" Tezos_micheline.Micheline_printer.print_expr
  in
  Client_proto_context.transfer
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?branch
    ~source
    ~src_pk
    ~src_sk
    ~destination:baker_contract
    ~arg
    ~amount:Tez.zero
    ~entrypoint:generic_entrypoint
    ?fee
    ?gas_limit
    ?storage_limit
    ?counter
    ~fee_parameter
    ()

let call_multisig (cctxt : #Protocol_client_context.full) ~chain ~block
    ?confirmations ?dry_run ?branch ~source ~src_pk ~src_sk ~baker ~signatures
    ~action ?fee ?gas_limit ?storage_limit ?counter ~fee_parameter () =
  check_action ~action ()
  >>=? fun () ->
  let baker_contract = Contract.baker_contract baker in
  Client_proto_multisig.multisig_get_information
    cctxt
    ~chain
    ~block
    baker_contract
  >>=? fun info ->
  let payload = mk_payload ~stored_counter:info.counter ~action in
  Chain_services.chain_id cctxt ~chain ()
  >>=? fun chain_id ->
  (* Make parameters bytes, used to check signatures *)
  let bytes = mk_bytes_to_sign ~chain_id ~payload baker_contract in
  Client_proto_multisig.check_multisig_signatures
    ~bytes
    ~threshold:info.threshold
    ~keys:info.keys
    signatures
  >>=? fun signatures ->
  let payload = mk_payload ~stored_counter:info.counter ~action in
  (* Turn action into transaction parameters *)
  let anon = Tezos_micheline.Micheline_printer.{comment = None} in
  let arg =
    mk_multisig_script_param ~payload ~signatures
    |> Michelson_v1_primitives.strings_of_prims
    |> Tezos_micheline.Micheline.inject_locations (fun _ -> anon)
    |> Format.asprintf "%a" Tezos_micheline.Micheline_printer.print_expr
  in
  Client_proto_context.transfer
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?branch
    ~source
    ~src_pk
    ~src_sk
    ~destination:baker_contract
    ~arg
    ~amount:Tez.zero
    ~entrypoint:generic_entrypoint
    ?fee
    ?gas_limit
    ?storage_limit
    ?counter
    ~fee_parameter
    ()

let prepare_multisig_transaction (cctxt : #Protocol_client_context.full) ~chain
    ~block ~baker ~action () =
  check_action ~action ()
  >>=? fun () ->
  let baker_contract = Contract.baker_contract baker in
  Client_proto_multisig.multisig_get_information
    cctxt
    ~chain
    ~block
    baker_contract
  >>=? fun info ->
  Chain_services.chain_id cctxt ~chain ()
  >>=? fun chain_id ->
  let payload = mk_payload ~stored_counter:info.counter ~action in
  let bytes = mk_bytes_to_sign ~chain_id ~payload baker_contract in
  return
    {
      bytes;
      threshold = info.threshold;
      keys = info.keys;
      counter = info.counter;
    }
