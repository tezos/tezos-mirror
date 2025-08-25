(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs <contact@nomadic-labs.com>                *)
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
open Protocol_client_context
open Tezos_micheline

let return_single_manager_result (oph, _, op, result) =
  match Apply_results.pack_contents_list op result with
  | Apply_results.Single_and_result ((Manager_operation _ as op), result) ->
      return (oph, op, result)
  | _ -> assert false

let check_smart_contract (cctxt : #full) opt_res some =
  Option.fold ~none:(cctxt#error "This is not a smart contract.") ~some opt_res

let get_contract_manager (cctxt : #full) contract =
  let open Micheline in
  let open Michelson_v1_primitives in
  Client_proto_context.get_storage
    cctxt
    ~chain:cctxt#chain
    ~block:cctxt#block
    ~unparsing_mode:Optimized
    contract
  >>=? fun storage ->
  check_smart_contract cctxt storage @@ fun storage ->
  match root storage with
  | Prim (_, D_Pair, Bytes (_, bytes) :: _, _) | Bytes (_, bytes) -> (
      match
        Data_encoding.Binary.of_bytes_opt
          Signature.Public_key_hash.encoding
          bytes
      with
      | Some k -> return k
      | None ->
          cctxt#error
            "Cannot find a manager key in contracts storage (decoding bytes \
             failed).\n\
             Transfer from scripted contract are currently only supported for \
             \"manager\" contract.")
  | Prim (_, D_Pair, String (_, value) :: _, _) | String (_, value) -> (
      match Signature.Public_key_hash.of_b58check_opt value with
      | Some k -> return k
      | None ->
          cctxt#error
            "Cannot find a manager key in contracts storage (\"%s\" is not a \
             valid key).\n\
             Transfer from scripted contract are currently only supported for \
             \"manager\" contract."
            value)
  | _raw_storage ->
      cctxt#error
        "Cannot find a manager key in contracts storage (wrong storage format \
         : @[%a@]).\n\
         Transfer from scripted contract are currently only supported for \
         \"manager\" contract."
        Michelson_v1_printer.print_expr
        storage

let parse code =
  Lwt.return
    ( Micheline_parser.no_parsing_error
    @@ Michelson_v1_parser.parse_expression code
    >>? fun exp -> ok @@ Script.lazy_expr Michelson_v1_parser.(exp.expanded) )

let build_lambda_for_set_delegate ~delegate =
  match delegate with
  | Some delegate ->
      let (`Hex delegate) = Signature.Public_key_hash.to_hex delegate in
      Format.asprintf
        "{ DROP ; NIL operation ; PUSH key_hash 0x%s ; SOME ; SET_DELEGATE ; \
         CONS }"
        delegate
  | None -> "{ DROP ; NIL operation ; NONE key_hash ; SET_DELEGATE ; CONS }"

let entrypoint_do = Entrypoint.do_

let entrypoint_set_delegate = Entrypoint.set_delegate

let entrypoint_remove_delegate = Entrypoint.remove_delegate

let build_delegate_operation (cctxt : #full) ~chain ~block ?fee
    contract (* the KT1 to delegate *)
    (delegate : Signature.public_key_hash option) =
  let entrypoint = entrypoint_do in
  ( Michelson_v1_entrypoints.contract_entrypoint_type
      cctxt
      ~chain
      ~block
      ~contract
      ~entrypoint
      ~normalize_types:true
  >>=? function
    | Some _ ->
        (* their is a "do" entrypoint (we could check its type here)*)
        parse @@ build_lambda_for_set_delegate ~delegate >>=? fun param ->
        return (param, entrypoint)
    | None -> (
        (*  their is no "do" entrypoint trying "set/remove_delegate" *)
        let entrypoint =
          match delegate with
          | Some _ -> entrypoint_set_delegate
          | None -> entrypoint_remove_delegate
        in
        Michelson_v1_entrypoints.contract_entrypoint_type
          cctxt
          ~chain
          ~block
          ~contract
          ~entrypoint
          ~normalize_types:true
        >>=? function
        | Some _ ->
            (*  their is a "set/remove_delegate" entrypoint *)
            let delegate_data =
              match delegate with
              | Some delegate ->
                  let (`Hex delegate) =
                    Signature.Public_key_hash.to_hex delegate
                  in
                  "0x" ^ delegate
              | None -> "Unit"
            in
            parse delegate_data >>=? fun param -> return (param, entrypoint)
        | None ->
            cctxt#error
              "Cannot find a %%do or %%set_delegate entrypoint in contract@.")
  )
  >>=? fun (parameters, entrypoint) ->
  return
    (Client_proto_context.build_transaction_operation
       ~amount:Tez.zero
       ~parameters
       ~entrypoint
       ?fee
       (Originated contract))

let set_delegate (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?branch ~fee_parameter ?fee ~source ~src_pk
    ~src_sk contract (* the KT1 to delegate *)
    (delegate : Signature.public_key_hash option) =
  build_delegate_operation cctxt ~chain ~block ?fee contract delegate
  >>=? fun operation ->
  let operation = Annotated_manager_operation.Single_manager operation in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ?branch
    ~source
    ~fee:(Limit.of_option fee)
    ~gas_limit:Limit.unknown
    ~storage_limit:(Limit.known Z.zero)
    ~src_pk
    ~src_sk
    ~fee_parameter
    operation
  >>=? return_single_manager_result

let d_unit =
  Micheline.strip_locations (Prim (0, Michelson_v1_primitives.D_Unit, [], []))

let t_unit =
  Micheline.strip_locations (Prim (0, Michelson_v1_primitives.T_unit, [], []))

let build_lambda_for_transfer_to_implicit ~destination ~amount =
  let (`Hex destination) = Signature.Public_key_hash.to_hex destination in
  Format.asprintf
    "{ DROP ; NIL operation ;PUSH key_hash 0x%s; IMPLICIT_ACCOUNT;PUSH mutez \
     %Ld ;UNIT;TRANSFER_TOKENS ; CONS }"
    destination
    (Tez.to_mutez amount)

let build_lambda_for_transfer_to_originated ~destination ~entrypoint ~amount
    ~parameter_type ~parameter =
  let destination =
    Data_encoding.Binary.to_bytes_exn Contract.originated_encoding destination
  in
  let amount = Tez.to_mutez amount in
  let (`Hex destination) = Hex.of_bytes destination in
  let entrypoint = Entrypoint.to_address_suffix entrypoint in
  if parameter_type = t_unit then
    Format.asprintf
      "{ DROP ; NIL operation ;PUSH address 0x%s; CONTRACT %s %a; \
       ASSERT_SOME;PUSH mutez %Ld ;UNIT;TRANSFER_TOKENS ; CONS }"
      destination
      entrypoint
      Michelson_v1_printer.print_expr
      parameter_type
      amount
  else
    Format.asprintf
      "{ DROP ; NIL operation ;PUSH address 0x%s; CONTRACT %s %a; \
       ASSERT_SOME;PUSH mutez %Ld ;PUSH %a %a;TRANSFER_TOKENS ; CONS }"
      destination
      entrypoint
      Michelson_v1_printer.print_expr
      parameter_type
      amount
      Michelson_v1_printer.print_expr
      parameter_type
      Michelson_v1_printer.print_expr
      parameter

let build_transaction_operation (cctxt : #full) ~chain ~block ~contract
    ~(destination : Contract.t) ?(entrypoint = Entrypoint.default) ?arg ~amount
    ?fee ?gas_limit ?storage_limit () =
  (match destination with
  | Implicit destination when Entrypoint.is_default entrypoint ->
      return @@ build_lambda_for_transfer_to_implicit ~destination ~amount
  | Implicit _ ->
      cctxt#error
        "Implicit accounts have no entrypoints. (targeted entrypoint %%%a on \
         contract %a)"
        Entrypoint.pp
        entrypoint
        Contract.pp
        destination
  | Originated destination ->
      ( Michelson_v1_entrypoints.contract_entrypoint_type
          cctxt
          ~chain
          ~block
          ~contract:destination
          ~entrypoint
          ~normalize_types:true
      >>=? function
        | None ->
            cctxt#error
              "Contract %a has no entrypoint named %a"
              Contract_hash.pp
              destination
              Entrypoint.pp
              entrypoint
        | Some parameter_type -> return parameter_type )
      >>=? fun parameter_type ->
      (match arg with
      | Some arg ->
          Lwt.return @@ Micheline_parser.no_parsing_error
          @@ Michelson_v1_parser.parse_expression arg
          >>=? fun {expanded = arg; _} -> return_some arg
      | None -> return_none)
      >>=? fun parameter ->
      let parameter = Option.value ~default:d_unit parameter in
      return
      @@ build_lambda_for_transfer_to_originated
           ~destination
           ~entrypoint
           ~amount
           ~parameter_type
           ~parameter)
  >>=? fun lambda ->
  parse lambda >>=? fun parameters ->
  let entrypoint = entrypoint_do in
  return
    (Client_proto_context.build_transaction_operation
       ~amount:Tez.zero
       ~parameters
       ~entrypoint
       ?fee
       ?gas_limit
       ?storage_limit
       contract)

let transfer (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?simulation ?(force = false) ?branch ~source ~src_pk
    ~src_sk ~contract ~destination ?(entrypoint = Entrypoint.default) ?arg
    ~amount ?fee ?gas_limit ?safety_guard ?storage_limit ?counter ~fee_parameter
    () :
    (Kind.transaction Kind.manager Injection.result * Contract_hash.t list)
    tzresult
    Lwt.t =
  build_transaction_operation
    cctxt
    ~chain
    ~block
    ~contract
    ~destination
    ~entrypoint
    ?arg
    ~amount
    ?fee
    ?gas_limit
    ?storage_limit
    ()
  >>=? fun operation ->
  let operation = Annotated_manager_operation.Single_manager operation in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?simulation
    ~force
    ?branch
    ~source
    ~fee:(Limit.of_option fee)
    ~gas_limit:(Limit.of_option gas_limit)
    ~storage_limit:(Limit.of_option storage_limit)
    ?safety_guard
    ?counter
    ~src_pk
    ~src_sk
    ~fee_parameter
    operation
  >>=? fun ((_, _, _, result) as res) ->
  Lwt.return (Injection.originated_contracts ~force result)
  >>=? fun contracts ->
  return_single_manager_result res >>=? fun res -> return (res, contracts)
