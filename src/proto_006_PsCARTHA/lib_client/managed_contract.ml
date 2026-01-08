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
open Client_proto_context

let get_contract_manager (cctxt : #full) contract =
  let open Micheline in
  let open Michelson_v1_primitives in
  get_storage cctxt ~chain:cctxt#chain ~block:cctxt#block contract >>=? function
  | None -> cctxt#error "This is not a smart contract."
  | Some storage -> (
      match root storage with
      | Prim (_, D_Pair, [Bytes (_, bytes); _], _) | Bytes (_, bytes) -> (
          match
            Data_encoding.Binary.of_bytes_opt
              Tezos_crypto.Signature.V0.Public_key_hash.encoding
              bytes
          with
          | Some k -> return k
          | None ->
              cctxt#error
                "Cannot find a manager key in contracts storage (decoding \
                 bytes failed).\n\
                 Transfer from scripted contract are currently only supported \
                 for \"manager\" contract.")
      | Prim (_, D_Pair, [String (_, value); _], _) | String (_, value) -> (
          match
            Tezos_crypto.Signature.V0.Public_key_hash.of_b58check_opt value
          with
          | Some k -> return k
          | None ->
              cctxt#error
                "Cannot find a manager key in contracts storage (\"%s\" is not \
                 a valid key).\n\
                 Transfer from scripted contract are currently only supported \
                 for \"manager\" contract."
                value)
      | _raw_storage ->
          cctxt#error
            "Cannot find a manager key in contracts storage (wrong storage \
             format : @[%a@]).\n\
             Transfer from scripted contract are currently only supported for \
             \"manager\" contract."
            Michelson_v1_printer.print_expr
            storage)

let parse code =
  Lwt.return
    ( Micheline_parser.no_parsing_error
    @@ Michelson_v1_parser.parse_expression code
    >>? fun exp -> ok @@ Script.lazy_expr Michelson_v1_parser.(exp.expanded) )

let set_delegate (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?branch ~fee_parameter ?fee ~source ~src_pk ~src_sk
    contract (* the KT1 to delegate *)
    (delegate : Tezos_crypto.Signature.V0.public_key_hash option) =
  let entrypoint = "do" in
  ( Michelson_v1_entrypoints.contract_entrypoint_type
      cctxt
      ~chain
      ~block
      ~contract
      ~entrypoint
  >>=? function
    | Some _ ->
        (* their is a "do" entrypoint (we could check its type here)*)
        let lambda =
          match delegate with
          | Some delegate ->
              let (`Hex delegate) =
                Tezos_crypto.Signature.V0.Public_key_hash.to_hex delegate
              in
              Format.asprintf
                "{ DROP ; NIL operation ; PUSH key_hash 0x%s ; SOME ; \
                 SET_DELEGATE ; CONS }"
                delegate
          | None ->
              "{ DROP ; NIL operation ; NONE key_hash ; SET_DELEGATE ; CONS }"
        in
        parse lambda >>=? fun param -> return (param, entrypoint)
    | None -> (
        (*  their is no "do" entrypoint trying "set/remove_delegate" *)
        let entrypoint =
          match delegate with
          | Some _ -> "set_delegate"
          | None -> "remove_delegate"
        in
        Michelson_v1_entrypoints.contract_entrypoint_type
          cctxt
          ~chain
          ~block
          ~contract
          ~entrypoint
        >>=? function
        | Some _ ->
            (*  their is a "set/remove_delegate" entrypoint *)
            let delegate_data =
              match delegate with
              | Some delegate ->
                  let (`Hex delegate) =
                    Tezos_crypto.Signature.V0.Public_key_hash.to_hex delegate
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
  let operation =
    Transaction
      {amount = Tez.zero; parameters; entrypoint; destination = contract}
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?branch
    ~source
    ?fee
    ~storage_limit:Z.zero
    ~src_pk
    ~src_sk
    ~fee_parameter
    operation
  >>=? fun res -> return res

let d_unit =
  Micheline.strip_locations (Prim (0, Michelson_v1_primitives.D_Unit, [], []))

let t_unit =
  Micheline.strip_locations (Prim (0, Michelson_v1_primitives.T_unit, [], []))

let build_lambda_for_implicit ~delegate ~amount =
  let (`Hex delegate) =
    Tezos_crypto.Signature.V0.Public_key_hash.to_hex delegate
  in
  Format.asprintf
    "{ DROP ; NIL operation ;PUSH key_hash 0x%s; IMPLICIT_ACCOUNT;PUSH mutez \
     %Ld ;UNIT;TRANSFER_TOKENS ; CONS }"
    delegate
    (Tez.to_mutez amount)

let build_lambda_for_originated ~destination ~entrypoint ~amount ~parameter_type
    ~parameter =
  let destination =
    Data_encoding.Binary.to_bytes_exn Contract.encoding destination
  in
  let amount = Tez.to_mutez amount in
  let (`Hex destination) = Hex.of_bytes destination in
  let entrypoint = match entrypoint with "default" -> "" | s -> "%" ^ s in
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

let transfer (cctxt : #full) ~chain ~block ?confirmations ?dry_run
    ?verbose_signing ?branch ~source ~src_pk ~src_sk ~contract ~destination
    ?(entrypoint = "default") ?arg ~amount ?fee ?gas_limit ?storage_limit
    ?counter ~fee_parameter () :
    (Kind.transaction Kind.manager Injection.result * Contract.t list) tzresult
    Lwt.t =
  (match Alpha_context.Contract.is_implicit destination with
  | Some delegate when entrypoint = "default" ->
      return @@ build_lambda_for_implicit ~delegate ~amount
  | Some _ ->
      cctxt#error
        "Implicit accounts have no entrypoints. (targeted entrypoint %%%s on \
         contract %a)"
        entrypoint
        Contract.pp
        destination
  | None ->
      ( Michelson_v1_entrypoints.contract_entrypoint_type
          cctxt
          ~chain
          ~block
          ~contract:destination
          ~entrypoint
      >>=? function
        | None ->
            cctxt#error
              "Contract %a has no entrypoint named %s"
              Contract.pp
              destination
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
      @@ build_lambda_for_originated
           ~destination
           ~entrypoint
           ~amount
           ~parameter_type
           ~parameter)
  >>=? fun lambda ->
  parse lambda >>=? fun parameters ->
  let entrypoint = "do" in
  let operation =
    Transaction
      {amount = Tez.zero; parameters; entrypoint; destination = contract}
  in
  Injection.inject_manager_operation
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?verbose_signing
    ?branch
    ~source
    ?fee
    ?gas_limit
    ?storage_limit
    ?counter
    ~src_pk
    ~src_sk
    ~fee_parameter
    operation
  >>=? fun ((_oph, _op, result) as res) ->
  Lwt.return (Injection.originated_contracts (Single_result result))
  >>=? fun contracts -> return (res, contracts)
