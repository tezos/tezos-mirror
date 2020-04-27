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

(* ------------------------------------------------------------------------- *)
(* Mockup protocol parameters *)

type mockup_protocol_parameters = {
  initial_timestamp : Time.Protocol.t;
  bootstrap_accounts : Protocol.Parameters_repr.bootstrap_account list;
  bootstrap_contracts : Protocol.Parameters_repr.bootstrap_contract list;
  constants : Protocol.Constants_repr.parametric;
}

type protocol_constants_overrides = {
  hard_gas_limit_per_operation : Z.t option;
  hard_gas_limit_per_block : Z.t option;
  hard_storage_limit_per_operation : Z.t option;
  cost_per_byte : Protocol.Tez_repr.t option;
  chain_id : Chain_id.t option;
}

type parsed_account_repr = {
  name : string;
  sk_uri : Client_keys.sk_uri;
  amount : Protocol.Tez_repr.t;
}

let parsed_account_repr_pp ppf account =
  let open Format in
  let format_amount ppf value =
    fprintf ppf "amount:%a" Protocol.Tez_repr.pp value
  in
  fprintf
    ppf
    "@[<v>name:%s@,sk_uri:%s@,%a@]"
    account.name
    (Uri.to_string (account.sk_uri :> Uri.t))
    format_amount
    account.amount

let bootstrap_account_encoding :
    Protocol.Parameters_repr.bootstrap_account Data_encoding.t =
  let open Data_encoding in
  let open Protocol.Parameters_repr in
  conv
    (fun {public_key_hash; public_key; amount} ->
      (public_key_hash, public_key, amount))
    (fun (public_key_hash, public_key, amount) ->
      {public_key_hash; public_key; amount})
    (obj3
       (req "public_key_hash" Signature.Public_key_hash.encoding)
       (opt "public_key" Signature.Public_key.encoding)
       (req "amount" Protocol.Tez_repr.encoding))

let bootstrap_contract_encoding :
    Protocol.Parameters_repr.bootstrap_contract Data_encoding.t =
  let open Data_encoding in
  let open Protocol.Parameters_repr in
  conv
    (fun {delegate; amount; script} -> (delegate, amount, script))
    (fun (delegate, amount, script) -> {delegate; amount; script})
    (obj3
       (req "delegate" Signature.Public_key_hash.encoding)
       (req "amount" Protocol.Tez_repr.encoding)
       (req "script" Protocol.Script_repr.encoding))

let mockup_protocol_parameters_encoding :
    mockup_protocol_parameters Data_encoding.t =
  let open Data_encoding in
  conv
    (fun p ->
      ( p.initial_timestamp,
        p.bootstrap_accounts,
        p.bootstrap_contracts,
        p.constants ))
    (fun (initial_timestamp, bootstrap_accounts, bootstrap_contracts, constants)
         ->
      {initial_timestamp; bootstrap_accounts; bootstrap_contracts; constants})
    (obj4
       (req "initial_timestamp" Time.Protocol.encoding)
       (req "bootstrap_accounts" (list bootstrap_account_encoding))
       (req "bootstrap_contracts" (list bootstrap_contract_encoding))
       (req "constants" Protocol.Constants_repr.parametric_encoding))

let protocol_constants_overrides_encoding =
  let open Data_encoding in
  conv
    (fun p ->
      ( p.hard_gas_limit_per_operation,
        p.hard_gas_limit_per_block,
        p.hard_storage_limit_per_operation,
        p.cost_per_byte,
        p.chain_id ))
    (fun ( hard_gas_limit_per_operation,
           hard_gas_limit_per_block,
           hard_storage_limit_per_operation,
           cost_per_byte,
           chain_id ) ->
      {
        hard_gas_limit_per_operation;
        hard_gas_limit_per_block;
        hard_storage_limit_per_operation;
        cost_per_byte;
        chain_id;
      })
    (obj5
       (opt "hard_gas_limit_per_operation" z)
       (opt "hard_gas_limit_per_block" z)
       (opt "hard_storage_limit_per_operation" z)
       (opt "cost_per_byte" Protocol.Tez_repr.encoding)
       (opt "chain_id" Chain_id.encoding))

let default_mockup_parameters : mockup_protocol_parameters =
  let parameters =
    Default_parameters.parameters_of_constants
      Default_parameters.constants_sandbox
  in
  {
    initial_timestamp = Time.Protocol.epoch;
    bootstrap_accounts = parameters.bootstrap_accounts;
    bootstrap_contracts = parameters.bootstrap_contracts;
    constants = parameters.constants;
  }

let default_mockup_protocol_constants : protocol_constants_overrides =
  let default = default_mockup_parameters in
  {
    hard_gas_limit_per_operation =
      Some default.constants.hard_gas_limit_per_operation;
    hard_gas_limit_per_block = Some default.constants.hard_gas_limit_per_block;
    hard_storage_limit_per_operation =
      Some default.constants.hard_storage_limit_per_operation;
    cost_per_byte = Some default.constants.cost_per_byte;
    chain_id = Some Tezos_mockup_registration.Mockup_args.Chain_id.dummy;
  }

(* Use the wallet to convert a bootstrap account's public key
  into a parsed_account_repr secret key Uri *)
let bootstrap_account_to_parsed_account_repr cctxt
    (bootstrap_account : Protocol.Parameters_repr.bootstrap_account) =
  Client_keys.get_key cctxt bootstrap_account.public_key_hash
  >>=? fun (name, _, sk_uri) ->
  return {name; sk_uri; amount = bootstrap_account.amount}

let parsed_account_repr_encoding =
  let open Data_encoding in
  conv
    (fun p -> (p.name, p.sk_uri, p.amount))
    (fun (name, sk_uri, amount) -> {name; sk_uri; amount})
    (obj3
       (req "name" string)
       (req "sk_uri" Client_keys.Secret_key.encoding)
       (req "amount" Protocol.Tez_repr.encoding))

let mockup_default_bootstrap_accounts
    (cctxt : Tezos_client_base.Client_context.full) : string tzresult Lwt.t =
  let rpc_context = new Protocol_client_context.wrap_full cctxt in
  let wallet = (cctxt :> Client_context.wallet) in
  let parsed_account_reprs = ref [] in
  let errors = ref [] in
  Client_keys.list_keys wallet
  >>=? fun all_keys ->
  Lwt_list.iter_s
    (function
      | (name, pkh, _pk_opt, Some sk_uri) -> (
          let contract =
            Protocol.Alpha_context.Contract.implicit_contract pkh
          in
          Client_proto_context.get_balance
            rpc_context
            ~chain:cctxt#chain
            ~block:cctxt#block
            contract
          >>= fun tz_balance ->
          match tz_balance with
          | Ok balance -> (
              let tez_repr =
                Protocol.Tez_repr.of_mutez
                @@ Protocol.Alpha_context.Tez.to_mutez balance
              in
              match tez_repr with
              | None ->
                  (* we're reading the wallet, it's content MUST be valid *)
                  assert false
              | Some amount ->
                  parsed_account_reprs :=
                    {name; sk_uri; amount} :: !parsed_account_reprs ;
                  Lwt.return_unit )
          | Error err ->
              errors := err :: !errors ;
              Lwt.return_unit )
      | _ ->
          Lwt.return_unit)
    all_keys
  >>= fun () ->
  match !errors with
  | [] ->
      let json =
        Data_encoding.Json.construct
          (Data_encoding.list parsed_account_repr_encoding)
          !parsed_account_reprs
      in
      return @@ Data_encoding.Json.to_string json
  | errs ->
      Lwt.return_error @@ List.concat errs

let protocol_constants_no_overrides =
  {
    hard_gas_limit_per_operation = None;
    hard_gas_limit_per_block = None;
    hard_storage_limit_per_operation = None;
    cost_per_byte = None;
    chain_id = None;
  }

let apply_protocol_overrides (cctxt : Tezos_client_base.Client_context.full)
    (o : protocol_constants_overrides) (c : Protocol.Constants_repr.parametric)
    =
  let has_custom =
    Option.is_some o.hard_gas_limit_per_operation
    || Option.is_some o.hard_gas_limit_per_block
    || Option.is_some o.hard_storage_limit_per_operation
    || Option.is_some o.cost_per_byte
  in
  ( if has_custom then
    let pp_opt_custom name pp ppf opt_value =
      match opt_value with
      | None ->
          ()
      | Some value ->
          Format.fprintf ppf "@[<h>%s: %a@]@," name pp value
    in
    cctxt#message
      "@[<v>mockup client uses protocol overrides:@,%a%a%a%a@]@?"
      (pp_opt_custom "hard_gas_limit_per_operation" Z.pp_print)
      o.hard_gas_limit_per_operation
      (pp_opt_custom "hard_gas_limit_per_block" Z.pp_print)
      o.hard_gas_limit_per_block
      (pp_opt_custom "hard_storage_limit_per_operation" Z.pp_print)
      o.hard_storage_limit_per_operation
      (pp_opt_custom "cost_per_byte" Protocol.Tez_repr.pp)
      o.cost_per_byte
  else Lwt.return_unit )
  >>= fun () ->
  return
    {
      c with
      hard_gas_limit_per_operation =
        Option.value
          ~default:c.hard_gas_limit_per_operation
          o.hard_gas_limit_per_operation;
      hard_gas_limit_per_block =
        Option.value
          ~default:c.hard_gas_limit_per_block
          o.hard_gas_limit_per_block;
      hard_storage_limit_per_operation =
        Option.value
          ~default:c.hard_storage_limit_per_operation
          o.hard_storage_limit_per_operation;
      cost_per_byte = Option.value ~default:c.cost_per_byte o.cost_per_byte;
    }

let to_bootstrap_account repr =
  Tezos_client_base.Client_keys.neuterize repr.sk_uri
  >>=? fun pk_uri ->
  Tezos_client_base.Client_keys.public_key pk_uri
  >>=? fun public_key ->
  let public_key_hash = Signature.Public_key.hash public_key in
  return
    Protocol.Parameters_repr.
      {public_key_hash; public_key = Some public_key; amount = repr.amount}

(* ------------------------------------------------------------------------- *)
(* Blocks *)

type block = {
  hash : Block_hash.t;
  header : Protocol.Alpha_context.Block_header.t;
  operations : Protocol.Alpha_context.Operation.packed list;
  context : Protocol.Environment.Context.t;
}

let block_encoding : block Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {hash; header; operations; context} ->
      (hash, header, operations, context))
    (fun (hash, header, operations, context) ->
      {hash; header; operations; context})
    (obj4
       (req "hash" Block_hash.encoding)
       (req "header" Protocol.Alpha_context.Block_header.encoding)
       (req
          "operations"
          (list (dynamic_size Protocol.Alpha_context.Operation.encoding)))
       (req "context" Memory_context.encoding))

module Forge = struct
  let default_proof_of_work_nonce =
    Bytes.create Protocol.Alpha_context.Constants.proof_of_work_nonce_size

  let make_shell ~level ~predecessor ~timestamp ~fitness ~operations_hash =
    Tezos_base.Block_header.
      {
        level;
        predecessor;
        timestamp;
        fitness;
        operations_hash;
        proto_level = 0;
        validation_passes = 0;
        context = Context_hash.zero;
      }
end

(* ------------------------------------------------------------------------- *)
(* RPC context *)

let initial_context (header : Block_header.shell_header)
    (params : mockup_protocol_parameters) =
  let parameters =
    Default_parameters.parameters_of_constants
      ~bootstrap_accounts:params.bootstrap_accounts
      ~bootstrap_contracts:params.bootstrap_contracts
      ~with_commitments:false
      params.constants
  in
  let json = Default_parameters.json_of_parameters parameters in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  Tezos_protocol_environment.Context.(
    let empty = Memory_context.empty in
    set empty ["version"] (Bytes.of_string "genesis")
    >>= fun ctxt -> set ctxt ["protocol_parameters"] proto_params)
  >>= fun ctxt ->
  Protocol.Main.init ctxt header
  >|= Protocol.Environment.wrap_error
  >>=? fun {context; _} -> return context

let mem_init :
    cctxt:Tezos_client_base.Client_context.full ->
    parameters:mockup_protocol_parameters ->
    chain_id:Chain_id.t option ->
    constants_overrides_json:Data_encoding.json option ->
    bootstrap_accounts_json:Data_encoding.json option ->
    (Chain_id.t * Tezos_protocol_environment.rpc_context) tzresult Lwt.t =
 fun ~cctxt
     ~parameters
     ~chain_id
     ~constants_overrides_json
     ~bootstrap_accounts_json ->
  let hash =
    Block_hash.of_b58check_exn
      "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU"
  in
  let shell =
    Forge.make_shell
      ~level:0l
      ~predecessor:hash
      ~timestamp:parameters.initial_timestamp
      ~fitness:(Protocol.Fitness_repr.from_int64 0L)
      ~operations_hash:Operation_list_list_hash.zero
  in
  ( match constants_overrides_json with
  | None ->
      return protocol_constants_no_overrides
  | Some json -> (
    match
      Data_encoding.Json.destruct protocol_constants_overrides_encoding json
    with
    | x ->
        return x
    | exception error ->
        failwith
          "cannot read protocol constants overrides: %a"
          (Data_encoding.Json.print_error ?print_unknown:None)
          error ) )
  >>=? fun protocol_overrides ->
  apply_protocol_overrides cctxt protocol_overrides parameters.constants
  >>=? fun protocol_custom ->
  ( match bootstrap_accounts_json with
  | None ->
      return None
  | Some json -> (
    match
      Data_encoding.Json.destruct
        (Data_encoding.list parsed_account_repr_encoding)
        json
    with
    | accounts ->
        cctxt#message "@[<h>mockup client uses custom bootstrap accounts:@]"
        >>= fun () ->
        let open Format in
        cctxt#message
          "@[%a@]"
          (pp_print_list
             ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
             parsed_account_repr_pp)
          accounts
        >>= fun () ->
        Tezos_base.TzPervasives.map_s to_bootstrap_account accounts
        >>=? fun bootstrap_accounts -> return (Some bootstrap_accounts)
    | exception error ->
        failwith
          "cannot read definitions of bootstrap accounts: %a"
          (Data_encoding.Json.print_error ?print_unknown:None)
          error ) )
  >>=? fun bootstrap_accounts_custom ->
  initial_context
    shell
    {
      parameters with
      bootstrap_accounts =
        Option.value
          ~default:parameters.bootstrap_accounts
          bootstrap_accounts_custom;
      constants = protocol_custom;
    }
  >>=? fun context ->
  let chain_id =
    Tezos_mockup_registration.Mockup_args.Chain_id.choose
      ~from_command_line:chain_id
      ~from_config_file:protocol_overrides.chain_id
  in
  return
    ( chain_id,
      {
        Tezos_protocol_environment.block_hash = hash;
        block_header = shell;
        context;
      } )

(* ------------------------------------------------------------------------- *)
(* Register mockup *)

let () =
  let open Tezos_mockup_registration.Registration in
  let module M : Mockup_sig = struct
    type parameters = mockup_protocol_parameters

    type protocol_constants = protocol_constants_overrides

    let parameters_encoding = mockup_protocol_parameters_encoding

    let protocol_constants_encoding = protocol_constants_overrides_encoding

    let default_bootstrap_accounts = mockup_default_bootstrap_accounts

    let default_parameters = default_mockup_parameters

    let default_protocol_constants = default_mockup_protocol_constants

    let protocol_hash = Protocol.hash

    module Protocol = Protocol_client_context.Lifted_protocol
    module Block_services = Protocol_client_context.Alpha_block_services

    let directory = Protocol.rpc_services

    let init = mem_init
  end in
  register_mockup_environment (module M)
