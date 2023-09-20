(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

(* ------------------------------------------------------------------------- *)
(* Mockup protocol parameters *)

(** Protocol constants overriding logic. *)
module Protocol_constants_overrides = struct
  (** Equivalent of [Constants.parametric] with additionally [chain_id] and [timestamp]. *)
  type t = {
    parametric : Constants.Parametric.t;
    chain_id : Chain_id.t option;
    timestamp : Time.Protocol.t option;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {parametric; chain_id; timestamp} ->
        (parametric, (chain_id, timestamp)))
      (fun (parametric, (chain_id, timestamp)) ->
        {parametric; chain_id; timestamp})
      (merge_objs
         Constants.Parametric.encoding
         (obj2
            (opt "chain_id" Chain_id.encoding)
            (opt "initial_timestamp" Time.Protocol.encoding)))

  let default_value (cctxt : Tezos_client_base.Client_context.full) :
      t tzresult Lwt.t =
    let open Lwt_result_syntax in
    let cpctxt = new Protocol_client_context.wrap_full cctxt in
    let* {parametric; _} =
      Protocol.Constants_services.all cpctxt (cpctxt#chain, cpctxt#block)
    in
    let to_chain_id_opt = function `Hash c -> Some c | _ -> None in
    let* header =
      Shell_services.Blocks.Header.shell_header
        cpctxt
        ~chain:cpctxt#chain
        ~block:cpctxt#block
        ()
    in
    return
      {
        parametric;
        chain_id = to_chain_id_opt cpctxt#chain;
        timestamp = Some header.timestamp;
      }
end

module Parsed_account = struct
  type t = {name : string; sk_uri : Client_keys.sk_uri; amount : Tez.t}

  let pp ppf account =
    let open Format in
    let format_amount ppf value = fprintf ppf "amount:%a" Tez.pp value in
    fprintf
      ppf
      "@[<v>name:%s@,sk_uri:%s@,%a@]"
      account.name
      (Uri.to_string (account.sk_uri :> Uri.t))
      format_amount
      account.amount

  let encoding =
    let open Data_encoding in
    conv
      (fun p -> (p.name, p.sk_uri, p.amount))
      (fun (name, sk_uri, amount) -> {name; sk_uri; amount})
      (obj3
         (req "name" string)
         (req "sk_uri" Client_keys.Secret_key.encoding)
         (req "amount" Tez.encoding))

  let to_bootstrap_account repr =
    let open Lwt_result_syntax in
    let* pk_uri = Tezos_client_base.Client_keys.neuterize repr.sk_uri in
    let* public_key = Tezos_client_base.Client_keys.public_key pk_uri in
    let public_key_hash = Signature.Public_key.hash public_key in
    return
      Parameters.
        {
          public_key_hash;
          public_key = Some public_key;
          amount = repr.amount;
          delegate_to = None;
          consensus_key = None;
        }

  let default_to_json (cctxt : Tezos_client_base.Client_context.full) :
      string tzresult Lwt.t =
    let open Lwt_result_syntax in
    let rpc_context = new Protocol_client_context.wrap_full cctxt in
    let wallet = (cctxt :> Client_context.wallet) in
    let parsed_account_reprs = ref [] in
    let errors = ref [] in
    let* all_keys = Client_keys.list_keys wallet in
    let*! () =
      List.iter_s
        (function
          | name, pkh, _pk_opt, Some sk_uri -> (
              let contract = Contract.Implicit pkh in
              let*! tz_balance =
                Client_proto_context.get_balance
                  rpc_context
                  ~chain:cctxt#chain
                  ~block:cctxt#block
                  contract
              in
              match tz_balance with
              | Ok balance -> (
                  let tez_repr = Tez.of_mutez @@ Tez.to_mutez balance in
                  match tez_repr with
                  | None ->
                      (* we're reading the wallet, it's content MUST be valid *)
                      assert false
                  | Some amount ->
                      parsed_account_reprs :=
                        {name; sk_uri; amount} :: !parsed_account_reprs ;
                      Lwt.return_unit)
              | Error err ->
                  errors := err :: !errors ;
                  Lwt.return_unit)
          | _ -> Lwt.return_unit)
        all_keys
    in
    match !errors with
    | [] ->
        let json =
          Data_encoding.Json.construct
            (Data_encoding.list encoding)
            !parsed_account_reprs
        in
        return @@ Data_encoding.Json.to_string json
    | errs -> Lwt.return_error @@ List.concat errs
end

module Bootstrap_account = struct
  let encoding : Parameters.bootstrap_account Data_encoding.t =
    let open Data_encoding in
    let open Parameters in
    conv
      (fun {public_key_hash; public_key; amount; delegate_to; consensus_key} ->
        (public_key_hash, public_key, amount, delegate_to, consensus_key))
      (fun (public_key_hash, public_key, amount, delegate_to, consensus_key) ->
        {public_key_hash; public_key; amount; delegate_to; consensus_key})
      (obj5
         (req "public_key_hash" Signature.Public_key_hash.encoding)
         (opt "public_key" Signature.Public_key.encoding)
         (req "amount" Tez.encoding)
         (opt "delegate_to" Signature.Public_key_hash.encoding)
         (opt "consensus_key" Signature.Public_key.encoding))
end

module Bootstrap_contract = struct
  let encoding : Parameters.bootstrap_contract Data_encoding.t =
    let open Data_encoding in
    let open Parameters in
    conv
      (fun {delegate; amount; script; hash} -> (delegate, amount, script, hash))
      (fun (delegate, amount, script, hash) -> {delegate; amount; script; hash})
      (obj4
         (opt "delegate" Signature.Public_key_hash.encoding)
         (req "amount" Tez.encoding)
         (req "script" Script.encoding)
         (opt "hash" Contract_hash.encoding))
end

module Protocol_parameters = struct
  type t = {
    initial_timestamp : Time.Protocol.t;
    bootstrap_accounts : Parameters.bootstrap_account list;
    bootstrap_contracts : Parameters.bootstrap_contract list;
    constants : Constants.Parametric.t;
  }

  let encoding : t Data_encoding.t =
    let open Data_encoding in
    conv
      (fun p ->
        ( p.initial_timestamp,
          p.bootstrap_accounts,
          p.bootstrap_contracts,
          p.constants ))
      (fun ( initial_timestamp,
             bootstrap_accounts,
             bootstrap_contracts,
             constants ) ->
        {initial_timestamp; bootstrap_accounts; bootstrap_contracts; constants})
      (obj4
         (req "initial_timestamp" Time.Protocol.encoding)
         (req "bootstrap_accounts" (list Bootstrap_account.encoding))
         (req "bootstrap_contracts" (list Bootstrap_contract.encoding))
         (req "constants" Constants.Parametric.encoding))

  let default_value : t =
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
end

(* This encoding extends [Protocol_constants_overrides.encoding] to allow
   reading json files as produced by lib_parameters. *)
let lib_parameters_json_encoding =
  Data_encoding.(
    merge_objs
      (obj2
         (opt "bootstrap_accounts" (list Parameters.bootstrap_account_encoding))
         (opt "commitments" (list Commitment.encoding)))
      Protocol_constants_overrides.encoding)

(* ------------------------------------------------------------------------- *)
(* Blocks *)

type block = {
  hash : Block_hash.t;
  header : Block_header.t;
  operations : Operation.packed list;
  context : Environment.Context.t;
}

module Forge = struct
  let default_proof_of_work_nonce =
    Bytes.create Constants.proof_of_work_nonce_size

  let make_shell ~level ~predecessor ~timestamp ~fitness ~operations_hash =
    (* We initialize the [proto_level] at 1 in order to be able to
       mimick a transition block in the baker. The baker distinguishes
       the first block of a protocol by comparing a block and its
       predecessor's proto level. If there is a difference, it must
       mean that the block is a transition one. If we start at 0, we
       cannot "hack" a transition block by decrementing the genesis
       predecessor's protocol level because protocol levels are
       encoded as uint8. *)
    let proto_level = 1 in
    Tezos_base.Block_header.
      {
        level;
        predecessor;
        timestamp;
        fitness;
        operations_hash;
        proto_level;
        validation_passes = 0;
        context = Context_hash.zero;
      }
end

(* ------------------------------------------------------------------------- *)
(* RPC context *)
let genesis_block_hash =
  Block_hash.of_b58check_exn
    "BLockGenesisGenesisGenesisGenesisGenesisCCCCCeZiLHU"

let attestation_branch_data_encoding =
  let open Data_encoding in
  conv
    (fun (block_hash, block_payload_hash) -> (block_hash, block_payload_hash))
    (fun (block_hash, block_payload_hash) -> (block_hash, block_payload_hash))
    (obj2
       (req "block_hash" Block_hash.encoding)
       (req "block_payload_hash" Protocol.Block_payload_hash.encoding))

let initial_context chain_id (header : Block_header.shell_header)
    ({bootstrap_accounts; bootstrap_contracts; constants; _} :
      Protocol_parameters.t) =
  let open Lwt_result_syntax in
  let parameters =
    Default_parameters.parameters_of_constants
      ~bootstrap_accounts
      ~bootstrap_contracts
      ~commitments:[]
      constants
  in
  let json = Default_parameters.json_of_parameters parameters in
  let proto_params =
    Data_encoding.Binary.to_bytes_exn Data_encoding.json json
  in
  let*! ctxt =
    Tezos_protocol_environment.Context.(
      let empty = Tezos_protocol_environment.Memory_context.empty in
      let*! ctxt = add empty ["version"] (Bytes.of_string "genesis") in
      add ctxt ["protocol_parameters"] proto_params)
  in
  let*! ctxt = Environment.Updater.activate ctxt Protocol.hash in
  let* {context; _} =
    let*! r = Protocol.Main.init chain_id ctxt header in
    Lwt.return (Environment.wrap_tzresult r)
  in
  let ({
         timestamp = predecessor_timestamp;
         level = predecessor_level;
         fitness = predecessor_fitness;
         _;
       }
        : Block_header.shell_header) =
    header
  in
  let timestamp = Time.System.to_protocol (Time.System.now ()) in
  (*

     We need to forge a predecessor hash to pass it to [value_of_key].
     This initial context is used for RPC, hence this piece of
     information is not important and does not have to be meaningful

  *)
  let predecessor =
    Tezos_base.Block_header.hash {shell = header; protocol_data = Bytes.empty}
  in
  let* value_of_key =
    let*! r =
      Protocol.Main.value_of_key
        ~chain_id
        ~predecessor_context:context
        ~predecessor_timestamp
        ~predecessor_level
        ~predecessor_fitness
        ~predecessor
        ~timestamp
    in
    Lwt.return (Environment.wrap_tzresult r)
  in
  (*
      In the mockup mode, reactivity is important and there are
      no constraints to be consistent with other nodes. For this
      reason, the mockup mode loads the cache lazily.
      See {!Environment_context.source_of_cache}.
  *)
  let* context =
    Tezos_protocol_environment.Context.load_cache
      predecessor
      context
      `Lazy
      (fun key ->
        let*! r = value_of_key key in
        Lwt.return (Environment.wrap_tzresult r))
  in
  return context

let mem_init :
    cctxt:Tezos_client_base.Client_context.printer ->
    parameters:Protocol_parameters.t ->
    constants_overrides_json:Data_encoding.json option ->
    bootstrap_accounts_json:Data_encoding.json option ->
    Tezos_mockup_registration.Registration.mockup_context tzresult Lwt.t =
  let open Lwt_result_syntax in
  fun ~cctxt ~parameters ~constants_overrides_json ~bootstrap_accounts_json ->
    let hash = genesis_block_hash in
    (* Need to read this Json file before since timestamp modification may be in
       there *)
    let override_protocol_parameters
        (constants_overrides_json : Data_encoding.json option)
        (parameters : Protocol_parameters.t) :
        Protocol_constants_overrides.t tzresult Lwt.t =
      (* [merge_objects v1 v2] recursively overrides [v1] with [v2], and
         point-wise if [v1] and [v2] are objects.

         If [v1] and [v2] are objects, then the resulting value is an object where
           - each field that is only present in either v1 and v2 is copied as is.
             however, if the value in [v2] is explicitly [Some `Null], then the field is
             absent in the result.
           - if a field is present in both [v1] and [v2], then its value in the
             result is merge recursively. *)
      let rec merge_objects (v1 : Data_encoding.Json.t)
          (v2 : Data_encoding.Json.t) : Data_encoding.Json.t =
        match (v1, v2) with
        | `O fs1, `O fs2 ->
            let fields =
              let tbl = String.Hashtbl.of_seq (List.to_seq fs1) in
              List.iter
                (fun (k2, v2) ->
                  match (String.Hashtbl.find_opt tbl k2, v2) with
                  | _, `Null -> String.Hashtbl.remove tbl k2
                  | Some v1, v2 ->
                      String.Hashtbl.replace tbl k2 (merge_objects v1 v2)
                  | None, v2 -> String.Hashtbl.add tbl k2 v2)
                fs2 ;
              List.of_seq (String.Hashtbl.to_seq tbl)
            in
            `O fields
        | _, v2 -> v2
      in
      match constants_overrides_json with
      | Some json ->
          let parameters_json =
            Data_encoding.Json.construct
              Constants.Parametric.encoding
              parameters.constants
          in
          let parameters_overriden = merge_objects parameters_json json in
          let* protocol_overrides =
            match
              Data_encoding.Json.destruct
                lib_parameters_json_encoding
                parameters_overriden
            with
            | _, x -> return x
            | exception error ->
                failwith
                  "cannot read protocol constants overrides: %a"
                  (Data_encoding.Json.print_error ?print_unknown:None)
                  error
          in
          let fields_with_override = match json with `O fs -> fs | _ -> [] in
          let field_pp ppf (name, value) =
            Format.fprintf ppf "@[<h>%s: %a@]" name Data_encoding.Json.pp value
          in
          let*! () =
            if fields_with_override <> [] then
              cctxt#message
                "@[<v>mockup client uses protocol overrides:@,%a@]@?"
                (Format.pp_print_list field_pp)
                fields_with_override
            else Lwt.return_unit
          in
          return protocol_overrides
      | None ->
          return
            Protocol_constants_overrides.
              {
                parametric = parameters.constants;
                timestamp = None;
                chain_id = None;
              }
    in
    let* (protocol_overrides : Protocol_constants_overrides.t) =
      override_protocol_parameters constants_overrides_json parameters
    in
    let chain_id =
      Tezos_mockup_registration.Mockup_args.Chain_id.choose
        ~from_config_file:protocol_overrides.chain_id
    in
    let default = parameters.initial_timestamp in
    let timestamp = Option.value ~default protocol_overrides.timestamp in
    let*! () =
      if not @@ Time.Protocol.equal default timestamp then
        cctxt#message
          "@[<h>initial_timestamp: %a@]"
          Time.Protocol.pp_hum
          timestamp
      else Lwt.return_unit
    in
    let fitness =
      Protocol.Alpha_context.(
        Fitness.create_without_locked_round
          ~level:Raw_level.root
          ~predecessor_round:Round.zero
          ~round:Round.zero
        |> Fitness.to_raw)
    in
    let shell_header =
      Forge.make_shell
        ~level:0l
        ~predecessor:hash
        ~timestamp
        ~fitness
        ~operations_hash:Operation_list_list_hash.zero
    in
    let* bootstrap_accounts_custom =
      match bootstrap_accounts_json with
      | None -> return_none
      | Some json -> (
          match
            Data_encoding.Json.destruct
              (Data_encoding.list Parsed_account.encoding)
              json
          with
          | accounts ->
              let*! () =
                cctxt#message
                  "@[<h>mockup client uses custom bootstrap accounts:@]"
              in
              let open Format in
              let*! () =
                cctxt#message
                  "@[%a@]"
                  (pp_print_list
                     ~pp_sep:(fun ppf () -> fprintf ppf ";@ ")
                     Parsed_account.pp)
                  accounts
              in
              let* bootstrap_accounts =
                List.map_es Parsed_account.to_bootstrap_account accounts
              in
              return_some bootstrap_accounts
          | exception error ->
              failwith
                "cannot read definitions of bootstrap accounts: %a"
                (Data_encoding.Json.print_error ?print_unknown:None)
                error)
    in
    let* context =
      initial_context
        chain_id
        shell_header
        {
          parameters with
          bootstrap_accounts =
            Option.value
              ~default:parameters.bootstrap_accounts
              bootstrap_accounts_custom;
          constants = protocol_overrides.parametric;
        }
    in
    let protocol_data =
      let payload_hash =
        Protocol.Block_payload_hash.hash_bytes
          [
            Block_hash.to_bytes hash;
            Operation_list_hash.(to_bytes @@ compute []);
          ]
      in
      let open Protocol.Alpha_context.Block_header in
      let _, _, sk = Signature.generate_key () in
      let proof_of_work_nonce =
        Bytes.create Protocol.Alpha_context.Constants.proof_of_work_nonce_size
      in
      let contents =
        {
          payload_round = Round.zero;
          payload_hash;
          seed_nonce_hash = None;
          proof_of_work_nonce;
          (* following Baking_configuration.per_block_votes in lib_delegate *)
          per_block_votes =
            {
              liquidity_baking_vote = Per_block_votes.Per_block_vote_pass;
              adaptive_issuance_vote = Per_block_votes.Per_block_vote_pass;
            };
        }
      in
      let unsigned_bytes =
        Data_encoding.Binary.to_bytes_exn
          Block_header.unsigned_encoding
          (shell_header, contents)
      in
      let signature =
        Signature.sign
          ~watermark:
            Protocol.Alpha_context.Block_header.(
              to_watermark (Block_header chain_id))
          sk
          unsigned_bytes
      in
      Data_encoding.Binary.to_bytes_exn
        Protocol.block_header_data_encoding
        {contents; signature}
    in
    return
      Tezos_mockup_registration.Registration_intf.
        {
          chain = chain_id;
          rpc_context =
            Tezos_protocol_environment.
              {block_hash = hash; block_header = shell_header; context};
          protocol_data;
        }

let migrate :
    Tezos_mockup_registration.Registration.mockup_context ->
    Tezos_mockup_registration.Registration.mockup_context tzresult Lwt.t =
  let open Lwt_result_syntax in
  fun {chain; rpc_context; protocol_data} ->
    let Tezos_protocol_environment.{block_hash; context; block_header} =
      rpc_context
    in
    let*! context = Environment.Updater.activate context Protocol.hash in
    let* {context; _} =
      let*! r = Protocol.Main.init chain context block_header in
      Lwt.return (Environment.wrap_tzresult r)
    in
    let rpc_context =
      Tezos_protocol_environment.{block_hash; block_header; context}
    in
    return
      Tezos_mockup_registration.Registration_intf.
        {chain; rpc_context; protocol_data}

(* ------------------------------------------------------------------------- *)
(* Register mockup *)

module M :
  Tezos_mockup_registration.Registration_intf.MOCKUP
    with module Protocol = Lifted_protocol = struct
  type parameters = Protocol_parameters.t

  type protocol_constants = Protocol_constants_overrides.t

  let parameters_encoding = Protocol_parameters.encoding

  let default_parameters = Protocol_parameters.default_value

  let protocol_constants_encoding = Protocol_constants_overrides.encoding

  let default_protocol_constants = Protocol_constants_overrides.default_value

  let default_bootstrap_accounts = Parsed_account.default_to_json

  let protocol_hash = Protocol.hash

  module Protocol = Lifted_protocol
  module Block_services = Protocol_client_context.Alpha_block_services

  let directory = Plugin.RPC.rpc_services

  let init ~cctxt ~parameters ~constants_overrides_json ~bootstrap_accounts_json
      =
    mem_init
      ~cctxt:(cctxt :> Tezos_client_base.Client_context.printer)
      ~parameters
      ~constants_overrides_json
      ~bootstrap_accounts_json

  let migrate = migrate
end

let () =
  Tezos_mockup_registration.Registration.register_mockup_environment (module M)
