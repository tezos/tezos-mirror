(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type contract_parameters = {
  probability : float;
  invocation_fee : Tez.t;
  invocation_gas_limit : Gas.Arith.integral;
}

let contract_parameters_encoding =
  let open Data_encoding in
  conv
    (fun {probability; invocation_fee; invocation_gas_limit} ->
      (probability, invocation_fee, invocation_gas_limit))
    (fun (probability, invocation_fee, invocation_gas_limit) ->
      {probability; invocation_fee; invocation_gas_limit})
    (obj3
       (req "probability" float)
       (req "invocation_fee" Tez.encoding)
       (req "invocation_gas_limit" Gas.Arith.n_integral_encoding))

let contract_parameters_collection_encoding =
  let open Data_encoding in
  assoc contract_parameters_encoding

(** Internal representation of a supported smart contract. *)
type smart_contract = {
  alias : string;  (** Human-readable alias of the contract *)
  mainnet_address : string;  (** Address this contract has on Mainnet *)
  initial_storage : string;  (** Initial storage (used for origination) *)
  origination_fee_cap : Tez.t;  (** Origination fee cap *)
  origination_burn_cap : Tez.t;  (** Origination burn cap *)
  code : Script.expr;  (** Code of the smart contract *)
  invocation_arg : Script.expr;
      (** Argument for invocations during the stress test *)
  invocation_entrypoint : Entrypoint_repr.t;
      (** Entrypoint for invocations during the stress test *)
}

type t = (contract_parameters * Contract.t * smart_contract) list

let no_contracts = []

let parse_michelson_expr_exn expr =
  let open Tezos_micheline in
  match
    Micheline_parser.no_parsing_error
      (Michelson_v1_parser.parse_expression expr)
  with
  | Ok x -> x.expanded
  | Error _ -> Stdlib.failwith "parse_michelson_expr_exn"

let parse_michelson_toplevel_exn expr =
  let open Tezos_micheline in
  match
    Micheline_parser.no_parsing_error
      (Michelson_v1_parser.parse_toplevel ?check:(Some true) expr)
  with
  | Ok x -> x.expanded
  | Error _ -> Stdlib.failwith "parse_michelson_toplevel_exn"

let hic_et_nunc =
  {
    alias = "hic_et_nunc";
    mainnet_address = "KT1RJ6PbjHpwc3M5rw5s2Nbmefwbuwbdxton";
    initial_storage =
      {| (Pair (Pair "tz1invkmrRJev6TvH9CvhpLN4auk4cik8G5Y" (Pair 1 {Elt (Pair "tz1invkmrRJev6TvH9CvhpLN4auk4cik8G5Y" 0) 1}))(Pair (Pair {} {})(Pair False {Elt 0 (Pair 0 {})})))
|};
    code =
      parse_michelson_toplevel_exn
        {|
   storage ( pair ( pair ( address %administrator ) ( pair ( nat %all_tokens ) ( big_map %ledger ( pair address nat ) nat ) ) ) ( pair ( pair ( big_map %metadata string bytes ) ( big_map %operators ( pair ( address %owner ) ( pair ( address %operator ) ( nat %token_id ) ) ) unit ) ) ( pair ( bool %paused ) ( big_map %token_metadata nat ( pair ( nat %token_id ) ( map %token_info string bytes ) ) ) ) ) ) ;
   parameter (or (or (or (pair %balance_of (list %requests (pair (address %owner) (nat %token_id))) (contract %callback (list (pair (pair %request (address %owner) (nat %token_id)) (nat %balance))))) (list %hDAO_batch (pair (nat %amount) (address %to_)))) (or (pair %mint (pair (address %address) (nat %amount)) (pair (nat %token_id) (map %token_info string bytes))) (address %set_administrator))) (or (or (bool %set_pause) (pair %token_metadata (list %token_ids nat) (lambda %handler (list (pair (nat %token_id) (map %token_info string bytes)))unit))) (or (list %transfer (pair (address %from_) (list %txs (pair (address %to_) (pair (nat %token_id) (nat %amount)))))) (list %update_Hunitoperators (or (pair %add_operator (address %owner) (pair (address %operator) (nat %token_id))) (pair %remove_operator (address %owner) (pair (address %operator) (nat %token_id))))))));
   code { DUP ; CDR ; SWAP ; CAR ; IF_LEFT { IF_LEFT { IF_LEFT { SWAP ; DUP ; DUG 2 ; CDR ; CDR ; CAR ; IF { PUSH int 722 ; FAILWITH } {} ; DUP ; CAR ; MAP { DIG 2 ; DUP ; DUG 3 ; CDR ; CDR ; CDR ; SWAP ; DUP ; DUG 2 ; CDR ; MEM ; IF {} { PUSH string "FA2_TOKEN_UNDEFINED" ; FAILWITH } ; DIG 2 ; DUP ; DUG 3 ; CAR ; CDR ; CDR ; SWAP ; DUP ; CDR ; SWAP ; DUP ; DUG 3 ; CAR ; PAIR ; MEM ; IF { DIG 2 ; DUP ; DUG 3 ; CAR ; CDR ; CDR ; SWAP ; DUP ; CDR ; SWAP ; DUP ; DUG 3 ; CAR ; PAIR ; GET ; IF_NONE { PUSH int 729 ; FAILWITH } {} ; SWAP ; PAIR %request %balance } { PUSH nat 0 ; SWAP ; PAIR %request %balance } } ; NIL operation ; DIG 2 ; CDR ; PUSH mutez 0 ; DIG 3 ; TRANSFER_TOKENS ; CONS } { SWAP ; DUP ; DUG 2 ; CAR ; CAR ; SENDER ; COMPARE ; EQ ; IF {} { PUSH int 776 ; FAILWITH } ; DUP ; ITER { DIG 2 ; DUP ; DUG 3 ; CAR ; CDR ; CDR ; PUSH nat 0 ; DIG 2 ; DUP ; DUG 3 ; CDR ; PAIR ; MEM ; IF { DIG 2 ; DUP ; DUG 3 ; DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; DUP ; CAR ; SWAP ; CDR ; DUP ; PUSH nat 0 ; DIG 6 ; DUP ; DUG 7 ; CDR ; PAIR ; DUP ; DUG 2 ; GET ; IF_NONE { PUSH int 781 ; FAILWITH } { DROP } ; DIG 5 ; DUP ; DUG 6 ; CAR ; DIG 8 ; CAR ; CDR ; CDR ; PUSH nat 0 ; DIG 8 ; CDR ; PAIR ; GET ; IF_NONE { PUSH int 781 ; FAILWITH } {} ; ADD ; SOME ; SWAP ; UPDATE ; SWAP ; PAIR ; SWAP ; PAIR ; PAIR ; SWAP } { DIG 2 ; DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; DUP ; CAR ; SWAP ; CDR ; DIG 4 ; DUP ; DUG 5 ; CAR ; SOME ; PUSH nat 0 ; DIG 6 ; CDR ; PAIR ; UPDATE ; SWAP ; PAIR ; SWAP ; PAIR ; PAIR ; SWAP } ; SWAP ; DUP ; DUG 2 ; CDR ; CDR ; CDR ; PUSH nat 0 ; MEM ; IF {} { SWAP ; DUP ; CAR ; SWAP ; CDR ; DUP ; CAR ; SWAP ; CDR ; DUP ; CAR ; SWAP ; CDR ; EMPTY_MAP string bytes ; PUSH string "ipfs://QmSVsfwH8es7Ur2eqto9hVpcd2dfWASmEaNxTPpcymuJzg" ; PACK ; SOME ; PUSH string "" ; UPDATE ; PUSH nat 0 ; PAIR %token_id %token_info ; SOME ; PUSH nat 0 ; UPDATE ; SWAP ; PAIR ; SWAP ; PAIR ; SWAP ; PAIR ; SWAP } } ; DROP ; NIL operation } } { IF_LEFT { SWAP ; DUP ; DUG 2 ; CAR ; CAR ; SENDER ; COMPARE ; NEQ ; IF {} { PUSH int 820 ; FAILWITH } ; SWAP ; DUP ; DUG 2 ; DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; CDR ; DIG 4 ; CAR ; CDR ; CAR ; DUP ; PUSH nat 1 ; DIG 6 ; DUP ; DUG 7 ; CDR ; CAR ; ADD ; DUP ; DUG 2 ; COMPARE ; LE ; IF { DROP } { SWAP ; DROP } ; PAIR ; SWAP ; PAIR ; PAIR ; DUP ; DUG 2 ; CAR ; CDR ; CDR ; SWAP ; DUP ; CDR ; CAR ; SWAP ; DUP ; DUG 3 ; CAR ; CAR ; PAIR ; MEM ; IF { SWAP ; DUP ; DUG 2 ; DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; DUP ; CAR ; SWAP ; CDR ; DUP ; DIG 5 ; DUP ; CDR ; CAR ; SWAP ; DUP ; DUG 7 ; CAR ; CAR ; PAIR ; DUP ; DUG 2 ; GET ; IF_NONE { PUSH int 832 ; FAILWITH } { DROP } ; DIG 5 ; DUP ; DUG 6 ; CAR ; CDR ; DIG 7 ; CAR ; CDR ; CDR ; DIG 7 ; DUP ; CDR ; CAR ; SWAP ; DUP ; DUG 9 ; CAR ; CAR ; PAIR ; GET ; IF_NONE { PUSH int 832 ; FAILWITH } {} ; ADD ; SOME ; SWAP ; UPDATE ; SWAP ; PAIR ; SWAP ; PAIR ; PAIR ; SWAP } { SWAP ; DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; DUP ; CAR ; SWAP ; CDR ; DIG 4 ; DUP ; DUG 5 ; CAR ; CDR ; SOME ; DIG 5 ; DUP ; CDR ; CAR ; SWAP ; DUP ; DUG 7 ; CAR ; CAR ; PAIR ; UPDATE ; SWAP ; PAIR ; SWAP ; PAIR ; PAIR ; SWAP } ; SWAP ; DUP ; DUG 2 ; CDR ; CDR ; CDR ; SWAP ; DUP ; DUG 2 ; CDR ; CAR ; MEM ; IF { DROP } { SWAP ; DUP ; CAR ; SWAP ; CDR ; DUP ; CAR ; SWAP ; CDR ; DUP ; CAR ; SWAP ; CDR ; DIG 4 ; DUP ; CDR ; CDR ; SWAP ; DUP ; DUG 6 ; CDR ; CAR ; PAIR %token_id %token_info ; SOME ; DIG 5 ; CDR ; CAR ; UPDATE ; SWAP ; PAIR ; SWAP ; PAIR ; SWAP ; PAIR } } { SWAP ; DUP ; DUG 2 ; CAR ; CAR ; SENDER ; COMPARE ; EQ ; IF {} { PUSH int 805 ; FAILWITH } ; SWAP ; DUP ; CDR ; SWAP ; CAR ; CDR ; DIG 2 ; PAIR ; PAIR } ; NIL operation } } { IF_LEFT { IF_LEFT { SWAP ; DUP ; DUG 2 ; CAR ; CAR ; SENDER ; COMPARE ; EQ ; IF {} { PUSH int 814 ; FAILWITH } ; SWAP ; DUP ; CAR ; SWAP ; CDR ; DUP ; CAR ; SWAP ; CDR ; CDR ; DIG 3 ; PAIR ; SWAP ; PAIR ; SWAP ; PAIR } { SWAP ; DUP ; DUG 2 ; CDR ; CDR ; CAR ; IF { PUSH int 855 ; FAILWITH } {} ; DUP ; CDR ; SWAP ; DUP ; DUG 2 ; CAR ; MAP { DIG 3 ; DUP ; DUG 4 ; CDR ; CDR ; CDR ; SWAP ; GET ; IF_NONE { PUSH int 865 ; FAILWITH } {} } ; DIG 2 ; DROP ; EXEC ; DROP } } { IF_LEFT { SWAP ; DUP ; DUG 2 ; CDR ; CDR ; CAR ; IF { PUSH int 679 ; FAILWITH } {} ; DUP ; ITER { DUP ; CDR ; ITER { DIG 3 ; DUP ; DUG 4 ; CAR ; CAR ; SENDER ; COMPARE ; NEQ ; IF { PUSH bool True } { SENDER ; DIG 2 ; DUP ; DUG 3 ; CAR ; COMPARE ; EQ } ; IF { PUSH bool True } { DIG 3 ; DUP ; DUG 4 ; CDR ; CAR ; CDR ; SWAP ; DUP ; DUG 2 ; CDR ; CAR ; SENDER ; PAIR %operator %token_id ; DIG 3 ; DUP ; DUG 4 ; CAR ; PAIR %owner ; MEM } ; IF {} { PUSH string "FA2_NOT_OPERATOR" ; FAILWITH } ; DIG 3 ; DUP ; DUG 4 ; CDR ; CDR ; CDR ; SWAP ; DUP ; DUG 2 ; CDR ; CAR ; MEM ; IF {} { PUSH string "FA2_TOKEN_UNDEFINED" ; FAILWITH } ; DUP ; CDR ; CDR ; PUSH nat 0 ; COMPARE ; LT ; IF { DUP ; CDR ; CDR ; DIG 4 ; DUP ; DUG 5 ; CAR ; CDR ; CDR ; DIG 2 ; DUP ; DUG 3 ; CDR ; CAR ; DIG 4 ; DUP ; DUG 5 ; CAR ; PAIR ; GET ; IF_NONE { PUSH int 706 ; FAILWITH } {} ; COMPARE ; GE ; IF {} { PUSH string "FA2_INSUFFICIENT_BALANCE" ; FAILWITH } ; DIG 3 ; DUP ; DUG 4 ; DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; DUP ; CAR ; SWAP ; CDR ; DUP ; DIG 5 ; DUP ; DUG 6 ; CDR ; CAR ; DIG 7 ; DUP ; DUG 8 ; CAR ; PAIR ; DUP ; DUG 2 ; GET ; IF_NONE { PUSH int 710 ; FAILWITH } { DROP } ; DIG 5 ; DUP ; DUG 6 ; CDR ; CDR ; DIG 9 ; CAR ; CDR ; CDR ; DIG 7 ; DUP ; DUG 8 ; CDR ; CAR ; DIG 9 ; DUP ; DUG 10 ; CAR ; PAIR ; GET ; IF_NONE { PUSH int 710 ; FAILWITH } {} ; SUB ; ISNAT ; IF_NONE { PUSH int 710 ; FAILWITH } {} ; SOME ; SWAP ; UPDATE ; SWAP ; PAIR ; SWAP ; PAIR ; PAIR ; DUP ; DUG 4 ; CAR ; CDR ; CDR ; SWAP ; DUP ; CDR ; CAR ; SWAP ; DUP ; DUG 3 ; CAR ; PAIR ; MEM ; IF { DIG 3 ; DUP ; DUG 4 ; DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; DUP ; CAR ; SWAP ; CDR ; DUP ; DIG 5 ; DUP ; CDR ; CAR ; SWAP ; DUP ; DUG 7 ; CAR ; PAIR ; DUP ; DUG 2 ; GET ; IF_NONE { PUSH int 713 ; FAILWITH } { DROP } ; DIG 5 ; DUP ; DUG 6 ; CDR ; CDR ; DIG 9 ; CAR ; CDR ; CDR ; DIG 7 ; DUP ; CDR ; CAR ; SWAP ; CAR ; PAIR ; GET ; IF_NONE { PUSH int 713 ; FAILWITH } {} ; ADD ; SOME ; SWAP ; UPDATE ; SWAP ; PAIR ; SWAP ; PAIR ; PAIR ; DUG 2 } { DIG 3 ; DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; DUP ; CAR ; SWAP ; CDR ; DIG 4 ; DUP ; DUG 5 ; CDR ; CDR ; SOME ; DIG 5 ; DUP ; CDR ; CAR ; SWAP ; CAR ; PAIR ; UPDATE ; SWAP ; PAIR ; SWAP ; PAIR ; PAIR ; DUG 2 } } { DROP } } ; DROP } ; DROP } { DUP ; ITER { IF_LEFT { DUP ; CAR ; SENDER ; COMPARE ; EQ ; IF { PUSH bool True } { DIG 2 ; DUP ; DUG 3 ; CAR ; CAR ; SENDER ; COMPARE ; EQ } ; IF {} { PUSH int 758 ; FAILWITH } ; DIG 2 ; DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; PUSH ( option unit ) ( Some Unit ) ; DIG 5 ; DUP ; CDR ; CDR ; SWAP ; DUP ; DUG 7 ; CDR ; CAR ; PAIR %operator %token_id ; DIG 6 ; CAR ; PAIR %owner ; UPDATE ; SWAP ; PAIR ; PAIR ; SWAP ; PAIR ; SWAP } { DUP ; CAR ; SENDER ; COMPARE ; EQ ; IF { PUSH bool True } { DIG 2 ; DUP ; DUG 3 ; CAR ; CAR ; SENDER ; COMPARE ; EQ } ; IF {} { PUSH int 765 ; FAILWITH } ; DIG 2 ; DUP ; CAR ; SWAP ; CDR ; DUP ; CDR ; SWAP ; CAR ; DUP ; CAR ; SWAP ; CDR ; NONE unit ; DIG 5 ; DUP ; CDR ; CDR ; SWAP ; DUP ; DUG 7 ; CDR ; CAR ; PAIR %operator %token_id ; DIG 6 ; CAR ; PAIR %owner ; UPDATE ; SWAP ; PAIR ; PAIR ; SWAP ; PAIR ; SWAP } } ; DROP } } ; NIL operation } ; PAIR}
   |};
    origination_fee_cap = Tez.of_mutez_exn 5_000L;
    origination_burn_cap = Tez.of_mutez_exn 1_500_000L;
    invocation_arg =
      parse_michelson_expr_exn
        {|{ Pair "tz1invkmrRJev6TvH9CvhpLN4auk4cik8G5Y" {  Pair "tz1invkmrRJev6TvH9CvhpLN4auk4cik8G5Y" ( Pair 0 1 )  } } |};
    invocation_entrypoint = Entrypoint_repr.of_string_strict_exn "transfer";
  }

(** This is the list of all supported smart contracts. If you wish to add
    support for a new smart contract all you need to do is to extend this
    list. *)
let all_contracts = [hic_et_nunc]

let init (cctxt : Protocol_client_context.full)
    (contract_parameters : (string * contract_parameters) list) :
    t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let sum_of_probabilities =
    List.fold_left
      (fun acc (_, {probability; _}) -> acc +. probability)
      0.0
      contract_parameters
  in
  let* () =
    if sum_of_probabilities > 1.0 then
      failwith "sum of smart contract call probabilities is greater than 1.0!"
    else return_unit
  in
  let init_one (alias, params) =
    let* contract =
      Client_proto_contracts.Contract_alias.get_contract cctxt alias
    in
    let* smart_contract =
      match List.find (fun x -> String.equal alias x.alias) all_contracts with
      | None -> failwith "unknown smart contract alias: %s" alias
      | Some x -> return x
    in
    return (params, contract, smart_contract)
  in
  List.map_es init_one contract_parameters

type invocation_parameters = {
  destination : Contract.t;
  entrypoint : Entrypoint_repr.t;
  arg : Script.expr;
  fee : Tez.t;
  gas_limit : Gas.Arith.integral;
}

let select (smart_contracts : t) (q : float) : invocation_parameters option =
  let rec go xs0 x =
    match xs0 with
    | [] -> None
    | (params, contract, smart_contract) :: xs1 ->
        if x < params.probability then
          Some
            {
              destination = contract;
              entrypoint = smart_contract.invocation_entrypoint;
              arg = smart_contract.invocation_arg;
              fee = params.invocation_fee;
              gas_limit = params.invocation_gas_limit;
            }
        else go xs1 (x -. params.probability)
  in
  go smart_contracts q

let originate_command =
  let open Tezos_clic in
  let open Client_proto_context in
  let open Client_proto_contracts in
  let open Client_proto_context_commands in
  command
    ~group
    ~desc:"Originate all supported smart contracts for use in the stresstest."
    no_options
    (prefixes ["stresstest"; "originate"; "smart"; "contracts"; "from"]
    @@ Contract_alias.destination_param
         ~name:"src"
         ~desc:"name of the source contract"
    @@ stop)
    (fun () source (cctxt : Protocol_client_context.full) ->
      let open Lwt_result_syntax in
      match source with
      | Originated _ ->
          failwith "only implicit accounts can be the source of an origination"
      | Implicit source ->
          let* _, src_pk, src_sk = Client_keys.get_key cctxt source in
          let originate_one (scontract : smart_contract) =
            let fee_parameter =
              {
                Injection.minimal_fees = Tez.of_mutez_exn 100L;
                minimal_nanotez_per_byte = Q.of_int 1000;
                minimal_nanotez_per_gas_unit = Q.of_int 100;
                force_low_fee = false;
                fee_cap = scontract.origination_fee_cap;
                burn_cap = scontract.origination_burn_cap;
              }
            in
            let*! errors =
              originate_contract
                cctxt
                ~chain:cctxt#chain
                ~block:cctxt#block
                ~delegate:None
                ~initial_storage:scontract.initial_storage
                ~balance:Tez.zero (* initial balance *)
                ~source
                ~src_pk
                ~src_sk
                ~code:scontract.code
                ~fee_parameter
                ()
            in
            let*! r =
              report_michelson_errors
                ~no_print_source:true
                ~msg:"origination simulation failed"
                cctxt
                errors
            in
            match r with
            | None -> return_unit
            | Some (_res, contract) ->
                save_contract ~force:false cctxt scontract.alias contract
          in
          List.iter_es originate_one all_contracts)

let with_every_known_smart_contract cctxt callback =
  let open Lwt_result_syntax in
  let items =
    List.map
      (fun x ->
        ( x.alias,
          {
            probability = 0.0;
            invocation_fee = Tez.zero;
            invocation_gas_limit =
              Default_parameters.constants_mainnet.hard_gas_limit_per_operation;
          } ))
      all_contracts
  in
  let* smart_contracts = init cctxt items in
  let rec go xs0 =
    match xs0 with
    | [] -> return []
    | (contract_parameters, contract, smart_contract) :: xs1 ->
        let* r =
          callback
            [
              ( {contract_parameters with probability = 1.0},
                contract,
                smart_contract );
            ]
        in
        let* rs = go xs1 in
        return ((smart_contract.alias, r) :: rs)
  in
  go smart_contracts

let mainnet_address_to_alias address =
  List.find (fun x -> String.equal x.mainnet_address address) all_contracts
  |> Option.map (fun x -> x.alias)
