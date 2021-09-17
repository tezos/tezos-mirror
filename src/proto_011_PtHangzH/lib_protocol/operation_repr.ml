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

(* Tezos Protocol Implementation - Low level Repr. of Operations *)

module Kind = struct
  type seed_nonce_revelation = Seed_nonce_revelation_kind

  type endorsement_with_slot = Endorsement_with_slot_kind

  type double_endorsement_evidence = Double_endorsement_evidence_kind

  type double_baking_evidence = Double_baking_evidence_kind

  type activate_account = Activate_account_kind

  type endorsement = Endorsement_kind

  type proposals = Proposals_kind

  type ballot = Ballot_kind

  type reveal = Reveal_kind

  type transaction = Transaction_kind

  type origination = Origination_kind

  type delegation = Delegation_kind

  type failing_noop = Failing_noop_kind

  type register_global_constant = Register_global_constant_kind

  type 'a manager =
    | Reveal_manager_kind : reveal manager
    | Transaction_manager_kind : transaction manager
    | Origination_manager_kind : origination manager
    | Delegation_manager_kind : delegation manager
    | Register_global_constant_manager_kind : register_global_constant manager
end

type raw = Operation.t = {shell : Operation.shell_header; proto : bytes}

let raw_encoding = Operation.encoding

type 'kind operation = {
  shell : Operation.shell_header;
  protocol_data : 'kind protocol_data;
}

and 'kind protocol_data = {
  contents : 'kind contents_list;
  signature : Signature.t option;
}

and _ contents_list =
  | Single : 'kind contents -> 'kind contents_list
  | Cons :
      'kind Kind.manager contents * 'rest Kind.manager contents_list
      -> ('kind * 'rest) Kind.manager contents_list

and _ contents =
  | Endorsement : {level : Raw_level_repr.t} -> Kind.endorsement contents
  | Seed_nonce_revelation : {
      level : Raw_level_repr.t;
      nonce : Seed_repr.nonce;
    }
      -> Kind.seed_nonce_revelation contents
  | Endorsement_with_slot : {
      endorsement : Kind.endorsement operation;
      slot : int;
    }
      -> Kind.endorsement_with_slot contents
  | Double_endorsement_evidence : {
      op1 : Kind.endorsement operation;
      op2 : Kind.endorsement operation;
      slot : int;
    }
      -> Kind.double_endorsement_evidence contents
  | Double_baking_evidence : {
      bh1 : Block_header_repr.t;
      bh2 : Block_header_repr.t;
    }
      -> Kind.double_baking_evidence contents
  | Activate_account : {
      id : Ed25519.Public_key_hash.t;
      activation_code : Blinded_public_key_hash.activation_code;
    }
      -> Kind.activate_account contents
  | Proposals : {
      source : Signature.Public_key_hash.t;
      period : int32;
      proposals : Protocol_hash.t list;
    }
      -> Kind.proposals contents
  | Ballot : {
      source : Signature.Public_key_hash.t;
      period : int32;
      proposal : Protocol_hash.t;
      ballot : Vote_repr.ballot;
    }
      -> Kind.ballot contents
  | Failing_noop : string -> Kind.failing_noop contents
  | Manager_operation : {
      source : Signature.public_key_hash;
      fee : Tez_repr.tez;
      counter : counter;
      operation : 'kind manager_operation;
      gas_limit : Gas_limit_repr.Arith.integral;
      storage_limit : Z.t;
    }
      -> 'kind Kind.manager contents

and _ manager_operation =
  | Reveal : Signature.Public_key.t -> Kind.reveal manager_operation
  | Transaction : {
      amount : Tez_repr.tez;
      parameters : Script_repr.lazy_expr;
      entrypoint : string;
      destination : Contract_repr.contract;
    }
      -> Kind.transaction manager_operation
  | Origination : {
      delegate : Signature.Public_key_hash.t option;
      script : Script_repr.t;
      credit : Tez_repr.tez;
      preorigination : Contract_repr.t option;
    }
      -> Kind.origination manager_operation
  | Delegation :
      Signature.Public_key_hash.t option
      -> Kind.delegation manager_operation
  | Register_global_constant : {
      value : Script_repr.lazy_expr;
    }
      -> Kind.register_global_constant manager_operation

and counter = Z.t

let manager_kind : type kind. kind manager_operation -> kind Kind.manager =
  function
  | Reveal _ -> Kind.Reveal_manager_kind
  | Transaction _ -> Kind.Transaction_manager_kind
  | Origination _ -> Kind.Origination_manager_kind
  | Delegation _ -> Kind.Delegation_manager_kind
  | Register_global_constant _ -> Kind.Register_global_constant_manager_kind

type 'kind internal_operation = {
  source : Contract_repr.contract;
  operation : 'kind manager_operation;
  nonce : int;
}

type packed_manager_operation =
  | Manager : 'kind manager_operation -> packed_manager_operation

type packed_contents = Contents : 'kind contents -> packed_contents

type packed_contents_list =
  | Contents_list : 'kind contents_list -> packed_contents_list

type packed_protocol_data =
  | Operation_data : 'kind protocol_data -> packed_protocol_data

type packed_operation = {
  shell : Operation.shell_header;
  protocol_data : packed_protocol_data;
}

let pack ({shell; protocol_data} : _ operation) : packed_operation =
  {shell; protocol_data = Operation_data protocol_data}

type packed_internal_operation =
  | Internal_operation : 'kind internal_operation -> packed_internal_operation

let rec to_list = function
  | Contents_list (Single o) -> [Contents o]
  | Contents_list (Cons (o, os)) -> Contents o :: to_list (Contents_list os)

(* This first version of of_list has the type (_, string) result expected by
   the conv_with_guard combinator of Data_encoding. For a more conventional
   return type see [of_list] below. *)
let rec of_list_internal = function
  | [] -> Error "Operation lists should not be empty."
  | [Contents o] -> Ok (Contents_list (Single o))
  | Contents o :: os -> (
      of_list_internal os >>? fun (Contents_list os) ->
      match (o, os) with
      | (Manager_operation _, Single (Manager_operation _)) ->
          Ok (Contents_list (Cons (o, os)))
      | (Manager_operation _, Cons _) -> Ok (Contents_list (Cons (o, os)))
      | _ ->
          Error
            "Operation list of length > 1 should only contains manager \
             operations.")

type error += Contents_list_error of string (* `Permanent *)

let of_list l =
  match of_list_internal l with
  | Ok contents -> Ok contents
  | Error s -> error @@ Contents_list_error s

module Encoding = struct
  open Data_encoding

  let case tag name args proj inj =
    case
      tag
      ~title:(String.capitalize_ascii name)
      (merge_objs (obj1 (req "kind" (constant name))) args)
      (fun x -> match proj x with None -> None | Some x -> Some ((), x))
      (fun ((), x) -> inj x)

  module Manager_operations = struct
    type 'kind case =
      | MCase : {
          tag : int;
          name : string;
          encoding : 'a Data_encoding.t;
          select : packed_manager_operation -> 'kind manager_operation option;
          proj : 'kind manager_operation -> 'a;
          inj : 'a -> 'kind manager_operation;
        }
          -> 'kind case
    [@@coq_force_gadt]

    let[@coq_axiom_with_reason "gadt"] reveal_case =
      MCase
        {
          tag = 0;
          name = "reveal";
          encoding = obj1 (req "public_key" Signature.Public_key.encoding);
          select = (function Manager (Reveal _ as op) -> Some op | _ -> None);
          proj = (function Reveal pkh -> pkh);
          inj = (fun pkh -> Reveal pkh);
        }

    let entrypoint_encoding =
      def
        ~title:"entrypoint"
        ~description:"Named entrypoint to a Michelson smart contract"
        "entrypoint"
      @@
      let builtin_case tag name =
        Data_encoding.case
          (Tag tag)
          ~title:name
          (constant name)
          (fun n -> if Compare.String.(n = name) then Some () else None)
          (fun () -> name)
      in
      union
        [
          builtin_case 0 "default";
          builtin_case 1 "root";
          builtin_case 2 "do";
          builtin_case 3 "set_delegate";
          builtin_case 4 "remove_delegate";
          Data_encoding.case
            (Tag 255)
            ~title:"named"
            (Bounded.string 31)
            (fun s -> Some s)
            (fun s -> s);
        ]

    let[@coq_axiom_with_reason "gadt"] transaction_case =
      MCase
        {
          tag = 1;
          name = "transaction";
          encoding =
            obj3
              (req "amount" Tez_repr.encoding)
              (req "destination" Contract_repr.encoding)
              (opt
                 "parameters"
                 (obj2
                    (req "entrypoint" entrypoint_encoding)
                    (req "value" Script_repr.lazy_expr_encoding)));
          select =
            (function Manager (Transaction _ as op) -> Some op | _ -> None);
          proj =
            (function
            | Transaction {amount; destination; parameters; entrypoint} ->
                let parameters =
                  if
                    Script_repr.is_unit_parameter parameters
                    && Compare.String.(entrypoint = "default")
                  then None
                  else Some (entrypoint, parameters)
                in
                (amount, destination, parameters));
          inj =
            (fun (amount, destination, parameters) ->
              let (entrypoint, parameters) =
                match parameters with
                | None -> ("default", Script_repr.unit_parameter)
                | Some (entrypoint, value) -> (entrypoint, value)
              in
              Transaction {amount; destination; parameters; entrypoint});
        }

    let[@coq_axiom_with_reason "gadt"] origination_case =
      MCase
        {
          tag = 2;
          name = "origination";
          encoding =
            obj3
              (req "balance" Tez_repr.encoding)
              (opt "delegate" Signature.Public_key_hash.encoding)
              (req "script" Script_repr.encoding);
          select =
            (function Manager (Origination _ as op) -> Some op | _ -> None);
          proj =
            (function
            | Origination
                {
                  credit;
                  delegate;
                  script;
                  preorigination =
                    _
                    (* the hash is only used internally
                               when originating from smart
                               contracts, don't serialize it *);
                } ->
                (credit, delegate, script));
          inj =
            (fun (credit, delegate, script) ->
              Origination {credit; delegate; script; preorigination = None});
        }

    let[@coq_axiom_with_reason "gadt"] delegation_case =
      MCase
        {
          tag = 3;
          name = "delegation";
          encoding = obj1 (opt "delegate" Signature.Public_key_hash.encoding);
          select =
            (function Manager (Delegation _ as op) -> Some op | _ -> None);
          proj = (function Delegation key -> key);
          inj = (fun key -> Delegation key);
        }

    let[@coq_axiom_with_reason "gadt"] register_global_constant_case =
      MCase
        {
          tag = 4;
          name = "register_global_constant";
          encoding = obj1 (req "value" Script_repr.lazy_expr_encoding);
          select =
            (function
            | Manager (Register_global_constant _ as op) -> Some op | _ -> None);
          proj = (function Register_global_constant {value} -> value);
          inj = (fun value -> Register_global_constant {value});
        }

    let encoding =
      let make (MCase {tag; name; encoding; select; proj; inj}) =
        case
          (Tag tag)
          name
          encoding
          (fun o ->
            match select o with None -> None | Some o -> Some (proj o))
          (fun x -> Manager (inj x))
      in
      union
        ~tag_size:`Uint8
        [
          make reveal_case;
          make transaction_case;
          make origination_case;
          make delegation_case;
          make register_global_constant_case;
        ]
  end

  type 'b case =
    | Case : {
        tag : int;
        name : string;
        encoding : 'a Data_encoding.t;
        select : packed_contents -> 'b contents option;
        proj : 'b contents -> 'a;
        inj : 'a -> 'b contents;
      }
        -> 'b case

  let endorsement_case =
    Case
      {
        tag = 0;
        name = "endorsement";
        encoding = obj1 (req "level" Raw_level_repr.encoding);
        select =
          (function Contents (Endorsement _ as op) -> Some op | _ -> None);
        proj = (fun [@coq_match_with_default] (Endorsement {level}) -> level);
        inj = (fun level -> Endorsement {level});
      }

  let[@coq_axiom_with_reason "gadt"] endorsement_encoding =
    let make (Case {tag; name; encoding; select = _; proj; inj}) =
      case (Tag tag) name encoding (fun o -> Some (proj o)) (fun x -> inj x)
    in
    let to_list : Kind.endorsement contents_list -> _ = fun (Single o) -> o in
    let of_list : Kind.endorsement contents -> _ = fun o -> Single o in
    def "inlined.endorsement"
    @@ conv
         (fun ({shell; protocol_data = {contents; signature}} : _ operation) ->
           (shell, (contents, signature)))
         (fun (shell, (contents, signature)) : _ operation ->
           {shell; protocol_data = {contents; signature}})
         (merge_objs
            Operation.shell_header_encoding
            (obj2
               (req
                  "operations"
                  (conv to_list of_list
                  @@ def "inlined.endorsement.contents"
                  @@ union [make endorsement_case]))
               (varopt "signature" Signature.encoding)))

  let[@coq_axiom_with_reason "gadt"] seed_nonce_revelation_case =
    Case
      {
        tag = 1;
        name = "seed_nonce_revelation";
        encoding =
          obj2
            (req "level" Raw_level_repr.encoding)
            (req "nonce" Seed_repr.nonce_encoding);
        select =
          (function
          | Contents (Seed_nonce_revelation _ as op) -> Some op | _ -> None);
        proj = (fun (Seed_nonce_revelation {level; nonce}) -> (level, nonce));
        inj = (fun (level, nonce) -> Seed_nonce_revelation {level; nonce});
      }

  let[@coq_axiom_with_reason "gadt"] endorsement_with_slot_case :
      Kind.endorsement_with_slot case =
    Case
      {
        tag = 10;
        name = "endorsement_with_slot";
        encoding =
          obj2
            (req "endorsement" (dynamic_size endorsement_encoding))
            (req "slot" uint16);
        select =
          (function
          | Contents (Endorsement_with_slot _ as op) -> Some op | _ -> None);
        proj =
          (fun (Endorsement_with_slot {endorsement; slot}) ->
            (endorsement, slot));
        inj =
          (fun (endorsement, slot) -> Endorsement_with_slot {endorsement; slot});
      }

  let[@coq_axiom_with_reason "gadt"] double_endorsement_evidence_case :
      Kind.double_endorsement_evidence case =
    Case
      {
        tag = 2;
        name = "double_endorsement_evidence";
        encoding =
          obj3
            (req "op1" (dynamic_size endorsement_encoding))
            (req "op2" (dynamic_size endorsement_encoding))
            (req "slot" uint16);
        select =
          (function
          | Contents (Double_endorsement_evidence _ as op) -> Some op
          | _ -> None);
        proj =
          (fun (Double_endorsement_evidence {op1; op2; slot}) ->
            (op1, op2, slot));
        inj =
          (fun (op1, op2, slot) -> Double_endorsement_evidence {op1; op2; slot});
      }

  let[@coq_axiom_with_reason "gadt"] double_baking_evidence_case =
    Case
      {
        tag = 3;
        name = "double_baking_evidence";
        encoding =
          obj2
            (req "bh1" (dynamic_size Block_header_repr.encoding))
            (req "bh2" (dynamic_size Block_header_repr.encoding));
        select =
          (function
          | Contents (Double_baking_evidence _ as op) -> Some op | _ -> None);
        proj = (fun (Double_baking_evidence {bh1; bh2}) -> (bh1, bh2));
        inj = (fun (bh1, bh2) -> Double_baking_evidence {bh1; bh2});
      }

  let[@coq_axiom_with_reason "gadt"] activate_account_case =
    Case
      {
        tag = 4;
        name = "activate_account";
        encoding =
          obj2
            (req "pkh" Ed25519.Public_key_hash.encoding)
            (req "secret" Blinded_public_key_hash.activation_code_encoding);
        select =
          (function
          | Contents (Activate_account _ as op) -> Some op | _ -> None);
        proj =
          (fun (Activate_account {id; activation_code}) ->
            (id, activation_code));
        inj =
          (fun (id, activation_code) -> Activate_account {id; activation_code});
      }

  let[@coq_axiom_with_reason "gadt"] proposals_case =
    Case
      {
        tag = 5;
        name = "proposals";
        encoding =
          obj3
            (req "source" Signature.Public_key_hash.encoding)
            (req "period" int32)
            (req "proposals" (list Protocol_hash.encoding));
        select =
          (function Contents (Proposals _ as op) -> Some op | _ -> None);
        proj =
          (fun (Proposals {source; period; proposals}) ->
            (source, period, proposals));
        inj =
          (fun (source, period, proposals) ->
            Proposals {source; period; proposals});
      }

  let[@coq_axiom_with_reason "gadt"] ballot_case =
    Case
      {
        tag = 6;
        name = "ballot";
        encoding =
          obj4
            (req "source" Signature.Public_key_hash.encoding)
            (req "period" int32)
            (req "proposal" Protocol_hash.encoding)
            (req "ballot" Vote_repr.ballot_encoding);
        select = (function Contents (Ballot _ as op) -> Some op | _ -> None);
        proj =
          (function
          | Ballot {source; period; proposal; ballot} ->
              (source, period, proposal, ballot));
        inj =
          (fun (source, period, proposal, ballot) ->
            Ballot {source; period; proposal; ballot});
      }

  let failing_noop_case =
    Case
      {
        tag = 17;
        name = "failing_noop";
        encoding = obj1 (req "arbitrary" Data_encoding.string);
        select =
          (function Contents (Failing_noop _ as op) -> Some op | _ -> None);
        proj =
          (function[@coq_match_with_default] Failing_noop message -> message);
        inj = (function message -> Failing_noop message);
      }

  let manager_encoding =
    obj5
      (req "source" Signature.Public_key_hash.encoding)
      (req "fee" Tez_repr.encoding)
      (req "counter" (check_size 10 n))
      (req "gas_limit" (check_size 10 Gas_limit_repr.Arith.n_integral_encoding))
      (req "storage_limit" (check_size 10 n))

  let extract : type kind. kind Kind.manager contents -> _ =
    function[@coq_match_with_default]
    | Manager_operation
        {source; fee; counter; gas_limit; storage_limit; operation = _} ->
        (source, fee, counter, gas_limit, storage_limit)

  let rebuild (source, fee, counter, gas_limit, storage_limit) operation =
    Manager_operation
      {source; fee; counter; gas_limit; storage_limit; operation}

  let[@coq_axiom_with_reason "gadt"] make_manager_case tag (type kind)
      (Manager_operations.MCase mcase : kind Manager_operations.case) =
    Case
      {
        tag;
        name = mcase.name;
        encoding = merge_objs manager_encoding mcase.encoding;
        select =
          (function
          | Contents (Manager_operation ({operation; _} as op)) -> (
              match mcase.select (Manager operation) with
              | None -> None
              | Some operation -> Some (Manager_operation {op with operation}))
          | _ -> None);
        proj =
          (function
          | Manager_operation {operation; _} as op ->
              (extract op, mcase.proj operation));
        inj = (fun (op, contents) -> rebuild op (mcase.inj contents));
      }

  let reveal_case = make_manager_case 107 Manager_operations.reveal_case

  let transaction_case =
    make_manager_case 108 Manager_operations.transaction_case

  let origination_case =
    make_manager_case 109 Manager_operations.origination_case

  let delegation_case = make_manager_case 110 Manager_operations.delegation_case

  let register_global_constant_case =
    make_manager_case 111 Manager_operations.register_global_constant_case

  let contents_encoding =
    let make (Case {tag; name; encoding; select; proj; inj}) =
      case
        (Tag tag)
        name
        encoding
        (fun o -> match select o with None -> None | Some o -> Some (proj o))
        (fun x -> Contents (inj x))
    in
    def "operation.alpha.contents"
    @@ union
         [
           make endorsement_case;
           make seed_nonce_revelation_case;
           make endorsement_with_slot_case;
           make double_endorsement_evidence_case;
           make double_baking_evidence_case;
           make activate_account_case;
           make proposals_case;
           make ballot_case;
           make reveal_case;
           make transaction_case;
           make origination_case;
           make delegation_case;
           make failing_noop_case;
           make register_global_constant_case;
         ]

  let contents_list_encoding =
    conv_with_guard to_list of_list_internal (Variable.list contents_encoding)

  let optional_signature_encoding =
    conv
      (function Some s -> s | None -> Signature.zero)
      (fun s -> if Signature.equal s Signature.zero then None else Some s)
      Signature.encoding

  let protocol_data_encoding =
    def "operation.alpha.contents_and_signature"
    @@ conv
         (fun (Operation_data {contents; signature}) ->
           (Contents_list contents, signature))
         (fun (Contents_list contents, signature) ->
           Operation_data {contents; signature})
         (obj2
            (req "contents" contents_list_encoding)
            (req "signature" optional_signature_encoding))

  let operation_encoding =
    conv
      (fun {shell; protocol_data} -> (shell, protocol_data))
      (fun (shell, protocol_data) -> {shell; protocol_data})
      (merge_objs Operation.shell_header_encoding protocol_data_encoding)

  let unsigned_operation_encoding =
    def "operation.alpha.unsigned_operation"
    @@ merge_objs
         Operation.shell_header_encoding
         (obj1 (req "contents" contents_list_encoding))

  let internal_operation_encoding =
    def "operation.alpha.internal_operation"
    @@ conv
         (fun (Internal_operation {source; operation; nonce}) ->
           ((source, nonce), Manager operation))
         (fun ((source, nonce), Manager operation) ->
           Internal_operation {source; operation; nonce})
         (merge_objs
            (obj2 (req "source" Contract_repr.encoding) (req "nonce" uint16))
            Manager_operations.encoding)
end

let encoding = Encoding.operation_encoding

let contents_encoding = Encoding.contents_encoding

let contents_list_encoding = Encoding.contents_list_encoding

let protocol_data_encoding = Encoding.protocol_data_encoding

let unsigned_operation_encoding = Encoding.unsigned_operation_encoding

let internal_operation_encoding = Encoding.internal_operation_encoding

let raw ({shell; protocol_data} : _ operation) =
  let proto =
    Data_encoding.Binary.to_bytes_exn
      protocol_data_encoding
      (Operation_data protocol_data)
  in
  {Operation.shell; proto}

let acceptable_passes (op : packed_operation) =
  let (Operation_data protocol_data) = op.protocol_data in
  match protocol_data.contents with
  | Single (Failing_noop _) -> []
  | Single (Endorsement _) -> [0]
  | Single (Endorsement_with_slot _) -> [0]
  | Single (Proposals _) -> [1]
  | Single (Ballot _) -> [1]
  | Single (Seed_nonce_revelation _) -> [2]
  | Single (Double_endorsement_evidence _) -> [2]
  | Single (Double_baking_evidence _) -> [2]
  | Single (Activate_account _) -> [2]
  | Single (Manager_operation _) -> [3]
  | Cons _ -> [3]

type error += Invalid_signature (* `Permanent *)

type error += Missing_signature (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"operation.invalid_signature"
    ~title:"Invalid operation signature"
    ~description:
      "The operation signature is ill-formed or has been made with the wrong \
       public key"
    ~pp:(fun ppf () -> Format.fprintf ppf "The operation signature is invalid")
    Data_encoding.unit
    (function Invalid_signature -> Some () | _ -> None)
    (fun () -> Invalid_signature) ;
  register_error_kind
    `Permanent
    ~id:"operation.missing_signature"
    ~title:"Missing operation signature"
    ~description:
      "The operation is of a kind that must be signed, but the signature is \
       missing"
    ~pp:(fun ppf () -> Format.fprintf ppf "The operation requires a signature")
    Data_encoding.unit
    (function Missing_signature -> Some () | _ -> None)
    (fun () -> Missing_signature) ;
  register_error_kind
    `Permanent
    ~id:"operation.contents_list_error"
    ~title:"Invalid list of operation contents."
    ~description:
      "An operation contents list has an unexpected shape; it should be either \
       a single operation or a non-empty list of manager operations"
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "An operation contents list has an unexpected shape: %s"
        s)
    Data_encoding.(obj1 (req "message" string))
    (function Contents_list_error s -> Some s | _ -> None)
    (fun s -> Contents_list_error s)

let check_signature (type kind) key chain_id
    ({shell; protocol_data} : kind operation) =
  let check ~watermark contents signature =
    let unsigned_operation =
      Data_encoding.Binary.to_bytes_exn
        unsigned_operation_encoding
        (shell, contents)
    in
    if Signature.check ~watermark key signature unsigned_operation then Ok ()
    else error Invalid_signature
  in
  match (protocol_data.contents, protocol_data.signature) with
  | (Single _, None) -> error Missing_signature
  | (Cons _, None) -> error Missing_signature
  | ((Single (Endorsement _) as contents), Some signature) ->
      check ~watermark:(Endorsement chain_id) (Contents_list contents) signature
  | ((Single _ as contents), Some signature) ->
      check ~watermark:Generic_operation (Contents_list contents) signature
  | ((Cons _ as contents), Some signature) ->
      check ~watermark:Generic_operation (Contents_list contents) signature

let hash_raw = Operation.hash

let hash (o : _ operation) =
  let proto =
    Data_encoding.Binary.to_bytes_exn
      protocol_data_encoding
      (Operation_data o.protocol_data)
  in
  Operation.hash {shell = o.shell; proto}

let hash_packed (o : packed_operation) =
  let proto =
    Data_encoding.Binary.to_bytes_exn protocol_data_encoding o.protocol_data
  in
  Operation.hash {shell = o.shell; proto}

type ('a, 'b) eq = Eq : ('a, 'a) eq [@@coq_force_gadt]

let equal_manager_operation_kind :
    type a b. a manager_operation -> b manager_operation -> (a, b) eq option =
 fun op1 op2 ->
  match (op1, op2) with
  | (Reveal _, Reveal _) -> Some Eq
  | (Reveal _, _) -> None
  | (Transaction _, Transaction _) -> Some Eq
  | (Transaction _, _) -> None
  | (Origination _, Origination _) -> Some Eq
  | (Origination _, _) -> None
  | (Delegation _, Delegation _) -> Some Eq
  | (Delegation _, _) -> None
  | (Register_global_constant _, Register_global_constant _) -> Some Eq
  | (Register_global_constant _, _) -> None

let equal_contents_kind : type a b. a contents -> b contents -> (a, b) eq option
    =
 fun op1 op2 ->
  match (op1, op2) with
  | (Endorsement _, Endorsement _) -> Some Eq
  | (Endorsement _, _) -> None
  | (Seed_nonce_revelation _, Seed_nonce_revelation _) -> Some Eq
  | (Seed_nonce_revelation _, _) -> None
  | (Endorsement_with_slot _, Endorsement_with_slot _) -> Some Eq
  | (Endorsement_with_slot _, _) -> None
  | (Double_endorsement_evidence _, Double_endorsement_evidence _) -> Some Eq
  | (Double_endorsement_evidence _, _) -> None
  | (Double_baking_evidence _, Double_baking_evidence _) -> Some Eq
  | (Double_baking_evidence _, _) -> None
  | (Activate_account _, Activate_account _) -> Some Eq
  | (Activate_account _, _) -> None
  | (Proposals _, Proposals _) -> Some Eq
  | (Proposals _, _) -> None
  | (Ballot _, Ballot _) -> Some Eq
  | (Ballot _, _) -> None
  | (Failing_noop _, Failing_noop _) -> Some Eq
  | (Failing_noop _, _) -> None
  | (Manager_operation op1, Manager_operation op2) -> (
      match equal_manager_operation_kind op1.operation op2.operation with
      | None -> None
      | Some Eq -> Some Eq)
  | (Manager_operation _, _) -> None

let rec equal_contents_kind_list :
    type a b. a contents_list -> b contents_list -> (a, b) eq option =
 fun op1 op2 ->
  match (op1, op2) with
  | (Single op1, Single op2) -> equal_contents_kind op1 op2
  | (Single _, Cons _) -> None
  | (Cons _, Single _) -> None
  | (Cons (op1, ops1), Cons (op2, ops2)) -> (
      match equal_contents_kind op1 op2 with
      | None -> None
      | Some Eq -> (
          match equal_contents_kind_list ops1 ops2 with
          | None -> None
          | Some Eq -> Some Eq))

let equal : type a b. a operation -> b operation -> (a, b) eq option =
 fun op1 op2 ->
  if not (Operation_hash.equal (hash op1) (hash op2)) then None
  else
    equal_contents_kind_list
      op1.protocol_data.contents
      op2.protocol_data.contents

open Cache_memory_helpers

let script_lazy_expr_size (expr : Script_repr.lazy_expr) =
  let fun_value expr = ret_adding (expr_size expr) word_size in
  let fun_bytes bytes = (Nodes.zero, word_size +! bytes_size bytes) in
  let fun_combine expr_size bytes_size = expr_size ++ bytes_size in
  ret_adding
    (Data_encoding.apply_lazy ~fun_value ~fun_bytes ~fun_combine expr)
    header_size

let script_repr_size ({code; storage} : Script_repr.t) =
  ret_adding (script_lazy_expr_size code ++ script_lazy_expr_size storage) h2w

let internal_manager_operation_size (type a) (op : a manager_operation) =
  match op with
  | Transaction {amount = _; parameters; entrypoint; destination} ->
      ret_adding
        (script_lazy_expr_size parameters)
        (h4w +! int64_size
        +! string_size_gen (String.length entrypoint)
        +! Contract_repr.in_memory_size destination)
  | Origination {delegate; script; credit = _; preorigination} ->
      ret_adding
        (script_repr_size script)
        (h4w
        +! option_size
             (fun _ -> Contract_repr.public_key_hash_in_memory_size)
             delegate
        +! int64_size
        +! option_size Contract_repr.in_memory_size preorigination)
  | Delegation pkh_opt ->
      ( Nodes.zero,
        h1w
        +! option_size
             (fun _ -> Contract_repr.public_key_hash_in_memory_size)
             pkh_opt )
  | Reveal _ ->
      (* Reveals can't occur as internal operations *)
      assert false
  | Register_global_constant _ ->
      (* Global constant registrations can't occur as internal operations *)
      assert false

let packed_internal_operation_in_memory_size :
    packed_internal_operation -> nodes_and_size = function
  | Internal_operation iop ->
      let {source; operation; nonce = _} = iop in
      let source_size = Contract_repr.in_memory_size source in
      let nonce_size = word_size in
      ret_adding
        (internal_manager_operation_size operation)
        (h2w +! source_size +! nonce_size)
