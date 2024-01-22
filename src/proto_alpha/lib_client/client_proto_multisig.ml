(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

open Protocol_client_context
open Protocol
open Alpha_context
open Michelson_v1_helpers

type error += Contract_has_no_script of Contract_hash.t

type error += Not_a_supported_multisig_contract of Script_expr_hash.t

type error += Contract_has_no_storage of Contract_hash.t

type error += Contract_has_unexpected_storage of Contract_hash.t

type error += Invalid_signature of signature

type error += Not_enough_signatures of int * int

type error += Action_deserialisation_error of Script.expr

type error += Bytes_deserialisation_error of Bytes.t

type error += Bad_deserialized_contract of (Contract_hash.t * Contract_hash.t)

type error += Bad_deserialized_counter of {received : Z.t; expected : Z.t}

type error += Non_positive_threshold of int

type error += Threshold_too_high of int * int

type error += Unsupported_feature_generic_call of Script.expr

type error += Unsupported_feature_generic_call_ty of Script.expr

type error += Unsupported_feature_lambda of string

type error +=
  | Ill_typed_argument of Contract.t * Entrypoint.t * Script.expr * Script.expr

type error += Ill_typed_lambda of Script.expr * Script.expr

let () =
  register_error_kind
    `Permanent
    ~id:"contractHasNoScript"
    ~title:
      "The given contract is not a multisig contract because it has no script"
    ~description:
      "A multisig command has referenced a scriptless smart contract instead \
       of a multisig smart contract."
    ~pp:(fun ppf contract ->
      Format.fprintf ppf "Contract has no script %a." Contract_hash.pp contract)
    Data_encoding.(obj1 (req "contract" Contract.originated_encoding))
    (function Contract_has_no_script c -> Some c | _ -> None)
    (fun c -> Contract_has_no_script c) ;
  register_error_kind
    `Permanent
    ~id:"notASupportedMultisigContract"
    ~title:"The given contract is not one of the supported contracts"
    ~description:
      "A multisig command has referenced a smart contract whose script is not \
       one of the known multisig contract scripts."
    ~pp:(fun ppf hash ->
      Format.fprintf
        ppf
        "Not a supported multisig contract.@\n\
         The hash of this script is %a, it was not found among in the list of \
         known multisig script hashes."
        Script_expr_hash.pp
        hash)
    Data_encoding.(obj1 (req "hash" Script_expr_hash.encoding))
    (function Not_a_supported_multisig_contract h -> Some h | _ -> None)
    (fun h -> Not_a_supported_multisig_contract h) ;
  register_error_kind
    `Permanent
    ~id:"contractHasNoStorage"
    ~title:
      "The given contract is not a multisig contract because it has no storage"
    ~description:
      "A multisig command has referenced a smart contract without storage \
       instead of a multisig smart contract."
    ~pp:(fun ppf contract ->
      Format.fprintf ppf "Contract has no storage %a." Contract_hash.pp contract)
    Data_encoding.(obj1 (req "contract" Contract.originated_encoding))
    (function Contract_has_no_storage c -> Some c | _ -> None)
    (fun c -> Contract_has_no_storage c) ;
  register_error_kind
    `Permanent
    ~id:"contractHasUnexpectedStorage"
    ~title:
      "The storage of the given contract is not of the shape expected for a \
       multisig contract"
    ~description:
      "A multisig command has referenced a smart contract whose storage is of \
       a different shape than the expected one."
    ~pp:(fun ppf contract ->
      Format.fprintf
        ppf
        "Contract has unexpected storage %a."
        Contract_hash.pp
        contract)
    Data_encoding.(obj1 (req "contract" Contract.originated_encoding))
    (function Contract_has_unexpected_storage c -> Some c | _ -> None)
    (fun c -> Contract_has_unexpected_storage c) ;
  register_error_kind
    `Permanent
    ~id:"invalidSignature"
    ~title:
      "The following signature did not match a public key in the given \
       multisig contract"
    ~description:
      "A signature was given for a multisig contract that matched none of the \
       public keys of the contract signers"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "Invalid signature %s." (Signature.to_b58check s))
    Data_encoding.(obj1 (req "invalid_signature" Signature.encoding))
    (function Invalid_signature s -> Some s | _ -> None)
    (fun s -> Invalid_signature s) ;
  register_error_kind
    `Permanent
    ~id:"notEnoughSignatures"
    ~title:"Not enough signatures were provided for this multisig action"
    ~description:
      "To run an action on a multisig contract, you should provide at least as \
       many signatures as indicated by the threshold stored in the multisig \
       contract."
    ~pp:(fun ppf (threshold, nsigs) ->
      Format.fprintf
        ppf
        "Not enough signatures: only %d signatures were given but the \
         threshold is currently %d"
        nsigs
        threshold)
    Data_encoding.(obj1 (req "threshold_nsigs" (tup2 int31 int31)))
    (function
      | Not_enough_signatures (threshold, nsigs) -> Some (threshold, nsigs)
      | _ -> None)
    (fun (threshold, nsigs) -> Not_enough_signatures (threshold, nsigs)) ;
  register_error_kind
    `Permanent
    ~id:"actionDeserialisation"
    ~title:"The expression is not a valid multisig action"
    ~description:
      "When trying to deserialise an action from a sequence of bytes, we got \
       an expression that does not correspond to a known multisig action"
    ~pp:(fun ppf e ->
      Format.fprintf
        ppf
        "Action deserialisation error %a."
        Michelson_v1_printer.print_expr
        e)
    Data_encoding.(obj1 (req "expr" Script.expr_encoding))
    (function Action_deserialisation_error e -> Some e | _ -> None)
    (fun e -> Action_deserialisation_error e) ;
  register_error_kind
    `Permanent
    ~id:"bytesDeserialisation"
    ~title:"The byte sequence is not a valid multisig action"
    ~description:
      "When trying to deserialise an action from a sequence of bytes, we got \
       an error"
    ~pp:(fun ppf b ->
      Format.fprintf ppf "Bytes deserialisation error %s." (Bytes.to_string b))
    Data_encoding.(obj1 (req "expr" bytes))
    (function Bytes_deserialisation_error b -> Some b | _ -> None)
    (fun b -> Bytes_deserialisation_error b) ;
  register_error_kind
    `Permanent
    ~id:"badDeserializedContract"
    ~title:"The byte sequence is not for the given multisig contract"
    ~description:
      "When trying to deserialise an action from a sequence of bytes, we got \
       an action for another multisig contract"
    ~pp:(fun ppf (received, expected) ->
      Format.fprintf
        ppf
        "Bad deserialized contract, received %a expected %a."
        Contract_hash.pp
        received
        Contract_hash.pp
        expected)
    Data_encoding.(
      obj1
        (req
           "received_expected"
           (tup2 Contract.originated_encoding Contract.originated_encoding)))
    (function Bad_deserialized_contract b -> Some b | _ -> None)
    (fun b -> Bad_deserialized_contract b) ;
  register_error_kind
    `Permanent
    ~id:"Bad deserialized counter"
    ~title:"Deserialized counter does not match the stored one"
    ~description:
      "The byte sequence references a multisig counter that does not match the \
       one currently stored in the given multisig contract"
    ~pp:(fun ppf (received, expected) ->
      Format.fprintf
        ppf
        "Bad deserialized counter, received %d expected %d."
        received
        expected)
    Data_encoding.(obj1 (req "received_expected" (tup2 int31 int31)))
    (function
      | Bad_deserialized_counter {received; expected} ->
          Some (Z.to_int received, Z.to_int expected)
      | _ -> None)
    (fun (received, expected) ->
      Bad_deserialized_counter
        {received = Z.of_int received; expected = Z.of_int expected}) ;
  register_error_kind
    `Permanent
    ~id:"thresholdTooHigh"
    ~title:"Given threshold is too high"
    ~description:
      "The given threshold is higher than the number of keys, this would lead \
       to a frozen multisig contract"
    ~pp:(fun ppf (threshold, nkeys) ->
      Format.fprintf
        ppf
        "Threshold too high: %d expected at most %d."
        threshold
        nkeys)
    Data_encoding.(obj1 (req "received_expected" (tup2 int31 int31)))
    (function Threshold_too_high (c1, c2) -> Some (c1, c2) | _ -> None)
    (fun (c1, c2) -> Threshold_too_high (c1, c2)) ;
  register_error_kind
    `Permanent
    ~id:"nonPositiveThreshold"
    ~title:"Given threshold is not positive"
    ~description:"A multisig threshold should be a positive number"
    ~pp:(fun ppf threshold ->
      Format.fprintf ppf "Multisig threshold %d should be positive." threshold)
    Data_encoding.(obj1 (req "threshold" int31))
    (function Non_positive_threshold t -> Some t | _ -> None)
    (fun t -> Non_positive_threshold t) ;
  register_error_kind
    `Permanent
    ~id:"unsupportedGenericMultisigFeature"
    ~title:"Unsupported multisig feature: generic call"
    ~description:
      "This multisig contract does not feature calling contracts with arguments"
    ~pp:(fun ppf arg ->
      Format.fprintf
        ppf
        "This multisig contract can only transfer tokens to contracts of type \
         unit; calling a contract with argument %a is not supported."
        Michelson_v1_printer.print_expr
        arg)
    Data_encoding.(obj1 (req "arg" Script.expr_encoding))
    (function Unsupported_feature_generic_call arg -> Some arg | _ -> None)
    (fun arg -> Unsupported_feature_generic_call arg) ;
  register_error_kind
    `Permanent
    ~id:"unsupportedGenericMultisigFeatureTy"
    ~title:"Unsupported multisig feature: generic call to non-unit entrypoint"
    ~description:
      "This multisig contract does not feature calling contracts with arguments"
    ~pp:(fun ppf ty ->
      Format.fprintf
        ppf
        "This multisig contract can only transfer tokens to contracts of type \
         unit; calling a contract of type %a is not supported."
        Michelson_v1_printer.print_expr
        ty)
    Data_encoding.(obj1 (req "ty" Script.expr_encoding))
    (function Unsupported_feature_generic_call_ty ty -> Some ty | _ -> None)
    (fun ty -> Unsupported_feature_generic_call_ty ty) ;
  register_error_kind
    `Permanent
    ~id:"unsupportedGenericMultisigLambda"
    ~title:"Unsupported multisig feature: running lambda"
    ~description:"This multisig contract does not feature running lambdas"
    ~pp:(fun ppf lam ->
      Format.fprintf
        ppf
        "This multisig contract has a fixed set of actions, it cannot run the \
         following lambda: %s."
        lam)
    Data_encoding.(obj1 (req "lam" string))
    (function Unsupported_feature_lambda lam -> Some lam | _ -> None)
    (fun lam -> Unsupported_feature_lambda lam) ;
  register_error_kind
    `Permanent
    ~id:"illTypedArgumentForMultisig"
    ~title:"Ill-typed argument in multi-signed transfer"
    ~description:
      "The provided argument for a transfer from a multisig contract is \
       ill-typed"
    ~pp:(fun ppf (destination, entrypoint, parameter_ty, parameter) ->
      Format.fprintf
        ppf
        "The entrypoint %a of contract %a called from a multisig contract is \
         of type %a; the provided parameter %a is ill-typed."
        Entrypoint.pp
        entrypoint
        Contract.pp
        destination
        Michelson_v1_printer.print_expr
        parameter_ty
        Michelson_v1_printer.print_expr
        parameter)
    Data_encoding.(
      obj4
        (req "destination" Contract.encoding)
        (req "entrypoint" Entrypoint.simple_encoding)
        (req "parameter_ty" Script.expr_encoding)
        (req "parameter" Script.expr_encoding))
    (function
      | Ill_typed_argument (destination, entrypoint, parameter_ty, parameter) ->
          Some (destination, entrypoint, parameter_ty, parameter)
      | _ -> None)
    (fun (destination, entrypoint, parameter_ty, parameter) ->
      Ill_typed_argument (destination, entrypoint, parameter_ty, parameter)) ;
  register_error_kind
    `Permanent
    ~id:"illTypedLambdaForMultisig"
    ~title:"Ill-typed lambda for multi-signed transfer"
    ~description:
      "The provided lambda for a transfer from a multisig contract is ill-typed"
    ~pp:(fun ppf (lam, exp) ->
      Format.fprintf
        ppf
        "The provided lambda %a for multisig contract is ill-typed; %a is \
         expected."
        Michelson_v1_printer.print_expr
        lam
        Michelson_v1_printer.print_expr
        exp)
    Data_encoding.(
      obj2 (req "lam" Script.expr_encoding) (req "exp" Script.expr_encoding))
    (function Ill_typed_lambda (lam, exp) -> Some (lam, exp) | _ -> None)
    (fun (lam, exp) -> Ill_typed_lambda (lam, exp))

(* The multisig contract script written by Arthur Breitman
     https://github.com/murbard/smart-contracts/blob/abdb582d8f1fe7ba7eb15975867d8862cb70acfe/multisig/michelson/generic.tz *)
let multisig_script_string =
  {|
parameter (or (unit %default)
              (pair %main
                 (pair :payload
                    (nat %counter) # counter, used to prevent replay attacks
                    (or :action    # payload to sign, represents the requested action
                       (lambda %operation unit (list operation))
                       (pair %change_keys          # change the keys controlling the multisig
                          (nat %threshold)         # new threshold
                          (list %keys key))))     # new list of keys
                 (list %sigs (option signature))));    # signatures

storage (pair (nat %stored_counter) (pair (nat %threshold) (list %keys key))) ;

code
  {
    UNPAIR ;
    IF_LEFT
      { # Default entry point: do nothing
        # This entry point can be used to send tokens to this contract
        DROP ; NIL operation ; PAIR }
      { # Main entry point
        # Assert no token was sent:
        # to send tokens, the default entry point should be used
        PUSH mutez 0 ; AMOUNT ; ASSERT_CMPEQ ;
        SWAP ; DUP ; DIP { SWAP } ;
        DIP
          {
            UNPAIR ;
            # pair the payload with the current contract address, to ensure signatures
            # can't be replayed across different contracts if a key is reused.
            DUP ; SELF ; ADDRESS ; CHAIN_ID ; PAIR ; PAIR ;
            PACK ; # form the binary payload that we expect to be signed
            DIP { UNPAIR @counter ; DIP { SWAP } } ; SWAP
          } ;

        # Check that the counters match
        UNPAIR @stored_counter; DIP { SWAP };
        ASSERT_CMPEQ ;

        # Compute the number of valid signatures
        DIP { SWAP } ; UNPAIR @threshold @keys;
        DIP
          {
            # Running count of valid signatures
            PUSH @valid nat 0; SWAP ;
            ITER
              {
                DIP { SWAP } ; SWAP ;
                IF_CONS
                  {
                    IF_SOME
                      { SWAP ;
                        DIP
                          {
                            SWAP ; DIP 2 { DUP 2 } ;
                            # Checks signatures, fails if invalid
                            { DUP 3; DIP {CHECK_SIGNATURE}; SWAP; IF {DROP} {FAILWITH} };
                            PUSH nat 1 ; ADD @valid } }
                      { SWAP ; DROP }
                  }
                  {
                    # There were fewer signatures in the list
                    # than keys. Not all signatures must be present, but
                    # they should be marked as absent using the option type.
                    FAIL
                  } ;
                SWAP
              }
          } ;
        # Assert that the threshold is less than or equal to the
        # number of valid signatures.
        ASSERT_CMPLE ;
        # Assert no unchecked signature remains
        IF_CONS {FAIL} {} ;
        DROP ;

        # Increment counter and place in storage
        DIP { UNPAIR ; PUSH nat 1 ; ADD @new_counter ; PAIR} ;

        # We have now handled the signature verification part,
        # produce the operation requested by the signers.
        IF_LEFT
          { # Get operation
            UNIT ; EXEC
          }
          {
            # Change set of signatures
            DIP { CAR } ; SWAP ; PAIR ; NIL operation
          };
        PAIR }
  }
|}

(* Client_proto_context.originate expects the contract script as a Script.expr *)
let multisig_script : Script.expr =
  Michelson_v1_parser.parse_toplevel ?check:(Some true) multisig_script_string
  |> Tezos_micheline.Micheline_parser.no_parsing_error
  |> function
  | Error _ ->
      assert false
      (* This is a top level assertion, it is asserted when the client's process runs. *)
  | Ok parsing_result -> parsing_result.Michelson_v1_parser.expanded

let multisig_script_hash =
  let bytes =
    Data_encoding.Binary.to_bytes_exn Script.expr_encoding multisig_script
  in
  Script_expr_hash.hash_bytes [bytes]

(* The previous multisig script is the only one that the client can
   originate but the client knows how to interact with several
   versions of the multisig contract. For each version, the description
   indicates which features are available and how to interact with
   the contract. *)

type multisig_contract_description = {
  hash : Script_expr_hash.t;
  (* The hash of the contract script *)
  requires_chain_id : bool;
  (* The signatures should contain the chain identifier *)
  main_entrypoint : Entrypoint.t option;
  (* name of the main entrypoint of the multisig contract, None means use the default entrypoint *)
  generic : bool;
      (* False means that the contract uses a custom action type, true
                       means that the contract expects the action as a (lambda unit
                       (list operation)). *)
}

let entrypoint_main = Entrypoint.of_string_strict_exn "main"

(* List of known multisig contracts hashes with their kinds *)
let known_multisig_contracts : multisig_contract_description list =
  [
    {
      (* First supported version of the generic multisig contract. Supports incoming
         transfers from unauthenticated senders and outgoing transfers of
         arbitrary operation lists.

         See docs/user/multisig.rst for more details. *)
      hash = multisig_script_hash;
      requires_chain_id = true;
      main_entrypoint = Some entrypoint_main;
      generic = true;
    };
    {
      (* Fourth supported version of the legacy multisig contract. This script is
         functionally equivalent to the third version but uses the [DUP 2]
         instruction introduced in Edo instead of the macro for [DIG 2; DUP; DUG 3]. *)
      hash =
        Script_expr_hash.of_b58check_exn
          "exprutz4BVGJ3Qms6qjmqvUF8sEk27H1cfqhRT17qpTdhEs5hEjbWm";
      requires_chain_id = true;
      main_entrypoint = None;
      generic = false;
    };
    {
      (* Third supported version of the legacy multisig contract. This script is
         functionally equivalent to the second version but uses the [DIP 2]
         instruction introduced in Babylon instead of the [DIIP] macro. *)
      hash =
        Script_expr_hash.of_b58check_exn
          "exprumpS39YZd26Cn4kyKUK5ezTR3at838iGWg7i6uETv8enDeAnfb";
      requires_chain_id = true;
      main_entrypoint = None;
      generic = false;
    };
    {
      (* Second supported version of the legacy multisig contract. This script
         is the one resulting from the stitching of the Babylon protocol, the
         only difference with the first version is that the chain id is part of
         the data to sign. *)
      hash =
        Script_expr_hash.of_b58check_exn
          "exprtw1v4KvQN414oEXdGuA1U3eQizuCdS8cipx8QGK8TbNLRwc3qL";
      requires_chain_id = true;
      main_entrypoint = None;
      generic = false;
    };
    {
      (* First supported version of the legacy multisig contract. This script should not
         be used anymore because it is subject to a small replay attack: when
         the test chain is forked both instances have the same address and
         counter so whatever happens on the test chain can be replayed on the
         main chain. The script has been fixed during the activation of the
         Babylon protocol. *)
      hash =
        Script_expr_hash.of_b58check_exn
          "expru4Ju9kf6MQ216FxUEsb9P6j8UhkPtsFcYP8r9XhQSRb47FZGfM";
      requires_chain_id = false;
      main_entrypoint = None;
      generic = false;
    };
  ]

let known_multisig_hashes =
  List.map (fun descr -> descr.hash) known_multisig_contracts

let check_multisig_script_hash hash :
    multisig_contract_description tzresult Lwt.t =
  let open Lwt_result_syntax in
  match
    List.find_opt
      (fun d -> Script_expr_hash.(d.hash = hash))
      known_multisig_contracts
  with
  | None -> tzfail (Not_a_supported_multisig_contract hash)
  | Some d -> return d

(* Returns [Ok ()] if [~contract] is an originated contract whose code
   is [multisig_script] *)
let check_multisig_contract (cctxt : #Protocol_client_context.full) ~chain
    ~block contract =
  let open Lwt_result_syntax in
  let* hash_opt =
    Client_proto_context.get_script_hash cctxt ~chain ~block contract
  in
  match hash_opt with
  | None -> tzfail (Contract_has_no_script contract)
  | Some hash -> check_multisig_script_hash hash

(* Some Michelson building functions, specific to the needs of the multisig
   interface.*)

(* The type of the lambdas consumed by the generic script *)
let lambda_action_t ~loc = lambda_t ~loc (unit_t ~loc) (operations_t ~loc)

(* Conversion functions from common types to Script_expr using the optimized representation *)
let mutez ~loc (amount : Tez.t) = int ~loc (Z.of_int64 (Tez.to_mutez amount))

let optimized_key_hash ~loc (key_hash : Signature.Public_key_hash.t) =
  bytes
    ~loc
    (Data_encoding.Binary.to_bytes_exn
       Signature.Public_key_hash.encoding
       key_hash)

let optimized_address ~loc ~(address : Contract.t) ~(entrypoint : Entrypoint.t)
    =
  bytes
    ~loc
    (Data_encoding.Binary.to_bytes_exn
       Data_encoding.(tup2 Contract.encoding Entrypoint.value_encoding)
       (address, entrypoint))

let optimized_key ~loc (key : Signature.Public_key.t) =
  bytes
    ~loc
    (Data_encoding.Binary.to_bytes_exn Signature.Public_key.encoding key)

(** * Actions *)

type multisig_action =
  | Transfer of {
      amount : Tez.t;
      destination : Contract.t;
      entrypoint : Entrypoint.t;
      parameter_type : Script.expr;
      parameter : Script.expr;
    }
  | Change_delegate of public_key_hash option
  | Lambda of Script.expr
  | Change_keys of Z.t * public_key list

let action_to_expr_generic ~loc =
  let open Result_syntax in
  function
  | Transfer {amount; destination; entrypoint; parameter_type; parameter} -> (
      match destination with
      | Implicit destination ->
          let* a =
            lambda_from_string
            @@ Managed_contract.build_lambda_for_transfer_to_implicit
                 ~destination
                 ~amount
          in
          return @@ left ~loc a
      | Originated destination ->
          let* a =
            lambda_from_string
            @@ Managed_contract.build_lambda_for_transfer_to_originated
                 ~destination
                 ~entrypoint
                 ~parameter_type
                 ~parameter
                 ~amount
          in
          return @@ left ~loc a)
  | Lambda code -> return Tezos_micheline.Micheline.(left ~loc (root code))
  | Change_delegate delegate ->
      let* a =
        lambda_from_string
        @@ Managed_contract.build_lambda_for_set_delegate ~delegate
      in
      return @@ left ~loc a
  | Change_keys (threshold, keys) ->
      let optimized_keys = seq ~loc (List.map (optimized_key ~loc) keys) in
      let expr = right ~loc (pair ~loc (int ~loc threshold) optimized_keys) in
      return expr

let action_to_expr_legacy ~loc =
  let open Result_syntax in
  function
  | Transfer {amount; destination; entrypoint; parameter_type; parameter} ->
      if parameter <> Tezos_micheline.Micheline.strip_locations (unit ~loc:())
      then tzfail @@ Unsupported_feature_generic_call parameter
      else if
        parameter_type
        <> Tezos_micheline.Micheline.strip_locations (unit_t ~loc:())
      then tzfail @@ Unsupported_feature_generic_call_ty parameter_type
      else
        return
        @@ left
             ~loc
             (pair
                ~loc
                (mutez ~loc amount)
                (optimized_address ~loc ~address:destination ~entrypoint))
  | Lambda _ -> tzfail @@ Unsupported_feature_lambda ""
  | Change_delegate delegate ->
      let delegate_opt =
        match delegate with
        | None -> none ~loc ()
        | Some delegate -> some ~loc (optimized_key_hash ~loc delegate)
      in
      return @@ right ~loc (left ~loc delegate_opt)
  | Change_keys (threshold, keys) ->
      let optimized_keys = seq ~loc (List.map (optimized_key ~loc) keys) in
      let expr = right ~loc (pair ~loc (int ~loc threshold) optimized_keys) in
      return (right ~loc expr)

let action_to_expr ~loc ~generic action =
  if generic then action_to_expr_generic ~loc action
  else action_to_expr_legacy ~loc action

let action_of_expr_generic e =
  let open Lwt_result_syntax in
  let fail () =
    tzfail
      (Action_deserialisation_error
         (Tezos_micheline.Micheline.strip_locations e))
  in
  match e with
  | Tezos_micheline.Micheline.Prim (_, Script.D_Left, [lam], []) ->
      return (Lambda (Tezos_micheline.Micheline.strip_locations lam))
  | Tezos_micheline.Micheline.Prim
      ( _,
        Script.D_Right,
        [
          Tezos_micheline.Micheline.Prim
            ( _,
              Script.D_Pair,
              [
                Tezos_micheline.Micheline.Int (_, threshold);
                Tezos_micheline.Micheline.Seq (_, key_bytes);
              ],
              [] );
        ],
        [] ) ->
      let* keys =
        List.map_es
          (function
            | Tezos_micheline.Micheline.Bytes (_, s) ->
                return
                @@ Data_encoding.Binary.of_bytes_exn
                     Signature.Public_key.encoding
                     s
            | _ -> fail ())
          key_bytes
      in
      return (Change_keys (threshold, keys))
  | _ -> fail ()

let action_of_expr_not_generic e =
  let open Lwt_result_syntax in
  let fail () =
    tzfail
      (Action_deserialisation_error
         (Tezos_micheline.Micheline.strip_locations e))
  in
  match e with
  | Tezos_micheline.Micheline.Prim
      ( _,
        Script.D_Left,
        [
          Tezos_micheline.Micheline.Prim
            ( _,
              Script.D_Pair,
              [
                Tezos_micheline.Micheline.Int (_, i);
                Tezos_micheline.Micheline.Bytes (_, s);
              ],
              [] );
        ],
        [] ) -> (
      match Tez.of_mutez (Z.to_int64 i) with
      | None -> fail ()
      | Some amount ->
          return
          @@ Transfer
               {
                 amount;
                 destination =
                   Data_encoding.Binary.of_bytes_exn Contract.encoding s;
                 entrypoint = Entrypoint.default;
                 parameter_type =
                   Tezos_micheline.Micheline.strip_locations @@ unit_t ~loc:();
                 parameter =
                   Tezos_micheline.Micheline.strip_locations @@ unit ~loc:();
               })
  | Tezos_micheline.Micheline.Prim
      ( _,
        Script.D_Right,
        [
          Tezos_micheline.Micheline.Prim
            ( _,
              Script.D_Left,
              [Tezos_micheline.Micheline.Prim (_, Script.D_None, [], [])],
              [] );
        ],
        [] ) ->
      return (Change_delegate None)
  | Tezos_micheline.Micheline.Prim
      ( _,
        Script.D_Right,
        [
          Tezos_micheline.Micheline.Prim
            ( _,
              Script.D_Left,
              [
                Tezos_micheline.Micheline.Prim
                  ( _,
                    Script.D_Some,
                    [Tezos_micheline.Micheline.Bytes (_, s)],
                    [] );
              ],
              [] );
        ],
        [] ) ->
      return
      @@ Change_delegate
           (Some
              (Data_encoding.Binary.of_bytes_exn
                 Signature.Public_key_hash.encoding
                 s))
  | Tezos_micheline.Micheline.Prim
      ( _,
        Script.D_Right,
        [
          Tezos_micheline.Micheline.Prim
            ( _,
              Script.D_Right,
              [
                Tezos_micheline.Micheline.Prim
                  ( _,
                    Script.D_Pair,
                    [
                      Tezos_micheline.Micheline.Int (_, threshold);
                      Tezos_micheline.Micheline.Seq (_, key_bytes);
                    ],
                    [] );
              ],
              [] );
        ],
        [] ) ->
      let* keys =
        List.map_es
          (function
            | Tezos_micheline.Micheline.Bytes (_, s) ->
                return
                @@ Data_encoding.Binary.of_bytes_exn
                     Signature.Public_key.encoding
                     s
            | _ -> fail ())
          key_bytes
      in
      return (Change_keys (threshold, keys))
  | _ -> fail ()

let action_of_expr ~generic =
  if generic then action_of_expr_generic else action_of_expr_not_generic

type key_list = Signature.Public_key.t list

(* The relevant information that we can get about a multisig smart contract *)
type multisig_contract_information = {
  counter : Z.t;
  threshold : Z.t;
  keys : key_list;
}

let multisig_get_information (cctxt : #Protocol_client_context.full) ~chain
    ~block contract =
  let open Client_proto_context in
  let open Tezos_micheline.Micheline in
  let open Lwt_result_syntax in
  let* storage_opt =
    get_storage cctxt ~chain ~block ~unparsing_mode:Readable contract
  in
  match storage_opt with
  | None -> tzfail (Contract_has_no_storage contract)
  | Some storage -> (
      match root storage with
      | Prim
          ( _,
            D_Pair,
            [Int (_, counter); Int (_, threshold); Seq (_, key_nodes)],
            _ ) ->
          let* keys =
            List.map_es
              (function
                | String (_, key_str) ->
                    return @@ Signature.Public_key.of_b58check_exn key_str
                | _ -> tzfail (Contract_has_unexpected_storage contract))
              key_nodes
          in
          return {counter; threshold; keys}
      | _ -> tzfail (Contract_has_unexpected_storage contract))

let multisig_create_storage ~counter ~threshold ~keys () :
    Script.expr tzresult Lwt.t =
  let open Tezos_micheline.Micheline in
  let open Lwt_result_syntax in
  let loc = Tezos_micheline.Micheline_parser.location_zero in
  let* l =
    List.map_es
      (fun key ->
        let key_str = Signature.Public_key.to_b58check key in
        return (String (loc, key_str)))
      keys
  in
  return @@ strip_locations
  @@ pair ~loc (int ~loc counter) (pair ~loc (int ~loc threshold) (seq ~loc l))

(* Client_proto_context.originate expects the initial storage as a string *)
let multisig_storage_string ~counter ~threshold ~keys () =
  let open Lwt_result_syntax in
  let* expr = multisig_create_storage ~counter ~threshold ~keys () in
  return @@ Format.asprintf "%a" Michelson_v1_printer.print_expr expr

let multisig_create_param ~counter ~generic ~action ~optional_signatures () :
    Script.expr tzresult Lwt.t =
  let open Tezos_micheline.Micheline in
  let open Lwt_result_syntax in
  let loc = 0 in
  let* l =
    List.map_es
      (fun sig_opt ->
        match sig_opt with
        | None -> return @@ none ~loc ()
        | Some signature ->
            return @@ some ~loc (String (loc, Signature.to_b58check signature)))
      optional_signatures
  in
  let*? expr = action_to_expr ~loc ~generic action in
  return @@ strip_locations
  @@ pair ~loc (pair ~loc (int ~loc counter) expr) (Seq (loc, l))

let multisig_param ~counter ~action ~optional_signatures ~generic () =
  let open Lwt_result_syntax in
  let* expr =
    multisig_create_param ~counter ~action ~optional_signatures ~generic ()
  in
  return @@ Script.lazy_expr expr

let get_contract_address_maybe_chain_id ~descr ~loc ~chain_id contract =
  let address =
    bytes ~loc (Data_encoding.Binary.to_bytes_exn Contract.encoding contract)
  in
  if descr.requires_chain_id then
    let chain_id_bytes =
      bytes ~loc (Data_encoding.Binary.to_bytes_exn Chain_id.encoding chain_id)
    in
    pair ~loc chain_id_bytes address
  else address

let multisig_bytes ~counter ~action ~contract ~chain_id ~descr () =
  let open Lwt_result_syntax in
  let loc = 0 in
  let*? expr = action_to_expr ~loc ~generic:descr.generic action in
  let triple =
    pair
      ~loc
      (get_contract_address_maybe_chain_id ~descr ~loc ~chain_id contract)
      (pair ~loc (int ~loc counter) expr)
  in
  let bytes =
    Data_encoding.Binary.to_bytes_exn Script.expr_encoding
    @@ Tezos_micheline.Micheline.strip_locations @@ triple
  in
  return @@ Bytes.cat (Bytes.of_string "\005") bytes

let check_threshold ~threshold ~keys () =
  let open Lwt_result_syntax in
  let threshold = Z.to_int threshold in
  if Compare.List_length_with.(keys < threshold) then
    tzfail (Threshold_too_high (threshold, List.length keys))
  else if Compare.Int.(threshold <= 0) then
    tzfail (Non_positive_threshold threshold)
  else return_unit

let originate_multisig (cctxt : #Protocol_client_context.full) ~chain ~block
    ?confirmations ?dry_run ?branch ?fee ?gas_limit ?storage_limit
    ?verbose_signing ~delegate ~threshold ~keys ~balance ~source ~src_pk ~src_sk
    ~fee_parameter () =
  let open Lwt_result_syntax in
  let* initial_storage =
    multisig_storage_string ~counter:Z.zero ~threshold ~keys ()
  in
  let* () = check_threshold ~threshold ~keys () in
  Client_proto_context.originate_contract
    cctxt
    ~chain
    ~block
    ?branch
    ?confirmations
    ?dry_run
    ?fee
    ?gas_limit
    ?storage_limit
    ?verbose_signing
    ~delegate
    ~initial_storage
    ~balance
    ~source
    ~src_pk
    ~src_sk
    ~code:multisig_script
    ~fee_parameter
    ()

type multisig_prepared_action = {
  bytes : Bytes.t;
  threshold : Z.t;
  keys : public_key list;
  counter : Z.t;
  entrypoint : Entrypoint.t option;
  generic : bool;
}

let check_parameter_type (cctxt : #Protocol_client_context.full) ~gas ~legacy
    ~destination ~entrypoint ~parameter_type ~parameter () =
  let open Lwt_result_syntax in
  let* _ =
    trace
      (Ill_typed_argument (destination, entrypoint, parameter_type, parameter))
    @@ Plugin.RPC.Scripts.typecheck_data
         cctxt
         (cctxt#chain, cctxt#block)
         ~data:parameter
         ~ty:parameter_type
         ~gas
         ~legacy
  in
  return_unit

let check_action (cctxt : #Protocol_client_context.full) ~action ~balance ~gas
    ~legacy () =
  let open Lwt_result_syntax in
  match action with
  | Change_keys (threshold, keys) ->
      let* () = check_threshold ~threshold ~keys () in
      return_unit
  | Transfer {amount; destination; entrypoint; parameter_type; parameter} ->
      let* () =
        check_parameter_type
          cctxt
          ~destination
          ~entrypoint
          ~parameter_type
          ~parameter
          ~gas:None
          ~legacy:false
          ()
      in
      if Tez.(amount > balance) then
        (* This is warning only because the contract can be filled
           before sending the signatures or even in the same
           transaction *)
        Format.eprintf
          "Transferred amount is bigger than current multisig balance" ;
      return_unit
  | Lambda code ->
      let action_t =
        Tezos_micheline.Micheline.strip_locations (lambda_action_t ~loc:())
      in
      let* _remaining_gas =
        trace (Ill_typed_lambda (code, action_t))
        @@ Plugin.RPC.Scripts.typecheck_data
             cctxt
             (cctxt#chain, cctxt#block)
             ~data:code
             ~ty:action_t
             ~gas
             ~legacy
      in
      return_unit
  | _ -> return_unit

let prepare_multisig_transaction (cctxt : #Protocol_client_context.full) ~chain
    ~block ~multisig_contract ~action () =
  let open Lwt_result_syntax in
  let contract = multisig_contract in
  let* descr = check_multisig_contract cctxt ~chain ~block contract in
  let* {counter; threshold; keys} =
    multisig_get_information cctxt ~chain ~block contract
  in
  let* chain_id = Chain_services.chain_id cctxt ~chain () in
  let contract = Contract.Originated contract in
  let* bytes = multisig_bytes ~counter ~action ~contract ~descr ~chain_id () in
  let* balance =
    Client_proto_context.get_balance
      cctxt
      ~chain:cctxt#chain
      ~block:cctxt#block
      contract
  in
  let* () = check_action cctxt ~action ~balance ~gas:None ~legacy:false () in
  return
    {
      bytes;
      threshold;
      keys;
      counter;
      entrypoint = descr.main_entrypoint;
      generic = descr.generic;
    }

let check_multisig_signatures ~bytes ~threshold ~keys signatures =
  let open Lwt_result_syntax in
  let key_array = Array.of_list keys in
  let nkeys = Array.length key_array in
  let opt_sigs_arr = Array.make nkeys None in
  let matching_key_found = ref false in
  let check_signature_against_key_number signature i key =
    if Signature.check key signature bytes then (
      matching_key_found := true ;
      opt_sigs_arr.(i) <- Some signature)
  in
  let* () =
    List.iter_ep
      (fun signature ->
        matching_key_found := false ;
        List.iteri (check_signature_against_key_number signature) keys ;
        fail_unless !matching_key_found (Invalid_signature signature))
      signatures
  in
  let opt_sigs = Array.to_list opt_sigs_arr in
  let signature_count =
    List.fold_left
      (fun n sig_opt -> match sig_opt with Some _ -> n + 1 | None -> n)
      0
      opt_sigs
  in
  let threshold_int = Z.to_int threshold in
  if signature_count >= threshold_int then return opt_sigs
  else tzfail (Not_enough_signatures (threshold_int, signature_count))

let call_multisig (cctxt : #Protocol_client_context.full) ~chain ~block
    ?confirmations ?dry_run ?verbose_signing ?branch ~source ~src_pk ~src_sk
    ~multisig_contract ~action ~signatures ~amount ?fee ?gas_limit
    ?storage_limit ?counter ~fee_parameter () =
  let open Lwt_result_syntax in
  let* {bytes; threshold; keys; counter = stored_counter; entrypoint; generic} =
    prepare_multisig_transaction
      cctxt
      ~chain
      ~block
      ~multisig_contract
      ~action
      ()
  in
  let* optional_signatures =
    check_multisig_signatures ~bytes ~threshold ~keys signatures
  in
  let* parameters =
    multisig_param
      ~counter:stored_counter
      ~action
      ~optional_signatures
      ~generic
      ()
  in
  Client_proto_context.transfer_with_script
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?branch
    ~source
    ~src_pk
    ~src_sk
    ~destination:(Originated multisig_contract)
    ?entrypoint
    ~parameters
    ~amount
    ?fee
    ?gas_limit
    ?storage_limit
    ?counter
    ~fee_parameter
    ?verbose_signing
    ()

let action_of_bytes ~multisig_contract ~stored_counter ~descr ~chain_id bytes =
  let open Lwt_result_syntax in
  if
    Compare.Int.(Bytes.length bytes >= 1)
    && Compare.Int.(TzEndian.get_uint8 bytes 0 = 0x05)
  then
    let nbytes = Bytes.sub bytes 1 (Bytes.length bytes - 1) in
    match Data_encoding.Binary.of_bytes_opt Script.expr_encoding nbytes with
    | None -> tzfail (Bytes_deserialisation_error bytes)
    | Some e -> (
        match Tezos_micheline.Micheline.root e with
        | Tezos_micheline.Micheline.Prim
            ( _,
              Script.D_Pair,
              [
                Tezos_micheline.Micheline.Bytes (_, contract_bytes);
                Tezos_micheline.Micheline.Prim
                  ( _,
                    Script.D_Pair,
                    [Tezos_micheline.Micheline.Int (_, counter); e],
                    [] );
              ],
              [] )
          when not descr.requires_chain_id ->
            let contract =
              Data_encoding.Binary.of_bytes_exn
                Contract.originated_encoding
                contract_bytes
            in
            if counter = stored_counter then
              if Contract_hash.(multisig_contract = contract) then
                action_of_expr ~generic:descr.generic e
              else
                tzfail (Bad_deserialized_contract (contract, multisig_contract))
            else
              tzfail
                (Bad_deserialized_counter
                   {received = counter; expected = stored_counter})
        | Tezos_micheline.Micheline.Prim
            ( _,
              Script.D_Pair,
              [
                Tezos_micheline.Micheline.Prim
                  ( _,
                    Script.D_Pair,
                    [
                      Tezos_micheline.Micheline.Bytes (_, chain_id_bytes);
                      Tezos_micheline.Micheline.Bytes (_, contract_bytes);
                    ],
                    [] );
                Tezos_micheline.Micheline.Prim
                  ( _,
                    Script.D_Pair,
                    [Tezos_micheline.Micheline.Int (_, counter); e],
                    [] );
              ],
              [] )
          when descr.requires_chain_id ->
            let contract =
              Data_encoding.Binary.of_bytes_exn
                Contract.originated_encoding
                contract_bytes
            in
            let cid =
              Data_encoding.Binary.of_bytes_exn Chain_id.encoding chain_id_bytes
            in
            if counter = stored_counter then
              if multisig_contract = contract && chain_id = cid then
                action_of_expr ~generic:descr.generic e
              else
                tzfail (Bad_deserialized_contract (contract, multisig_contract))
            else
              tzfail
                (Bad_deserialized_counter
                   {received = counter; expected = stored_counter})
        | _ -> tzfail (Bytes_deserialisation_error bytes))
  else tzfail (Bytes_deserialisation_error bytes)

let call_multisig_on_bytes (cctxt : #Protocol_client_context.full) ~chain ~block
    ?confirmations ?dry_run ?verbose_signing ?branch ~source ~src_pk ~src_sk
    ~multisig_contract ~bytes ~signatures ~amount ?fee ?gas_limit ?storage_limit
    ?counter ~fee_parameter () =
  let open Lwt_result_syntax in
  let* info = multisig_get_information cctxt ~chain ~block multisig_contract in
  let* descr = check_multisig_contract cctxt ~chain ~block multisig_contract in
  let* chain_id = Chain_services.chain_id cctxt ~chain () in
  let* action =
    action_of_bytes
      ~multisig_contract
      ~stored_counter:info.counter
      ~chain_id
      ~descr
      bytes
  in
  call_multisig
    cctxt
    ~chain
    ~block
    ?confirmations
    ?dry_run
    ?branch
    ~source
    ~src_pk
    ~src_sk
    ~multisig_contract
    ~action
    ~signatures
    ~amount
    ?fee
    ?gas_limit
    ?storage_limit
    ?counter
    ~fee_parameter
    ?verbose_signing
    ()
