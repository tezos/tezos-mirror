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

type error += Node_connection_lost

type error += Cannot_load_local_file of string

type error += Broken_locked_values_invariant

let register_error_kind category ~id ~title ~description ~pp encoding from_error
    to_error =
  Error_monad.register_error_kind
    category
    ~id:(String.concat "." ["baker"; Protocol.name; id])
    ~title
    ~description
    ~pp
    encoding
    from_error
    to_error

let () =
  register_error_kind
    `Temporary
    ~id:"Baking_scheduling.node_connection_lost"
    ~title:"Node connection lost"
    ~description:"The connection with the node was lost."
    ~pp:(fun fmt () -> Format.fprintf fmt "Lost connection with the node")
    Data_encoding.empty
    (function Node_connection_lost -> Some () | _ -> None)
    (fun () -> Node_connection_lost) ;
  register_error_kind
    `Temporary
    ~id:"Baking_scheduling.cannot_load_local_file"
    ~title:"Cannot load local file"
    ~description:"Cannot load local file."
    ~pp:(fun fmt filename ->
      Format.fprintf fmt "Cannot load the local file %s" filename)
    Data_encoding.(obj1 (req "file" string))
    (function Cannot_load_local_file s -> Some s | _ -> None)
    (fun s -> Cannot_load_local_file s) ;
  register_error_kind
    `Permanent
    ~id:"Baking_state.broken_locked_values_invariant"
    ~title:"Broken locked values invariant"
    ~description:
      "The expected consistency invariant on locked values does not hold"
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The expected consistency invariant on locked values does not hold")
    Data_encoding.unit
    (function Broken_locked_values_invariant -> Some () | _ -> None)
    (fun () -> Broken_locked_values_invariant)

type signing_request = [`Preattestation | `Attestation | `Block_header]

let signing_request_encoding : signing_request Data_encoding.t =
  let open Data_encoding in
  union
    [
      case
        ~title:"preattestation"
        (Tag 0)
        (constant "preattestation")
        (function `Preattestation -> Some () | _ -> None)
        (function () -> `Preattestation);
      case
        ~title:"attestation"
        (Tag 1)
        (constant "attestation")
        (function `Attestation -> Some () | _ -> None)
        (function () -> `Attestation);
      case
        ~title:"block_header"
        (Tag 2)
        (constant "block_header")
        (function `Block_header -> Some () | _ -> None)
        (function () -> `Block_header);
    ]

let pp_signing_request fmt = function
  | `Preattestation -> Format.fprintf fmt "a preattestation"
  | `Attestation -> Format.fprintf fmt "an attestation"
  | `Block_header -> Format.fprintf fmt "a block header"

type error += Signature_timeout of (float * signing_request)

let () =
  register_error_kind
    `Permanent
    ~id:"Signature_timeout"
    ~title:"Signature timeout"
    ~description:"Signature call reached a timeout."
    ~pp:(fun ppf (timeout, request) ->
      Format.fprintf
        ppf
        "@[A call for signing %a has reached the timeout of %f seconds.@]"
        pp_signing_request
        request
        timeout)
    Data_encoding.(
      obj2 (req "timeout" float) (req "request" signing_request_encoding))
    (function
      | Signature_timeout (timeout, request) -> Some (timeout, request)
      | _ -> None)
    (fun (timeout, request) -> Signature_timeout (timeout, request))

type error += Deterministic_nonce_timeout of float

let () =
  register_error_kind
    `Permanent
    ~id:"Deterministic_nonce_timeout"
    ~title:"Deterministic timeout"
    ~description:"Deterministic nonce call reached a timeout."
    ~pp:(fun ppf timeout ->
      Format.fprintf
        ppf
        "@[A deterministic nonce call has reached the timeout of %f seconds.@]"
        timeout)
    Data_encoding.(obj1 (req "timeout" float))
    (function Deterministic_nonce_timeout timeout -> Some timeout | _ -> None)
    (fun timeout -> Deterministic_nonce_timeout timeout)

type error += Block_vote_file_not_found of string

type error += Block_vote_file_invalid of string

type error += Block_vote_file_wrong_content of string

type error += Block_vote_file_missing_liquidity_baking_toggle_vote of string

type error += Missing_vote_on_startup

let () =
  register_error_kind
    `Permanent
    ~id:"Per_block_vote_file.block_vote_file_not_found"
    ~title:
      "The provided block vote file path does not point to an existing file."
    ~description:
      "A block vote file path was provided on the command line but the path \
       does not point to an existing file."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[The provided block vote file path \"%s\" does not point to an \
         existing file.@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function
      | Block_vote_file_not_found file_path -> Some file_path | _ -> None)
    (fun file_path -> Block_vote_file_not_found file_path) ;
  register_error_kind
    `Permanent
    ~id:"Per_block_vote_file.block_vote_file_invalid"
    ~title:
      "The provided block vote file path does not point to a valid JSON file."
    ~description:
      "A block vote file path was provided on the command line but the path \
       does not point to a valid JSON file."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[The provided block vote file path \"%s\" does not point to a valid \
         JSON file. The file exists but its content is not valid JSON.@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function Block_vote_file_invalid file_path -> Some file_path | _ -> None)
    (fun file_path -> Block_vote_file_invalid file_path) ;
  register_error_kind
    `Permanent
    ~id:"Per_block_vote_file.block_vote_file_wrong_content"
    ~title:"The content of the provided block vote file is unexpected."
    ~description:
      "The block vote file is valid JSON but its content is not the expected \
       one."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[The provided block vote file \"%s\" is a valid JSON file but its \
         content is unexpected. Expecting a JSON file containing \
         '{\"liquidity_baking_toggle_vote\": value1, \
         \"adaptive_issuance_vote\": value2}' or '{\"adaptive_issuance_vote\": \
         value1, \"liquidity_baking_toggle_vote\": value2}', where value1 is \
         one of \"on\", \"off\", or \"pass\" and value2 is one of \"on\", \
         \"off\", or \"pass\", or '{\"liquidity_baking_toggle_vote\": value}' \
         where value is one of \"on\", \"off\", or \"pass\".@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function
      | Block_vote_file_wrong_content file_path -> Some file_path | _ -> None)
    (fun file_path -> Block_vote_file_wrong_content file_path) ;
  register_error_kind
    `Permanent
    ~id:
      "Per_block_vote_file.block_vote_file_missing_liquidity_baking_toggle_vote"
    ~title:
      "In the provided block vote file, no entry for liquidity baking toggle \
       vote was found"
    ~description:
      "In the provided block vote file, no entry for liquidity baking toggle \
       vote was found."
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "@[In the provided block vote file \"%s\", the \
         \"liquidity_baking_toggle_vote\" field is missing. Expecting a JSON \
         file containing '{\"liquidity_baking_toggle_vote\": value1, \
         \"adaptive_issuance_vote\": value2}' or '{\"adaptive_issuance_vote\": \
         value1, \"liquidity_baking_toggle_vote\": value2}', where value1 is \
         one of \"on\", \"off\", or \"pass\" and value2 is one of \"on\", \
         \"off\", or \"pass\", or '{\"liquidity_baking_toggle_vote\": value}' \
         where value is one of \"on\", \"off\", or \"pass\".@]"
        file_path)
    Data_encoding.(obj1 (req "file_path" string))
    (function
      | Block_vote_file_missing_liquidity_baking_toggle_vote file_path ->
          Some file_path
      | _ -> None)
    (fun file_path ->
      Block_vote_file_missing_liquidity_baking_toggle_vote file_path) ;
  register_error_kind
    `Permanent
    ~id:"Per_block_vote_file.missing_vote_on_startup"
    ~title:"Missing vote on startup"
    ~description:
      "No CLI flag, file path, or votes file in default location provided on \
       startup"
    ~pp:(fun fmt () ->
      Format.fprintf
        fmt
        "Missing liquidity baking toggle vote, please use either the \
         --liquidity-baking-toggle-vote option, or the --votefile option or a \
         votes file in the default location: per_block_votes.json in the \
         current working directory or in the baker directory.")
    Data_encoding.empty
    (function Missing_vote_on_startup -> Some () | _ -> None)
    (fun () -> Missing_vote_on_startup)

type error += Failed_to_checkout_context

type error += Invalid_context

let () =
  register_error_kind
    `Permanent
    ~id:"Client_baking_simulator.failed_to_checkout_context"
    ~title:"Failed to checkout context"
    ~description:"The given context hash does not exist in the context."
    ~pp:(fun ppf () -> Format.fprintf ppf "Failed to checkout the context")
    Data_encoding.unit
    (function Failed_to_checkout_context -> Some () | _ -> None)
    (fun () -> Failed_to_checkout_context) ;
  register_error_kind
    `Permanent
    ~id:"Client_baking_simulator.invalid_context"
    ~title:"Invalid context"
    ~description:"Occurs when the context is inconsistent."
    ~pp:(fun ppf () -> Format.fprintf ppf "The given context is invalid.")
    Data_encoding.unit
    (function Invalid_context -> Some () | _ -> None)
    (fun () -> Invalid_context)

type error +=
  | Unexpected_empty_block_list of {
      chain : string;
      block_hash : Block_hash.t;
      length : int;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"Client_baking_blocks.unexpected_empty_block_list"
    ~title:"Unexpected empty blocklist"
    ~description:
      "The block list retrieved by Shell_services.Blocks.list is empty"
    ~pp:(fun ppf (chain, block_hash, length) ->
      Format.fprintf
        ppf
        "Unexpected empty block list retrieved from chain %s at block %a, \
         length %d"
        chain
        Block_hash.pp
        block_hash
        length)
    Data_encoding.(
      obj3
        (req "chain" string)
        (req "block_hash" Block_hash.encoding)
        (req "length" int31))
    (function
      | Unexpected_empty_block_list {chain; block_hash; length} ->
          Some (chain, block_hash, length)
      | _ -> None)
    (fun (chain, block_hash, length) ->
      Unexpected_empty_block_list {chain; block_hash; length})

(* BLS related errors *)
type error +=
  | Signature_aggregation_failure
  | Unexpected_signature_type of Signature.t
  | Unexpected_public_key_type of Signature.public_key

let () =
  register_error_kind
    `Permanent
    ~id:"baker.signature_aggregation_failure"
    ~title:"Signature aggregation failure"
    ~description:"Signature aggregation failed."
    ~pp:(fun ppf () -> Format.fprintf ppf "Signature aggregation failed.")
    Data_encoding.unit
    (function Signature_aggregation_failure -> Some () | _ -> None)
    (fun () -> Signature_aggregation_failure) ;
  register_error_kind
    `Permanent
    ~id:"Baking_actions.unexpected_signature_type"
    ~title:"Unexpected signature type"
    ~description:"Signature should be a BLS signature."
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "Signature should be a BLS signature %a."
        Signature.pp
        s)
    Data_encoding.(obj1 (req "signature" Signature.encoding))
    (function Unexpected_signature_type s -> Some s | _ -> None)
    (fun s -> Unexpected_signature_type s) ;
  register_error_kind
    `Permanent
    ~id:"Baking_actions.unexpected_public_key_type"
    ~title:"Unexpected public key type"
    ~description:"Public key should be a BLS public key."
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "Public key should be a BLS public key %a."
        Signature.Public_key.pp
        s)
    Data_encoding.(obj1 (req "pk" Signature.Public_key.encoding))
    (function Unexpected_public_key_type s -> Some s | _ -> None)
    (fun s -> Unexpected_public_key_type s)
