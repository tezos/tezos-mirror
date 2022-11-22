(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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
open Environment
open Error_monad

type error +=
  | Cannot_convert_root_page_hash_to_bytes of string
  | Cannot_compute_aggregate_signature of string
  | Public_key_for_witness_not_available of int * string

let () =
  register_error_kind
    `Permanent
    ~id:"cannot_extract_root_page_hash_to_sign"
    ~title:"Cannot convert root hash page to byte sequence"
    ~description:"Cannot convert root hash page to byte sequence"
    ~pp:(fun ppf b58_hash ->
      Format.fprintf
        ppf
        "Cannot convert root hash page to byte sequence: %s"
        b58_hash)
    Data_encoding.(obj1 (req "hash" string))
    (function
      | Cannot_convert_root_page_hash_to_bytes b58_hash -> Some b58_hash
      | _ -> None)
    (fun b58_hash -> Cannot_convert_root_page_hash_to_bytes b58_hash) ;
  register_error_kind
    `Permanent
    ~id:"cannot_compute_root_hash_aggregate_signature"
    ~title:"Cannot compute aggregate signature of root page hash"
    ~description:"Cannot compute aggregate signature of root page hash"
    ~pp:(fun ppf b58_hash ->
      Format.fprintf
        ppf
        "Cannot compute aggregate signature of root page hash: %s"
        b58_hash)
    Data_encoding.(obj1 (req "hash" string))
    (function
      | Cannot_compute_aggregate_signature b58_hash -> Some b58_hash | _ -> None)
    (fun b58_hash -> Cannot_compute_aggregate_signature b58_hash) ;
  register_error_kind
    `Permanent
    ~id:"public_key_of_witness_not_available"
    ~title:
      "Public key of witness dac member not available for verifying signature"
    ~description:
      "Public key of witness dac member not available for verifying signature"
    ~pp:(fun ppf (witness_index, b58_hash) ->
      Format.fprintf
        ppf
        "Public key of dac member %d not available for verifying signature of \
         root page hash %s"
        witness_index
        b58_hash)
    Data_encoding.(obj2 (req "witness_index" int31) (req "hash" string))
    (function
      | Public_key_for_witness_not_available (index, hash) -> Some (index, hash)
      | _ -> None)
    (fun (index, hash) -> Public_key_for_witness_not_available (index, hash))

module type REVEAL_HASH = module type of Sc_rollup_reveal_hash

module Make (Hashing_scheme : REVEAL_HASH) = struct
  let bind_es (f : 'a -> 'b option tzresult Lwt.t) v_opt =
    let open Lwt_result_syntax in
    match v_opt with None -> return None | Some v -> f v

  let rev_collect_indexed_signatures cctxt dac_sk_uris bytes_to_sign =
    let open Lwt_result_syntax in
    List.rev_mapi_es
      (fun index sk_uri_opt ->
        (* TODO: <insert issue here>
           Implement Option.bind_es and revisit this*)
        bind_es
          (fun sk_uri ->
            let*! signature_res =
              Tezos_client_base.Client_keys.aggregate_sign
                cctxt
                sk_uri
                bytes_to_sign
            in
            let signature_opt = Result.to_option signature_res in
            return
            @@ Option.map (fun signature -> (index, signature)) signature_opt)
          sk_uri_opt)
      dac_sk_uris

  let compute_signatures_with_witnesses rev_indexed_signatures =
    let open Lwt_result_syntax in
    List.fold_left_es
      (fun (signatures, witnesses) signature_opt ->
        match signature_opt with
        | None -> return (signatures, witnesses)
        | Some (index, signature) ->
            let*? bitmap = Bitset.add witnesses index in
            return (signature :: signatures, bitmap))
      ([], Bitset.empty)
      rev_indexed_signatures

  let sign_root_hash cctxt dac_sk_uris root_hash =
    let open Lwt_result_syntax in
    let bytes_to_sign =
      Data_encoding.Binary.to_bytes_opt Hashing_scheme.encoding root_hash
    in
    let b58_root_hash = Hashing_scheme.to_b58check root_hash in
    match bytes_to_sign with
    | None -> tzfail @@ Cannot_convert_root_page_hash_to_bytes b58_root_hash
    | Some bytes_to_sign -> (
        let* rev_indexed_signatures =
          rev_collect_indexed_signatures cctxt dac_sk_uris bytes_to_sign
        in
        let* signatures, witnesses =
          compute_signatures_with_witnesses rev_indexed_signatures
        in
        let final_signature =
          Tezos_crypto.Aggregate_signature.aggregate_signature_opt signatures
        in
        match final_signature with
        | None -> tzfail @@ Cannot_compute_aggregate_signature b58_root_hash
        | Some signature -> return @@ (signature, witnesses))
end

module Reveal_hash = Make (Sc_rollup_reveal_hash)
