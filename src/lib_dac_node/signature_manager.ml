(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 Trili Tech  <contact@trili.tech>                  *)
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
    Data_encoding.(obj1 (req "hash" (string' Plain)))
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
    Data_encoding.(obj1 (req "hash" (string' Plain)))
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
    Data_encoding.(
      obj2 (req "witness_index" int31) (req "hash" (string' Plain)))
    (function
      | Public_key_for_witness_not_available (index, hash) -> Some (index, hash)
      | _ -> None)
    (fun (index, hash) -> Public_key_for_witness_not_available (index, hash))

let bind_es (f : 'a -> 'b option tzresult Lwt.t) v_opt =
  let open Lwt_result_syntax in
  match v_opt with None -> return None | Some v -> f v

let rev_collect_indexed_signatures cctxt dac_sk_uris bytes_to_sign =
  let open Lwt_result_syntax in
  List.rev_mapi_es
    (fun index sk_uri_opt ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/4306
         Implement Option.bind_es and revisit this. *)
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
          let bit = Z.shift_left Z.one index in
          let witnesses = Z.logor witnesses bit in
          return (signature :: signatures, witnesses))
    ([], Z.zero)
    rev_indexed_signatures

let sign_root_hash ((module P) : Dac_plugin.t) cctxt dac_sk_uris root_hash =
  let open Lwt_result_syntax in
  let bytes_to_sign = Data_encoding.Binary.to_bytes_opt P.encoding root_hash in
  let root_hash = P.to_hex root_hash in
  match bytes_to_sign with
  | None -> tzfail @@ Cannot_convert_root_page_hash_to_bytes root_hash
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
      | None -> tzfail @@ Cannot_compute_aggregate_signature root_hash
      | Some signature -> return @@ (signature, witnesses))

let verify ((module P) : Dac_plugin.t) ~public_keys_opt root_page_hash signature
    witnesses =
  let open Lwt_result_syntax in
  let hash_as_bytes =
    Data_encoding.Binary.to_bytes_opt P.encoding root_page_hash
  in
  match hash_as_bytes with
  | None ->
      tzfail @@ Cannot_convert_root_page_hash_to_bytes (P.to_hex root_page_hash)
  | Some bytes ->
      let* pk_msg_list =
        public_keys_opt
        |> List.mapi (fun i public_key_opt -> (i, public_key_opt))
        |> List.filter_map_es (fun (i, public_key_opt) ->
               let is_witness = Z.testbit witnesses i in
               match public_key_opt with
               | None ->
                   if is_witness then
                     tzfail
                     @@ Public_key_for_witness_not_available
                          (i, P.to_hex root_page_hash)
                   else return None
               | Some public_key ->
                   if is_witness then return @@ Some (public_key, None, bytes)
                   else return None)
      in
      return
      @@ Tezos_crypto.Aggregate_signature.aggregate_check pk_msg_list signature
