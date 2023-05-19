(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Trili Tech, <contact@trili.tech>                       *)
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

module Aggregate_signature = Tezos_crypto.Aggregate_signature

module Coordinator = struct
  type t = {
    public_key_hash : Aggregate_signature.public_key_hash;
    public_key : Aggregate_signature.public_key;
  }

  let of_committee_member_public_key public_key =
    let open Lwt_result_syntax in
    let public_key_hash = Aggregate_signature.Public_key.hash public_key in
    return {public_key_hash; public_key}
end

module Committee_member = struct
  type error +=
    | Committee_member_cannot_sign of Aggregate_signature.public_key_hash

  let () =
    register_error_kind
      `Permanent
      ~id:"committee_member_cannot_sign"
      ~title:"Committee member cannot sign messages"
      ~description:
        "Committee member cannot sign messages because the signing key is not \
         available"
      ~pp:(fun ppf pkh ->
        Format.fprintf
          ppf
          "Cannot convert root hash page to byte sequence: %a"
          Aggregate_signature.Public_key_hash.pp
          pkh)
      Data_encoding.(
        obj1
          (req "public_key_hash" Aggregate_signature.Public_key_hash.encoding))
      (function Committee_member_cannot_sign pkh -> Some pkh | _ -> None)
      (fun pkh -> Committee_member_cannot_sign pkh)

  type t = {
    public_key_hash : Aggregate_signature.public_key_hash;
    secret_key_uri : Client_keys.aggregate_sk_uri;
  }

  let of_committee_member_address pkh cctxt =
    let open Lwt_result_syntax in
    let* public_key_hash, _, secret_key_uri_opt =
      Wallet_cctxt_helpers.get_keys cctxt pkh
    in
    match secret_key_uri_opt with
    | None -> tzfail @@ Committee_member_cannot_sign pkh
    | Some secret_key_uri -> return {public_key_hash; secret_key_uri}
end

module Legacy = struct
  type t = {
    public_key_hash : Aggregate_signature.public_key_hash;
    public_key_opt : Aggregate_signature.public_key option;
    secret_key_uri_opt : Client_keys.aggregate_sk_uri option;
  }

  let of_committee_member_address pkh cctxt =
    let open Lwt_result_syntax in
    let+ public_key_hash, public_key_opt, secret_key_uri_opt =
      Wallet_cctxt_helpers.get_keys cctxt pkh
    in
    {public_key_hash; public_key_opt; secret_key_uri_opt}
end
