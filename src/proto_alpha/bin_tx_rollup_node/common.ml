(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type signer = {
  alias : string;
  pkh : Signature.public_key_hash;
  pk : Signature.public_key;
  sk : Client_keys.sk_uri;
}

let get_signer cctxt signer =
  let open Lwt_result_syntax in
  match signer with
  | None -> return None
  | Some operator -> (
      let*! pkh_err = Client_keys.Public_key_hash.of_source operator in
      match pkh_err with
      | Ok pkh ->
          let* (alias, pk, sk) = Client_keys.get_key cctxt pkh in
          return_some {alias; pkh; pk; sk}
      | Error _ -> (
          (* TODO/TORU: use proper errors *)
          let* keys = Client_keys.alias_keys cctxt operator in
          match keys with
          | None -> failwith "Unknown signer alias %s" operator
          | Some ((_, None, _) | (_, _, None)) ->
              failwith "Unknown secret key for signer %s" operator
          | Some (pkh, Some pk, Some sk) ->
              return_some {alias = operator; pkh; pk; sk}))
