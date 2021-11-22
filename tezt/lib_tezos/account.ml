(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

(** This type is used to construct values for secret keys.

    Note: The tests only use unencrypted keys for the moment, please
    add new constructors for other keys here, as needed. *)
type secret_key =
  | Unencrypted of string
      (** The string does NOT contain the 'unencrypted:' prefix *)

type key = {
  alias : string;
  public_key_hash : string;
  public_key : string;
  secret_key : secret_key;
}

let write_stresstest_sources_file (accounts : key list) =
  let account_to_json (account : key) =
    let (Unencrypted sk) = account.secret_key in
    `O
      [
        ("pkh", `String account.public_key_hash);
        ("pk", `String account.public_key);
        ("sk", `String sk);
      ]
  in
  let accounts_json_obj = `A (List.map account_to_json accounts) in
  let sources = Temp.file "sources.json" in
  let* () =
    Lwt_io.with_file ~mode:Lwt_io.Output sources (fun oc ->
        Lwt_io.fprintf oc "%s" @@ Ezjsonm.value_to_string accounts_json_obj)
  in
  return sources
