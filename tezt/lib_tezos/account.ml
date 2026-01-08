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

type secret_key =
  | Unencrypted of string
  | Encrypted of string
  | Remote of string

type key = {
  alias : string;
  public_key_hash : string;
  public_key : string;
  secret_key : secret_key;
}

let require_unencrypted_secret_key ~__LOC__ = function
  | Unencrypted b58_secret_key -> b58_secret_key
  | Encrypted _ | Remote _ ->
      Test.fail
        ~__LOC__
        "[require_unencrypted_secret_key] expected an unencrypted secret key"

let require_unencrypted_or_remote_secret_key ~__LOC__ = function
  | Unencrypted _ | Remote _ -> ()
  | Encrypted _ ->
      Test.fail
        ~__LOC__
        "[require_unencrypted_or_remote_secret_key] expected an unencrypted or \
         remote secret key"

let sign_bytes ?watermark ~(signer : key) (message : Bytes.t) =
  let b58_secret_key =
    require_unencrypted_secret_key ~__LOC__ signer.secret_key
  in
  let secret_key =
    Tezos_crypto.Signature.Secret_key.of_b58check_exn b58_secret_key
  in
  Tezos_crypto.Signature.sign ?watermark secret_key message

let uri_of_secret_key = function
  | Unencrypted secret_key -> "unencrypted:" ^ secret_key
  | Encrypted secret_key -> "encrypted:" ^ secret_key
  | Remote pkh -> "remote:" ^ pkh

let secret_key_equal secret_key1 secret_key2 =
  match (secret_key1, secret_key2) with
  | Unencrypted s1, Unencrypted s2
  | Encrypted s1, Encrypted s2
  | Remote s1, Remote s2 ->
      String.equal s1 s2
  | _ -> false

let secret_key_typ : secret_key Check.typ =
  Check.equalable
    (fun fmt secret_key ->
      Format.fprintf fmt "%s" (uri_of_secret_key secret_key))
    (fun secret_key1 secret_key2 -> secret_key_equal secret_key1 secret_key2)

let key_typ : key Check.typ =
  Check.equalable
    (fun fmt {alias; public_key_hash; public_key; secret_key} ->
      Format.fprintf
        fmt
        "{alias: %S; public_key_hash: %S; public_key: %s; secret_key: %S}"
        alias
        public_key_hash
        public_key
        (uri_of_secret_key secret_key))
    (fun key1 key2 ->
      String.equal key1.alias key2.alias
      && String.equal key1.public_key_hash key2.public_key_hash
      && String.equal key1.public_key key2.public_key
      && secret_key_equal key1.secret_key key2.secret_key)

(* The key is not added to a client, it must be manually added later *)
let generate_new_key ~algo ~alias =
  let open Tezos_crypto.Signature in
  let pkh, pk, sk = generate_key ~algo () in
  {
    alias;
    public_key = Public_key.to_b58check pk;
    public_key_hash = Public_key_hash.to_b58check pkh;
    secret_key = Unencrypted (Secret_key.to_b58check sk);
  }

module Wallet : sig
  val write : key list -> base_dir:string -> unit
end = struct
  let path_of_kind = function
    | `Public_key_hash -> "public_key_hashs"
    | `Public_key -> "public_keys"
    | `Secret_key -> "secret_keys"

  let json_of_secret_key secret_key = `String (uri_of_secret_key secret_key)

  let json_of_public_key public_key = `String ("unencrypted:" ^ public_key)

  let json_of_kind key =
    let mk_obj value = `O [("name", `String key.alias); ("value", value)] in
    function
    | `Public_key_hash -> mk_obj @@ `String key.public_key_hash
    | `Public_key -> mk_obj @@ json_of_public_key key.public_key
    | `Secret_key -> mk_obj @@ json_of_secret_key key.secret_key

  let write_kind wallet ~base_dir kind =
    let filename = base_dir // path_of_kind kind in
    let json = `A (List.map (fun x -> json_of_kind x kind) wallet) in
    JSON.encode_to_file_u filename json

  let write wallet ~base_dir =
    List.iter
      (fun kind -> write_kind wallet ~base_dir kind)
      [`Public_key_hash; `Public_key; `Secret_key]
end

let write = Wallet.write

module Bootstrap = struct
  let alias n = "bootstrap" ^ string_of_int n

  let keys =
    [|
      {
        alias = "bootstrap1";
        public_key_hash = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
        public_key = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav";
        secret_key =
          Unencrypted "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh";
      };
      {
        alias = "bootstrap2";
        public_key_hash = "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN";
        public_key = "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9";
        secret_key =
          Unencrypted "edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo";
      };
      {
        alias = "bootstrap3";
        public_key_hash = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU";
        public_key = "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV";
        secret_key =
          Unencrypted "edsk4ArLQgBTLWG5FJmnGnT689VKoqhXwmDPBuGx3z4cvwU9MmrPZZ";
      };
      {
        alias = "bootstrap4";
        public_key_hash = "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv";
        public_key = "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU";
        secret_key =
          Unencrypted "edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3";
      };
      {
        alias = "bootstrap5";
        public_key_hash = "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv";
        public_key = "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n";
        secret_key =
          Unencrypted "edsk4QLrcijEffxV31gGdN2HU7UpyJjA8drFoNcmnB28n89YjPNRFm";
      };
    |]
end

let parse_client_output_public_keys ~client_output =
  let public_key_hash =
    (* group of letters and digits after "Hash: "
       e.g. "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" *)
    client_output =~* rex "Hash: ?(\\w*)" |> mandatory "public key hash"
  in
  let public_key =
    (* group of letters and digits after "Public Key: "
       e.g. "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav" *)
    client_output =~* rex "Public Key: ?(\\w*)" |> mandatory "public key"
  in
  (public_key_hash, public_key)

let parse_client_output ~alias ~client_output =
  let public_key_hash, public_key =
    parse_client_output_public_keys ~client_output
  in
  let secret_key =
    (* group of letters and digits after "Secret Key: (un)encrypted:" e.g.
       "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh". *)
    match client_output =~** rex "Secret Key: ?(\\w*):?(\\w*)" with
    | Some ("unencrypted", sk) -> Unencrypted sk
    | Some ("encrypted", sk) -> Encrypted sk
    | _ -> Test.fail "Could not parse [show address] output: %s" client_output
  in
  {alias; public_key_hash; public_key; secret_key}
