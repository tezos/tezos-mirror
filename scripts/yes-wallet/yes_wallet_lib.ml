(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(*
   dune exec scripts/yes-wallet/yes-wallet.exe

   Given a list of aliases and public key hashes:
   - encodes each public key as a fake secret key that can be used
     with the yes-node.patch
   - creates a 'yes-wallet' directory to be passed to tezos-client -d option
 *)

let pkh_json (alias, pkh, _pk) =
  Ezjsonm.(dict [("name", string alias); ("value", string pkh)])

let pk_json (alias, _pkh, pk) =
  Ezjsonm.(
    dict
      [ ("name", string alias);
        ( "value",
          dict [("locator", string @@ "unencrypted:" ^ pk); ("key", string pk)]
        ) ])

(* P-256 pk : 33+1 bytes
   ed25519 pk sk : 32+1 bytes
 *)

let sk_of_pk (pk_s : string) : string =
  let open Tezos_crypto.Signature in
  let pk = Public_key.of_b58check_exn pk_s in
  let pk_b = Data_encoding.Binary.to_bytes_exn Public_key.encoding pk in
  let sk_b = Bytes.sub pk_b 0 33 in
  let sk = Data_encoding.Binary.of_bytes_exn Secret_key.encoding sk_b in
  let sk_s = Secret_key.to_b58check sk in
  sk_s

let sk_json (alias, _pkh, pk) =
  Ezjsonm.(
    dict
      [ ("name", string alias);
        ("value", string @@ "unencrypted:" ^ sk_of_pk pk) ])

let map_bind_to_json f list = Ezjsonm.list f list

let pkh_list_json list = map_bind_to_json pkh_json list

let pk_list_json list = map_bind_to_json pk_json list

let sk_list_json list = map_bind_to_json sk_json list

let json_to_file json file =
  let chan = open_out file in
  Ezjsonm.to_channel ~minify:false chan json ;
  close_out chan

let alias_pkh_pk_list =
  [ ( "foundation1",
      "tz3RDC3Jdn4j15J7bBHZd29EUee9gVB1CxD9",
      "p2pk67wVncLFS1DQDm2gVR45sYCzQSXTtqn3bviNYXVCq6WRoqtxHXL" );
    ( "foundation2",
      "tz3bvNMQ95vfAYtG8193ymshqjSvmxiCUuR5",
      "p2pk66n1NmhPDEkcf9sXEKe9kBoTwBoTYxke1hx16aTRVq8MoXuwNqo" );
    ( "foundation3",
      "tz3RB4aoyjov4KEVRbuhvQ1CKJgBJMWhaeB8",
      "p2pk67NECc8vGK4eLbXGEgBZGhk53x1pCMbgnFEgLxZEMGDtzVcFQok" );
    ( "foundation4",
      "tz3bTdwZinP8U1JmSweNzVKhmwafqWmFWRfk",
      "p2pk6796esaR3dNr8jUx8S7xxZdRvpYSrhHMg6NagjwMRJHsERMiUKM" );
    ( "foundation5",
      "tz3NExpXn9aPNZPorRE4SdjJ2RGrfbJgMAaV",
      "p2pk66iTZwLmRPshQgUr2HE3RUzSFwAN5MNaBQ5rfduT1dGKXd25pNN" );
    ( "foundation6",
      "tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r",
      "p2pk65ffAqpYT6Et73DXdNqudthwmSNzNyzL3Wdn2EYuiiMwoPu6vFJ" );
    ( "foundation7",
      "tz3WMqdzXqRWXwyvj5Hp2H7QEepaUuS7vd9K",
      "p2pk67Cwb5Ke6oSmqeUbJxURXMe3coVnH9tqPiB2xD84CYhHbBKs4oM" );
    ( "foundation8",
      "tz3VEZ4k6a4Wx42iyev6i2aVAptTRLEAivNN",
      "p2pk67uapBxwkM1JNasGJ6J3rozzYELgrtcqxKZwZLjvsr4XcAr4FqC" ) ]
