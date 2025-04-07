(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(*                                                                           *)
(*****************************************************************************)

(* This file is sourced from the efunc library, licensed under the MIT License:
   https://gitlab.com/functori/dev/efunc. *)

(** This library relies on the [Random] module, and users should make sure to
    properly initialize its state in a way which fits their use case (e.g.,
    with [Random.self_init ()]. *)

open Types
open Libsecp256k1.External

let keccak s = Digestif.KECCAK_256.(to_raw_string @@ digest_string s)

let context =
  let c = ref None in
  fun () ->
    (match !c with
    | None -> c := Some (Libsecp256k1.External.Context.create ())
    | _ -> ()) ;
    Option.get !c

let rand n = String.init n (fun _ -> Char.chr @@ Random.int 256)

let to_pk sk = Key.neuterize_exn (context ()) sk

let to_address pk =
  let pkh =
    keccak @@ Bigstring.to_string
    @@ Bigstring.sub (Key.to_bytes ~compress:false (context ()) pk) 1 64
  in
  a (Hex.show @@ Hex.of_string @@ String.sub pkh 12 20)

(** [generate ~seed ()] can be used to generate a new EOA. [seed] is expected
    to be at least {!Libsecp256k1.External.Key.secret_bytes}-byte long (32). *)
let generate ~seed () =
  let sk = Key.read_sk_exn (context ()) (Bigstring.of_string seed) in
  let pk = to_pk sk in
  let address = to_address pk in
  (sk, pk, address)

let rec remove_leading_zeroes s offset len =
  if offset = len then Bigstring.empty
  else if Bigstring.get s offset = '\000' then
    remove_leading_zeroes s (offset + 1) len
  else if offset = 0 then s
  else Bigstring.sub s offset (len - offset)

let of_secp256k1 si =
  let rs = Sign.to_bytes (context ()) si in
  let r =
    b @@ Hex.show @@ Hex.of_bigstring
    @@ remove_leading_zeroes (Bigstring.sub rs 0 32) 0 32
  in
  let s =
    b @@ Hex.show @@ Hex.of_bigstring
    @@ remove_leading_zeroes (Bigstring.sub rs 32 32) 0 32
  in
  let v = int_of_char (Bigstring.get rs 64) in
  {v; r; s}

let to_secp256k1 {v; r; s} =
  let r = Hex.to_bigstring (`Hex (r :> string)) in
  let s = Hex.to_bigstring (`Hex (s :> string)) in
  let v = Bigstring.make 1 @@ char_of_int v in
  let b = Bigstring.concat "" [r; s; v] in
  Sign.read_recoverable_exn (context ()) b

let sign sk msg =
  let hash = keccak msg in
  let si =
    Sign.sign_recoverable_exn (context ()) ~sk (Bigstring.of_string hash)
  in
  of_secp256k1 si

let recover signature msg =
  Sign.recover_exn (context ()) ~signature:(to_secp256k1 signature) msg
