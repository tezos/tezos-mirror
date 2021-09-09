(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Rresult
open Ledgerwallet

type ins =
  | Get_version
  | Get_public_key
  | Sign_hash
  | Sign_txn

let int_of_ins = function
  | Get_version -> 0x01
  | Get_public_key -> 0x02
  | Sign_hash -> 0x04
  | Sign_txn -> 0x08

let wrap_ins cmd =
  Apdu.create_cmd ~cmd ~cla_of_cmd:(fun _ -> 0xe0) ~ins_of_cmd:int_of_ins

let get_version ?pp ?buf h =
  let msg = "Zil.get_version" in
  let apdu = Apdu.create (wrap_ins Get_version) in
  Transport.apdu ~msg ?pp ?buf h apdu >>| fun ver ->
  Cstruct.(get_uint8 ver 0, get_uint8 ver 1, get_uint8 ver 2)

let get_pk ?(display_addr=false) ?pp ?buf h i =
  let msg = "Zil.get_pk" in
  let data = Cstruct.create 4 in
  Cstruct.LE.set_uint32 data 0 i ;
  let p2 = if display_addr then 1 else 0 in
  let apdu = Apdu.create ~p2 ~data (wrap_ins Get_public_key) in
  Transport.apdu ~msg ?pp ?buf h apdu >>| fun buf ->
  let pk = Cstruct.sub buf 0 33 in
  let buf = Cstruct.shift buf 33 in
  pk, Bech32.Segwit.(decode_exn (module Zil) (Cstruct.to_string buf))

let sign_hash ?pp ?buf h i hash =
  let msg = "Zil.sign_hash" in
  let data = Cstruct.create 36 in
  Cstruct.LE.set_uint32 data 0 i ;
  Cstruct.blit hash 0 data 4 32 ;
  let apdu = Apdu.create ~data (wrap_ins Sign_hash) in
  Transport.apdu ~msg ?pp ?buf h apdu

let sign_txn ?pp ?buf h i txn =
  let msg = "Zil.sign_txn" in
  let txnlen = Cstruct.len txn in
  let data = Cstruct.create (12 + txnlen) in
  Cstruct.LE.set_uint32 data 0 i ;
  Cstruct.LE.set_uint32 data 4 0l ;
  Cstruct.LE.set_uint32 data 8 (Int32.of_int txnlen) ;
  Cstruct.blit txn 0 data 12 txnlen ;
  let apdu = Apdu.create ~data (wrap_ins Sign_txn) in
  Transport.apdu ~msg ?pp ?buf h apdu

(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
