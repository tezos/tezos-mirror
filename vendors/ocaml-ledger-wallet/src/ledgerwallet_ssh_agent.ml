(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Rresult
open Ledgerwallet

type ins =
  | Get_public_key
  | Sign_ssh_blob
  | Sign_generic_hash
  | Sign_direct_hash
  | Get_ecdh_secret

let int_of_ins = function
  | Get_public_key -> 0x02
  | Sign_ssh_blob -> 0x04
  | Sign_generic_hash -> 0x06
  | Sign_direct_hash -> 0x08
  | Get_ecdh_secret -> 0x0A

let wrap_ins cmd =
  Apdu.create_cmd ~cmd ~cla_of_cmd:(fun _ -> 0x80) ~ins_of_cmd:int_of_ins

type curve =
  | Prime256v1
  | Curve25519

let int_of_curve = function
  | Prime256v1 -> 0x01
  | Curve25519 -> 0x02

let get_public_key ?pp ?buf ~curve ~path h =
  let nb_derivations = List.length path in
  if nb_derivations > 10 then invalid_arg "get_public_key: max 10 derivations" ;
  let lc = 1 + 4 * nb_derivations in
  let p2 = int_of_curve curve in
  let data_init = Cstruct.create lc in
  Cstruct.set_uint8 data_init 0 nb_derivations ;
  let data = Cstruct.shift data_init 1 in
  let _data = ListLabels.fold_left path ~init:data ~f:begin fun cs i ->
      Cstruct.BE.set_uint32 cs 0 i ;
      Cstruct.shift cs 4
  end in
  Transport.apdu ?pp ?buf h
    Apdu.(create ~lc ~p2 ~data:data_init (wrap_ins Get_public_key)) >>|
    fun addr ->
    let keylen = Cstruct.get_uint8 addr 0 in
    Cstruct.sub addr 1 keylen

(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff

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
