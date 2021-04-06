(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type curve

type secp256r1

type _ t =
  | Secp256r1 : curve -> secp256r1 t

external curve : int -> curve = "uECC_curve_stub"

let secp256r1 =
  let c = curve 3 in
  Secp256r1 c

let to_curve : type a. a t -> curve = function
  | Secp256r1 curve -> curve

external keypair :
  Bigstring.t -> Bigstring.t -> curve -> bool = "uECC_make_key_stub" [@@noalloc]
external pk_of_sk :
  Bigstring.t -> Bigstring.t -> curve -> bool = "uECC_compute_public_key_stub" [@@noalloc]
external valid_pk :
  Bigstring.t -> curve -> bool = "uECC_valid_public_key_stub" [@@noalloc]

let pk_size = 64
let sk_size = 32
let compressed_size = pk_size / 2 + 1
let keypair pk sk =
  let pk', sk' = Bigstring.create pk_size, Bigstring.create sk_size in
  if keypair pk' sk' (to_curve secp256r1) then
    (Bigstring.blit_to_bytes pk' 0 pk 0 (Bytes.length pk);
    Bigstring.blit_to_bytes sk' 0 sk 0 (Bytes.length sk);
    true)
  else
    false

let pk_of_sk sk pk = pk_of_sk sk pk (to_curve secp256r1)
let valid_pk pk = valid_pk (Bigstring.of_bytes pk) (to_curve secp256r1)

external compress :
  Bigstring.t -> Bigstring.t -> curve -> unit = "uECC_compress_stub" [@@noalloc]
external decompress :
  Bigstring.t -> Bigstring.t -> curve -> unit = "uECC_decompress_stub" [@@noalloc]

let compress : Bytes.t -> Bytes.t -> unit = fun pk buf ->
  let out = Bigstring.create (Bytes.length buf) in
  compress (Bigstring.of_bytes pk) out (to_curve secp256r1);
  Bigstring.blit_to_bytes out 0 buf 0 (Bytes.length buf)

let decompress : Bytes.t -> Bytes.t -> unit = fun pk buf ->
  let out = Bigstring.create (Bytes.length buf) in
  decompress (Bigstring.of_bytes pk) out (to_curve secp256r1);
  Bigstring.blit_to_bytes out 0 buf 0 (Bytes.length buf)

type secret
type public

type _ key =
  | Sk : Bytes.t -> secret key
  | Pk : Bytes.t -> public key

let equal : type a. a key -> a key -> bool = fun k1 k2 ->
  match k1, k2 with
  | Sk sk, Sk sk2 -> Bytes.equal sk sk2
  | Pk pk, Pk pk2 ->
      Bytes.equal pk pk2

let compare : type a. a key -> a key -> int = fun k1 k2 ->
  match k1, k2 with
  | Sk sk, Sk sk2 -> Bytes.compare sk sk2
  | Pk pk, Pk pk2 ->
      Bytes.compare pk pk2

let neuterize : type a. a key -> public key = function
  | Pk pk -> Pk pk
  | Sk sk ->
    let pk = Bigstring.create pk_size in
    let pk_computed_ok = pk_of_sk (Bigstring.of_bytes sk) pk in
    let pk = Bigstring.to_bytes pk in
    let pk_is_valid = valid_pk pk in
    Pk pk

let pk_of_bytes : Bytes.t -> (public key) option =
  fun buf ->
    match Bytes.length buf with
    | len when len = compressed_size ->
      let pk = Bytes.create pk_size in
      decompress buf pk ;
      if valid_pk pk then Some (Pk pk)
      else None
    | len when len = pk_size + 1 ->
      let pk = Bytes.create pk_size in
      Bytes.blit buf 1 pk 0 (len - 1) ;
      if Bytes.get buf 0 = '\004' && valid_pk pk then
        Some (Pk pk)
      else None
    | len when len = pk_size ->
      if valid_pk buf then
        Some (Pk buf)
      else None
    | _ -> None

let sk_of_bytes : Bytes.t -> (secret key * public key) option =
  fun buf ->
    if Bytes.length buf <> sk_size then None
    else
      let sk = Sk (Bytes.copy buf) in
      try
        let pk = neuterize sk in
        Some (sk, pk)
      with _ -> None

let to_bytes : type a. ?compress:bool -> a key -> Bytes.t =
  fun ?compress:(comp=true) -> function
    | Sk sk -> Bytes.copy sk
    | Pk pk ->
        if comp then
          let buf = Bytes.create compressed_size in
          compress pk buf ;
          buf
        else
          let len = pk_size in
          let buf = Bytes.create (len + 1) in
          Bytes.set buf 0 '\004' ;
          Bytes.blit pk 0 buf 1 len ;
          buf

let write_key : type a. ?compress:bool -> Bytes.t -> a key -> int =
  fun ?compress:(comp=true) buf -> function
    | Sk sk ->
        let len = Bytes.length sk in
        Bytes.blit sk 0 buf 0 len ;
        len
    | Pk pk ->
        if comp then begin
          compress pk buf ;
          compressed_size
        end
        else
          let len = Bytes.length pk in
          Bytes.set buf 0 '\004' ;
          Bytes.blit pk 0 buf 1 len ;
          len + 1

let keypair () : (secret key * public key) option =
  let sk = Bytes.create sk_size in
  let pk = Bytes.create pk_size in
  match keypair pk sk with
  | true -> Some (Sk sk, Pk pk)
  | false -> None

(* external sign :
 *   Bigstring.t -> Bigstring.t -> Bigstring.t -> curve -> bool =
 *   "uECC_sign_stub" [@@noalloc] *)

external verify :
  Bigstring.t -> Bigstring.t -> Bigstring.t -> curve -> bool =
  "uECC_verify_stub" [@@noalloc]

let write_sign (Sk _sk) _buf ~msg:_ =
  failwith "Not implemented"
(* if Bigstring.length buf < pk_size c then 0
 * else
 *   match sign sk msg buf (to_curve c) with
 *   | true -> pk_size c
 *   | false -> 0 *)

let sign (Sk _sk) _msg =
  failwith "Not implemented"
(* let signature = Bigstring.create (pk_size c) in
 * match sign sk msg signature (to_curve c) with
 * | true -> Some signature
 * | false -> None *)

let verify (Pk pk) ~msg ~signature =
  if Bytes.length signature <> pk_size then false
  else verify (Bigstring.of_bytes pk) (Bigstring.of_bytes msg) (Bigstring.of_bytes signature) (to_curve secp256r1)

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
