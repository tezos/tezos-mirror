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

open Error_monad

module Public_key_hash = struct
  include
    Blake2B.Make
      (Base58)
      (struct
        let name = "Ed25519.Public_key_hash"

        let title = "An Ed25519 public key hash"

        let b58check_prefix = Base58.Prefix.ed25519_public_key_hash

        let size = Some 20
      end)

  module Logging = struct
    let tag = Tag.def ~doc:title name pp
  end
end

let () = Base58.check_encoded_prefix Public_key_hash.b58check_encoding "tz1" 36

open Hacl.Ed25519

module Public_key = struct
  type t = Hacl.public Hacl.Ed25519.key

  let name = "Ed25519.Public_key"

  let title = "Ed25519 public key"

  let to_bytes = to_bytes

  let to_string s = Bytes.to_string (to_bytes s)

  let of_bytes_opt = pk_of_bytes

  let of_string_opt s = of_bytes_opt (Bytes.of_string s)

  let of_bytes_without_validation = of_bytes_opt

  let size _ = pk_size

  type Base58.data += Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix:Base58.Prefix.ed25519_public_key
      ~length:(size ())
      ~to_raw:to_string
      ~of_raw:of_string_opt
      ~wrap:(fun x -> Data x)

  let () = Base58.check_encoded_prefix b58check_encoding "edpk" 54

  let hash v = Public_key_hash.hash_bytes [to_bytes v]

  include Compare.Make (struct
    type nonrec t = t

    let compare = Hacl.Ed25519.compare
  end)

  include Helpers.MakeRaw (struct
    type nonrec t = t

    let name = name

    let of_bytes_opt = of_bytes_opt

    let of_string_opt = of_string_opt

    let to_string = to_string
  end)

  include Helpers.MakeB58 (struct
    type nonrec t = t

    let name = name

    let b58check_encoding = b58check_encoding
  end)

  include Helpers.MakeEncoder (struct
    type nonrec t = t

    let name = name

    let title = title

    let raw_encoding =
      let open Data_encoding in
      conv to_bytes of_bytes_exn (Fixed.bytes (size ()))

    let of_b58check = of_b58check

    let of_b58check_opt = of_b58check_opt

    let of_b58check_exn = of_b58check_exn

    let to_b58check = to_b58check

    let to_short_b58check = to_short_b58check
  end)

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)
end

module Secret_key = struct
  type t = Hacl.secret key

  let name = "Ed25519.Secret_key"

  let title = "An Ed25519 secret key"

  let size = sk_size

  let to_bytes = to_bytes

  let to_string s = Bytes.to_string (to_bytes s)

  let of_bytes_opt = sk_of_bytes

  let of_string_opt s = of_bytes_opt (Bytes.of_string s)

  let to_public_key = neuterize

  type Base58.data += Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix:Base58.Prefix.ed25519_seed
      ~length:size
      ~to_raw:to_string
      ~of_raw:of_string_opt
      ~wrap:(fun sk -> Data sk)

  (* Legacy NaCl secret key encoding. Used to store both sk and pk. *)
  let secret_key_encoding =
    Base58.register_encoding
      ~prefix:Base58.Prefix.ed25519_secret_key
      ~length:(sk_size + pk_size)
      ~to_raw:(fun sk ->
        let pk = neuterize sk in
        let buf = Bytes.create (sk_size + pk_size) in
        blit_to_bytes sk buf ;
        blit_to_bytes pk ~pos:sk_size buf ;
        Bytes.unsafe_to_string buf)
      ~of_raw:(fun buf ->
        let sk = Bytes.create sk_size in
        Bytes.blit_string buf 0 sk 0 sk_size ;
        sk_of_bytes sk)
      ~wrap:(fun x -> Data x)

  let of_b58check_opt s =
    match Base58.simple_decode b58check_encoding s with
    | Some x -> Some x
    | None -> Base58.simple_decode secret_key_encoding s

  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x -> x
    | None -> Format.kasprintf Stdlib.failwith "Unexpected data (%s)" name

  let of_b58check s =
    match of_b58check_opt s with
    | Some x -> Ok x
    | None ->
        error_with "Failed to read a b58check_encoding data (%s): %S" name s

  let to_b58check s = Base58.simple_encode b58check_encoding s

  let to_short_b58check s =
    String.sub
      (to_b58check s)
      0
      (10 + String.length (Base58.prefix b58check_encoding))

  let () =
    Base58.check_encoded_prefix b58check_encoding "edsk" 54 ;
    Base58.check_encoded_prefix secret_key_encoding "edsk" 98

  include Compare.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  include Helpers.MakeRaw (struct
    type nonrec t = t

    let name = name

    let of_bytes_opt = of_bytes_opt

    let of_string_opt = of_string_opt

    let to_string = to_string
  end)

  include Helpers.MakeEncoder (struct
    type nonrec t = t

    let name = name

    let title = title

    let raw_encoding =
      let open Data_encoding in
      conv to_bytes of_bytes_exn (Fixed.bytes size)

    let of_b58check = of_b58check

    let of_b58check_opt = of_b58check_opt

    let of_b58check_exn = of_b58check_exn

    let to_b58check = to_b58check

    let to_short_b58check = to_short_b58check
  end)

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)
end

type t = Bytes.t

type watermark = Bytes.t

let name = "Ed25519"

let title = "An Ed25519 signature"

let size = size

let to_bytes s = Bytes.copy s

let to_string s = Bytes.to_string (to_bytes s)

let of_bytes_opt s = if Bytes.length s = size then Some s else None

let of_string_opt s = of_bytes_opt (Bytes.of_string s)

type Base58.data += Data of t

let b58check_encoding =
  Base58.register_encoding
    ~prefix:Base58.Prefix.ed25519_signature
    ~length:size
    ~to_raw:to_string
    ~of_raw:of_string_opt
    ~wrap:(fun x -> Data x)

let () = Base58.check_encoded_prefix b58check_encoding "edsig" 99

include Helpers.MakeRaw (struct
  type nonrec t = t

  let name = name

  let of_bytes_opt = of_bytes_opt

  let of_string_opt = of_string_opt

  let to_string = to_string
end)

include Helpers.MakeB58 (struct
  type nonrec t = t

  let name = name

  let b58check_encoding = b58check_encoding
end)

include Helpers.MakeEncoder (struct
  type nonrec t = t

  let name = name

  let title = title

  let raw_encoding =
    let open Data_encoding in
    conv to_bytes of_bytes_exn (Fixed.bytes size)

  let of_b58check = of_b58check

  let of_b58check_opt = of_b58check_opt

  let of_b58check_exn = of_b58check_exn

  let to_b58check = to_b58check

  let to_short_b58check = to_short_b58check
end)

let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

let zero = Bytes.make size '\000'

let sign ?watermark sk msg =
  let msg =
    Blake2B.to_bytes @@ Blake2B.hash_bytes
    @@ match watermark with None -> [msg] | Some prefix -> [prefix; msg]
  in
  sign ~sk ~msg

let check ?watermark pk signature msg =
  let msg =
    Blake2B.to_bytes @@ Blake2B.hash_bytes
    @@ match watermark with None -> [msg] | Some prefix -> [prefix; msg]
  in
  verify ~pk ~msg ~signature

let generate_key ?seed () =
  match seed with
  | None ->
      let pk, sk = keypair () in
      (Public_key.hash pk, pk, sk)
  | Some seed -> (
      let seedlen = Bytes.length seed in
      if seedlen < Secret_key.size then
        invalid_arg
          (Printf.sprintf
             "Ed25519.generate_key: seed must be at least %d bytes long (got \
              %d)"
             Secret_key.size
             seedlen)
      else
        match sk_of_bytes (Bytes.sub seed 0 Secret_key.size) with
        | None -> invalid_arg "Ed25519.generate_key: invalid seed"
        | Some sk ->
            let pk = neuterize sk in
            (Public_key.hash pk, pk, sk))

let deterministic_nonce sk msg =
  let key = Secret_key.to_bytes sk in
  Hacl.Hash.SHA256.HMAC.digest ~key ~msg

let deterministic_nonce_hash sk msg =
  Blake2B.to_bytes (Blake2B.hash_bytes [deterministic_nonce sk msg])

include (Compare.Bytes : Compare.S with type t := t)
