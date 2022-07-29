(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Public_key_hash = struct
  include
    Blake2B.Make
      (Base58)
      (struct
        let name = "Bls12_381.Public_key_hash"

        let title = "A Bls12_381 public key hash"

        let b58check_prefix = Base58.Prefix.bls12_381_public_key_hash

        let size = Some 20
      end)

  module Logging = struct
    let tag = Tag.def ~doc:title name pp
  end
end

let () = Base58.check_encoded_prefix Public_key_hash.b58check_encoding "tz4" 36

module Public_key = struct
  open Bls12_381.Signature.MinPk

  type t = Bls12_381.Signature.MinPk.pk

  let name = "Bls12_381.Public_key"

  let title = "A Bls12_381 public key"

  let to_bytes = pk_to_bytes

  let to_string s = Bytes.to_string (to_bytes s)

  let of_bytes_opt = pk_of_bytes_opt

  let of_string_opt s = of_bytes_opt (Bytes.of_string s)

  let of_bytes_without_validation = of_bytes_opt

  let size _pk = Bls12_381.Signature.MinPk.pk_size_in_bytes

  type Base58.data += Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix:Base58.Prefix.bls12_381_public_key
      ~length:(size ())
      ~to_raw:to_string
      ~of_raw:of_string_opt
      ~wrap:(fun x -> Data x)

  let () = Base58.check_encoded_prefix b58check_encoding "BLpk" 76

  let hash v = Public_key_hash.hash_bytes [to_bytes v]

  include Compare.Make (struct
    type nonrec t = t

    let compare a b =
      Bytes.compare
        (Bls12_381.Signature.MinPk.pk_to_bytes a)
        (Bls12_381.Signature.MinPk.pk_to_bytes b)
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
  type t = Bls12_381.Signature.sk

  let name = "Bls12_381.Secret_key"

  let title = "A Bls12_381 secret key"

  include Compare.Make (struct
    type nonrec t = t

    let compare a b =
      let a = Bls12_381.Signature.sk_to_bytes a
      and b = Bls12_381.Signature.sk_to_bytes b in
      Bytes.compare a b
  end)

  let size = Bls12_381.Signature.sk_size_in_bytes

  let to_bytes = Bls12_381.Signature.sk_to_bytes

  let to_string s = Bytes.to_string (to_bytes s)

  let of_bytes_opt = Bls12_381.Signature.sk_of_bytes_opt

  let of_string_opt s = of_bytes_opt (Bytes.of_string s)

  let to_public_key = Bls12_381.Signature.MinPk.derive_pk

  type Base58.data += Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix:Base58.Prefix.bls12_381_secret_key
      ~length:size
      ~to_raw:to_string
      ~of_raw:of_string_opt
      ~wrap:(fun sk -> Data sk)

  let of_b58check_opt s =
    match Base58.simple_decode b58check_encoding s with
    | Some x -> Some x
    | None -> Format.kasprintf Stdlib.failwith "Unexpected data (%s)" name

  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x -> x
    | None -> Format.kasprintf Stdlib.failwith "Unexpected data (%s)" name

  let of_b58check s =
    match of_b58check_opt s with
    | Some x -> Ok x
    | None ->
        Error_monad.error_with
          "Failed to read a b58check_encoding data (%s): %S"
          name
          s

  let to_b58check s = Base58.simple_encode b58check_encoding s

  let to_short_b58check s =
    String.sub
      (to_b58check s)
      0
      (10 + String.length (Base58.prefix b58check_encoding))

  let () = Base58.check_encoded_prefix b58check_encoding "BLsk" 54

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

type t = Bls12_381.Signature.MinPk.signature

let name = "Bls12_381.Signature"

let title = "A Bls12_381 signature"

let size = Bls12_381.Signature.MinPk.signature_size_in_bytes

let to_bytes = Bls12_381.Signature.MinPk.signature_to_bytes

let of_bytes_opt s =
  if Bytes.length s = size then
    Bls12_381.Signature.MinPk.signature_of_bytes_opt s
  else None

let to_string s = Bytes.to_string (to_bytes s)

let of_string_opt s = of_bytes_opt (Bytes.of_string s)

type Base58.data += Data of t

let b58check_encoding =
  Base58.register_encoding
    ~prefix:Base58.Prefix.bls12_381_signature
    ~length:size
    ~to_raw:to_string
    ~of_raw:of_string_opt
    ~wrap:(fun x -> Data x)

let () = Base58.check_encoded_prefix b58check_encoding "BLsig" 142

include Helpers.MakeB58 (struct
  type nonrec t = t

  let name = name

  let b58check_encoding = b58check_encoding
end)

include Helpers.MakeRaw (struct
  type nonrec t = t

  let name = name

  let of_bytes_opt = of_bytes_opt

  let of_string_opt = of_string_opt

  let to_string = to_string
end)

include Compare.Make (struct
  type nonrec t = t

  let compare a b =
    let a = to_bytes a and b = to_bytes b in
    Bytes.compare a b
end)

include Helpers.MakeEncoder (struct
  type nonrec t = t

  let name = name

  let title = title

  let raw_encoding =
    Data_encoding.conv to_bytes of_bytes_exn (Data_encoding.Fixed.bytes size)

  let of_b58check = of_b58check

  let of_b58check_opt = of_b58check_opt

  let of_b58check_exn = of_b58check_exn

  let to_b58check = to_b58check

  let to_short_b58check = to_short_b58check
end)

let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

let sign = Bls12_381.Signature.MinPk.Aug.sign

let check pk signature msg =
  Bls12_381.Signature.MinPk.Aug.verify pk msg signature

(* [seed] must be at least of 32 bytes or [Bls12_381.Signature.generate_sk] will
   throw an error. *)
let generate_key ?seed () =
  let seed =
    match seed with
    | Some seed -> seed
    | None ->
        (* same source of random as other signature, should be safe. Bls needs
           bytes of 32 *)
        Hacl.Rand.gen 32
  in
  let sk = Bls12_381.Signature.generate_sk seed in
  let pk = Bls12_381.Signature.MinPk.derive_pk sk in
  let pkh = Public_key.hash pk in
  (pkh, pk, sk)

let aggregate_check pk_msg_list signature =
  Bls12_381.Signature.MinPk.Aug.aggregate_verify pk_msg_list signature

let aggregate_signature_opt = Bls12_381.Signature.MinPk.aggregate_signature_opt
