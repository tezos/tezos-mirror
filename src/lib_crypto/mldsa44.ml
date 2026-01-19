(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad

module Public_key_hash = struct
  include
    Blake2B.Make
      (Base58)
      (struct
        let name = "ML_DSA_44.Public_key_hash"

        let title = "An ML-DSA-44 public key hash"

        let b58check_prefix = Base58.Prefix.mldsa44_public_key_hash

        let size = Some 20
      end)

  module Logging = struct
    let tag = Tag.def ~doc:title name pp
  end
end

let () = Base58.check_encoded_prefix Public_key_hash.b58check_encoding "tz5" 36

open Octez_ml_dsa

module Public_key = struct
  type t = Ml_dsa_44.verification_key

  let name = "ML_DSA_44.Public_key"

  let title = "ML-DSA-44 public key"

  let to_bytes = Ml_dsa_44.verification_key_to_bytes

  let to_string s = Bytes.to_string (to_bytes s)

  let of_bytes_opt bytes = Ml_dsa_44.verification_key_from_bytes_opt bytes

  let of_string_opt s = of_bytes_opt (Bytes.of_string s)

  let of_bytes_without_validation = of_bytes_opt

  let size _ = Ml_dsa_44.verification_key_size

  type Base58.data += Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix:Base58.Prefix.mldsa44_public_key
      ~length:(size ())
      ~to_raw:to_string
      ~of_raw:of_string_opt
      ~wrap:(fun x -> Data x)

  let () = Base58.check_encoded_prefix b58check_encoding "mdpk" 1802

  let hash v = Public_key_hash.hash_bytes [to_bytes v]

  include Compare.Make (struct
    type nonrec t = t

    let compare a b =
      let a = to_bytes a and b = to_bytes b in
      Bytes.compare a b
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
  type t =
    (* Secret key includes both a signing key and a verification key,
       as the standard does not define the derivation function for the
       verification key from the signing key. *)
    Ml_dsa_44.signing_key * Ml_dsa_44.verification_key

  let name = "ML_DSA_44.Secret_key"

  let title = "An ML-DSA-44 secret key"

  let signing_key_size = Ml_dsa_44.signing_key_size

  let verification_key_size = Ml_dsa_44.verification_key_size

  let size = signing_key_size + verification_key_size

  let to_bytes (signing_key, verification_key) =
    let signing_key_bytes = Ml_dsa_44.signing_key_to_bytes signing_key in
    let verification_key_bytes =
      Ml_dsa_44.verification_key_to_bytes verification_key
    in
    Bytes.cat signing_key_bytes verification_key_bytes

  let to_string s = Bytes.to_string (to_bytes s)

  let of_bytes_opt bytes =
    let open Option_syntax in
    let* signing_key =
      Ml_dsa_44.signing_key_from_bytes_opt (Bytes.sub bytes 0 signing_key_size)
    in
    let* verification_key =
      Ml_dsa_44.verification_key_from_bytes_opt
        (Bytes.sub bytes signing_key_size verification_key_size)
    in
    Some (signing_key, verification_key)

  let of_string_opt s = of_bytes_opt (Bytes.of_string s)

  let to_public_key (_signing_key, verification_key) = verification_key

  type Base58.data += Data of t

  let b58check_encoding =
    Base58.register_encoding
      ~prefix:Base58.Prefix.mldsa44_secret_key
      ~length:size
      ~to_raw:to_string
      ~of_raw:of_string_opt
      ~wrap:(fun sk -> Data sk)

  let of_b58check_opt s = Base58.simple_decode b58check_encoding s

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

  let () = Base58.check_encoded_prefix b58check_encoding "mdsk" 5298

  include Compare.Make (struct
    type nonrec t = t

    let compare a b =
      let a = to_bytes a and b = to_bytes b in
      Bytes.compare a b
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

let name = "ML_DSA_44"

let title = "An ML-DSA-44 signature"

let size = Ml_dsa_44.signature_size

let to_bytes s = Bytes.copy s

let to_string s = Bytes.to_string (to_bytes s)

let of_bytes_opt s = if Bytes.length s = size then Some s else None

let of_string_opt s = of_bytes_opt (Bytes.of_string s)

type Base58.data += Data of t

let b58check_encoding =
  Base58.register_encoding
    ~prefix:Base58.Prefix.mldsa44_signature
    ~length:size
    ~to_raw:to_string
    ~of_raw:of_string_opt
    ~wrap:(fun x -> Data x)

let () = Base58.check_encoded_prefix b58check_encoding "mdsig" 3316

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

let sign ?watermark ((signing_key, _verification_key) : Secret_key.t) msg =
  let msg =
    Blake2B.to_bytes @@ Blake2B.hash_bytes
    @@ match watermark with None -> [msg] | Some prefix -> [prefix; msg]
  in
  let randomness = Rand.generate Ml_dsa_44.signing_randomness_size in
  match Ml_dsa_44.sign ~randomness signing_key msg with
  | Ok signature -> Ml_dsa_44.signature_to_bytes signature
  | Error err -> Stdlib.failwith err

let check ?watermark verification_key signature msg =
  let msg =
    Blake2B.to_bytes @@ Blake2B.hash_bytes
    @@ match watermark with None -> [msg] | Some prefix -> [prefix; msg]
  in
  match Ml_dsa_44.signature_from_bytes_opt signature with
  | Some signature -> Ml_dsa_44.verify_b verification_key msg signature
  | None -> false

let generate_key ?seed () =
  let seed =
    match seed with
    | Some seed -> seed
    | None -> Rand.generate Ml_dsa_44.key_gen_randomness_size
  in
  match Ml_dsa_44.generate_key_pair ~seed with
  | Ok (signing_key, verification_key) ->
      ( Public_key.hash verification_key,
        verification_key,
        (signing_key, verification_key) )
  | Error err -> Stdlib.failwith err

let deterministic_nonce sk msg =
  let key = Secret_key.to_bytes sk in
  Hacl.Hash.SHA256.HMAC.digest ~key ~msg

let deterministic_nonce_hash sk msg =
  Blake2B.to_bytes (Blake2B.hash_bytes [deterministic_nonce sk msg])

let pop_verify _ ?msg:_ _ = false

include (Compare.Bytes : Compare.S with type t := t)
