(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Error_monad

type public_key_hash =
  | Ed25519 of Ed25519.Public_key_hash.t
  | Secp256k1 of Secp256k1.Public_key_hash.t
  | P256 of P256.Public_key_hash.t
  | Bls of Bls.Public_key_hash.t

type public_key =
  | Ed25519 of Ed25519.Public_key.t
  | Secp256k1 of Secp256k1.Public_key.t
  | P256 of P256.Public_key.t
  | Bls of Bls.Public_key.t

type secret_key =
  | Ed25519 of Ed25519.Secret_key.t
  | Secp256k1 of Secp256k1.Secret_key.t
  | P256 of P256.Secret_key.t
  | Bls of Bls.Secret_key.t

type watermark = Signature_v0.watermark =
  | Block_header of Chain_id.t
  | Endorsement of Chain_id.t
  | Generic_operation
  | Custom of Bytes.t

module Public_key_hash = struct
  type t = public_key_hash =
    | Ed25519 of Ed25519.Public_key_hash.t
    | Secp256k1 of Secp256k1.Public_key_hash.t
    | P256 of P256.Public_key_hash.t
    | Bls of Bls.Public_key_hash.t

  let name = "Signature.Public_key_hash"

  let title = "A Ed25519, Secp256k1, P256, or BLS public key hash"

  let is_bls : t -> bool = function
    | Bls _ -> true
    | Ed25519 _ | Secp256k1 _ | P256 _ -> false

  type Base58.data += Data of t (* unused *)

  let b58check_encoding =
    (* unused *)
    Base58.register_encoding
      ~prefix:"\255\255"
      ~length:2
      ~to_raw:(fun _ -> assert false)
      ~of_raw:(fun _ -> assert false)
      ~wrap:(fun x -> Data x)

  let raw_encoding =
    let open Data_encoding in
    def "public_key_hash" ~description:title
    @@ union
         [
           case
             (Tag 0)
             Ed25519.Public_key_hash.encoding
             ~title:"Ed25519"
             (function Ed25519 x -> Some x | _ -> None)
             (function x -> Ed25519 x);
           case
             (Tag 1)
             Secp256k1.Public_key_hash.encoding
             ~title:"Secp256k1"
             (function Secp256k1 x -> Some x | _ -> None)
             (function x -> Secp256k1 x);
           case
             (Tag 2)
             ~title:"P256"
             P256.Public_key_hash.encoding
             (function P256 x -> Some x | _ -> None)
             (function x -> P256 x);
           case
             (Tag 3)
             ~title:"Bls"
             Bls.Public_key_hash.encoding
             (function Bls x -> Some x | _ -> None)
             (function x -> Bls x);
         ]

  let to_bytes s = Data_encoding.Binary.to_bytes_exn raw_encoding s

  let of_bytes_opt s = Data_encoding.Binary.of_bytes_opt raw_encoding s

  let to_string s = Bytes.to_string (to_bytes s)

  let of_string_opt s = of_bytes_opt (Bytes.of_string s)

  let size = 1 + Ed25519.size

  let zero = Ed25519 Ed25519.Public_key_hash.zero

  include Helpers.MakeRaw (struct
    type nonrec t = t

    let name = name

    let of_bytes_opt = of_bytes_opt

    let of_string_opt = of_string_opt

    let to_string = to_string
  end)

  let of_b58check_opt s =
    match Base58.decode s with
    | Some (Ed25519.Public_key_hash.Data pkh) -> Some (Ed25519 pkh)
    | Some (Secp256k1.Public_key_hash.Data pkh) -> Some (Secp256k1 pkh)
    | Some (P256.Public_key_hash.Data pkh) -> Some (P256 pkh)
    | Some (Bls.Public_key_hash.Data pkh) -> Some (Bls pkh)
    | _ -> None

  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x -> x
    | None -> Format.kasprintf Stdlib.failwith "Unexpected data (%s)" name

  let of_b58check s =
    match of_b58check_opt s with
    | Some x -> Ok x
    | None ->
        error_with "Failed to read a b58check_encoding data (%s): %S" name s

  let to_b58check = function
    | Ed25519 pkh -> Ed25519.Public_key_hash.to_b58check pkh
    | Secp256k1 pkh -> Secp256k1.Public_key_hash.to_b58check pkh
    | P256 pkh -> P256.Public_key_hash.to_b58check pkh
    | Bls pkh -> Bls.Public_key_hash.to_b58check pkh

  let to_short_b58check = function
    | Ed25519 pkh -> Ed25519.Public_key_hash.to_short_b58check pkh
    | Secp256k1 pkh -> Secp256k1.Public_key_hash.to_short_b58check pkh
    | P256 pkh -> P256.Public_key_hash.to_short_b58check pkh
    | Bls pkh -> Bls.Public_key_hash.to_short_b58check pkh

  let to_path key l =
    match key with
    | Ed25519 h -> "ed25519" :: Ed25519.Public_key_hash.to_path h l
    | Secp256k1 h -> "secp256k1" :: Secp256k1.Public_key_hash.to_path h l
    | P256 h -> "p256" :: P256.Public_key_hash.to_path h l
    | Bls h -> "bls" :: Bls.Public_key_hash.to_path h l

  let of_path = function
    | "ed25519" :: q -> (
        match Ed25519.Public_key_hash.of_path q with
        | Some pkh -> Some (Ed25519 pkh)
        | None -> None)
    | "secp256k1" :: q -> (
        match Secp256k1.Public_key_hash.of_path q with
        | Some pkh -> Some (Secp256k1 pkh)
        | None -> None)
    | "p256" :: q -> (
        match P256.Public_key_hash.of_path q with
        | Some pkh -> Some (P256 pkh)
        | None -> None)
    | "bls" :: q -> (
        match Bls.Public_key_hash.of_path q with
        | Some pkh -> Some (Bls pkh)
        | None -> None)
    | _ -> assert false

  (* FIXME classification des erreurs *)

  let of_path_exn = function
    | "ed25519" :: q -> Ed25519 (Ed25519.Public_key_hash.of_path_exn q)
    | "secp256k1" :: q -> Secp256k1 (Secp256k1.Public_key_hash.of_path_exn q)
    | "p256" :: q -> P256 (P256.Public_key_hash.of_path_exn q)
    | "bls" :: q -> Bls (Bls.Public_key_hash.of_path_exn q)
    | _ -> assert false

  (* FIXME classification des erreurs *)

  let path_length =
    let l1 = Ed25519.Public_key_hash.path_length
    and l2 = Secp256k1.Public_key_hash.path_length
    and l3 = P256.Public_key_hash.path_length
    and l4 = Bls.Public_key_hash.path_length in
    assert (Compare.Int.(l1 = l2)) ;
    assert (Compare.Int.(l1 = l3)) ;
    assert (Compare.Int.(l1 = l4)) ;
    1 + l1

  let prefix_path _ = assert false (* unused *)

  let seeded_hash = Stdlib.Hashtbl.seeded_hash

  let hash = Stdlib.Hashtbl.hash

  include Compare.Make (struct
    type nonrec t = t

    let compare a b =
      match (a, b) with
      | Ed25519 x, Ed25519 y -> Ed25519.Public_key_hash.compare x y
      | Secp256k1 x, Secp256k1 y -> Secp256k1.Public_key_hash.compare x y
      | P256 x, P256 y -> P256.Public_key_hash.compare x y
      | Bls x, Bls y -> Bls.Public_key_hash.compare x y
      | _ -> Stdlib.compare a b
  end)

  include Helpers.MakeEncoder (struct
    type nonrec t = t

    let name = name

    let title = title

    let raw_encoding = raw_encoding

    let of_b58check = of_b58check

    let of_b58check_opt = of_b58check_opt

    let of_b58check_exn = of_b58check_exn

    let to_b58check = to_b58check

    let to_short_b58check = to_short_b58check
  end)

  include Helpers.MakeIterator (struct
    type nonrec t = t

    let hash = hash

    let seeded_hash = seeded_hash

    let compare = compare

    let equal = equal

    let encoding = encoding
  end)

  let rpc_arg =
    Tezos_rpc.Arg.like
      rpc_arg
      ~descr:"A Secp256k1 of a Ed25519 public key hash (Base58Check-encoded)"
      "pkh"

  module Logging = struct
    let tag = Tag.def ~doc:title name pp
  end
end

module Public_key = struct
  type t = public_key =
    | Ed25519 of Ed25519.Public_key.t
    | Secp256k1 of Secp256k1.Public_key.t
    | P256 of P256.Public_key.t
    | Bls of Bls.Public_key.t

  let name = "Signature.Public_key"

  let title = "A Ed25519, Secp256k1, P256 or a BLS public key"

  let hash pk =
    match pk with
    | Ed25519 pk -> Public_key_hash.Ed25519 (Ed25519.Public_key.hash pk)
    | Secp256k1 pk -> Public_key_hash.Secp256k1 (Secp256k1.Public_key.hash pk)
    | P256 pk -> Public_key_hash.P256 (P256.Public_key.hash pk)
    | Bls pk -> Public_key_hash.Bls (Bls.Public_key.hash pk)

  include Compare.Make (struct
    type nonrec t = t

    let compare a b =
      match (a, b) with
      | Ed25519 x, Ed25519 y -> Ed25519.Public_key.compare x y
      | Secp256k1 x, Secp256k1 y -> Secp256k1.Public_key.compare x y
      | P256 x, P256 y -> P256.Public_key.compare x y
      | Bls x, Bls y -> Bls.Public_key.compare x y
      | Ed25519 _, (Secp256k1 _ | P256 _ | Bls _) -> -1
      | Secp256k1 _, (P256 _ | Bls _) -> -1
      | P256 _, Bls _ -> -1
      | Bls _, (P256 _ | Secp256k1 _ | Ed25519 _) -> 1
      | P256 _, (Secp256k1 _ | Ed25519 _) -> 1
      | Secp256k1 _, Ed25519 _ -> 1
  end)

  type Base58.data += Data of t (* unused *)

  let b58check_encoding =
    (* unused *)
    Base58.register_encoding
      ~prefix:"\255\255"
      ~length:2
      ~to_raw:(fun _ -> assert false)
      ~of_raw:(fun _ -> assert false)
      ~wrap:(fun x -> Data x)

  let of_b58check_opt s =
    match Base58.decode s with
    | Some (Ed25519.Public_key.Data public_key) -> Some (Ed25519 public_key)
    | Some (Secp256k1.Public_key.Data public_key) -> Some (Secp256k1 public_key)
    | Some (P256.Public_key.Data public_key) -> Some (P256 public_key)
    | Some (Bls.Public_key.Data public_key) -> Some (Bls public_key)
    | _ -> None

  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x -> x
    | None -> Format.kasprintf Stdlib.failwith "Unexpected data (%s)" name

  let of_b58check s =
    match of_b58check_opt s with
    | Some x -> Ok x
    | None ->
        error_with "Failed to read a b58check_encoding data (%s): %S" name s

  let to_b58check = function
    | Ed25519 pk -> Ed25519.Public_key.to_b58check pk
    | Secp256k1 pk -> Secp256k1.Public_key.to_b58check pk
    | P256 pk -> P256.Public_key.to_b58check pk
    | Bls pk -> Bls.Public_key.to_b58check pk

  let to_short_b58check = function
    | Ed25519 pk -> Ed25519.Public_key.to_short_b58check pk
    | Secp256k1 pk -> Secp256k1.Public_key.to_short_b58check pk
    | P256 pk -> P256.Public_key.to_short_b58check pk
    | Bls pk -> Bls.Public_key.to_short_b58check pk

  let of_bytes_without_validation b =
    let tag = Bytes.(get_int8 b 0) in
    let b = Bytes.(sub b 1 (length b - 1)) in
    match tag with
    | 0 ->
        Option.bind
          (Ed25519.Public_key.of_bytes_without_validation b)
          (fun pk -> Some (Ed25519 pk))
    | 1 ->
        Option.bind
          (Secp256k1.Public_key.of_bytes_without_validation b)
          (fun pk -> Some (Secp256k1 pk))
    | 2 ->
        Option.bind (P256.Public_key.of_bytes_without_validation b) (fun pk ->
            Some (P256 pk))
    | 3 ->
        Option.bind (Bls.Public_key.of_bytes_without_validation b) (fun pk ->
            Some (Bls pk))
    | _ -> None

  include Helpers.MakeEncoder (struct
    type nonrec t = t

    let name = name

    let title = title

    let raw_encoding =
      let open Data_encoding in
      def "public_key" ~description:title
      @@ union
           [
             case
               (Tag 0)
               Ed25519.Public_key.encoding
               ~title:"Ed25519"
               (function Ed25519 x -> Some x | _ -> None)
               (function x -> Ed25519 x);
             case
               (Tag 1)
               Secp256k1.Public_key.encoding
               ~title:"Secp256k1"
               (function Secp256k1 x -> Some x | _ -> None)
               (function x -> Secp256k1 x);
             case
               ~title:"P256"
               (Tag 2)
               P256.Public_key.encoding
               (function P256 x -> Some x | _ -> None)
               (function x -> P256 x);
             case
               ~title:"Bls"
               (Tag 3)
               Bls.Public_key.encoding
               (function Bls x -> Some x | _ -> None)
               (function x -> Bls x);
           ]

    let of_b58check = of_b58check

    let of_b58check_opt = of_b58check_opt

    let of_b58check_exn = of_b58check_exn

    let to_b58check = to_b58check

    let to_short_b58check = to_short_b58check
  end)

  let size pk = Data_encoding.Binary.length encoding pk

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)
end

module Secret_key = struct
  type t = secret_key =
    | Ed25519 of Ed25519.Secret_key.t
    | Secp256k1 of Secp256k1.Secret_key.t
    | P256 of P256.Secret_key.t
    | Bls of Bls.Secret_key.t

  let name = "Signature.Secret_key"

  let title = "A Ed25519, Secp256k1, P256 or a BLS secret key"

  let to_public_key = function
    | Ed25519 sk -> Public_key.Ed25519 (Ed25519.Secret_key.to_public_key sk)
    | Secp256k1 sk ->
        Public_key.Secp256k1 (Secp256k1.Secret_key.to_public_key sk)
    | P256 sk -> Public_key.P256 (P256.Secret_key.to_public_key sk)
    | Bls sk -> Public_key.Bls (Bls.Secret_key.to_public_key sk)

  include Compare.Make (struct
    type nonrec t = t

    let compare a b =
      match (a, b) with
      | Ed25519 x, Ed25519 y -> Ed25519.Secret_key.compare x y
      | Secp256k1 x, Secp256k1 y -> Secp256k1.Secret_key.compare x y
      | P256 x, P256 y -> P256.Secret_key.compare x y
      | Bls x, Bls y -> Bls.Secret_key.compare x y
      | _ -> Stdlib.compare a b
  end)

  type Base58.data += Data of t (* unused *)

  let b58check_encoding =
    (* unused *)
    Base58.register_encoding
      ~prefix:"\255\255"
      ~length:2
      ~to_raw:(fun _ -> assert false)
      ~of_raw:(fun _ -> assert false)
      ~wrap:(fun x -> Data x)

  let of_b58check_opt b =
    match Base58.decode b with
    | Some (Ed25519.Secret_key.Data sk) -> Some (Ed25519 sk)
    | Some (Secp256k1.Secret_key.Data sk) -> Some (Secp256k1 sk)
    | Some (P256.Secret_key.Data sk) -> Some (P256 sk)
    | Some (Bls.Secret_key.Data sk) -> Some (Bls sk)
    | _ -> None

  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x -> x
    | None -> Format.kasprintf Stdlib.failwith "Unexpected data (%s)" name

  let of_b58check s =
    match of_b58check_opt s with
    | Some x -> Ok x
    | None ->
        error_with "Failed to read a b58check_encoding data (%s): %S" name s

  let to_b58check = function
    | Ed25519 sk -> Ed25519.Secret_key.to_b58check sk
    | Secp256k1 sk -> Secp256k1.Secret_key.to_b58check sk
    | P256 sk -> P256.Secret_key.to_b58check sk
    | Bls sk -> Bls.Secret_key.to_b58check sk

  let to_short_b58check = function
    | Ed25519 sk -> Ed25519.Secret_key.to_short_b58check sk
    | Secp256k1 sk -> Secp256k1.Secret_key.to_short_b58check sk
    | P256 sk -> P256.Secret_key.to_short_b58check sk
    | Bls sk -> Bls.Secret_key.to_short_b58check sk

  include Helpers.MakeEncoder (struct
    type nonrec t = t

    let name = name

    let title = title

    let raw_encoding =
      let open Data_encoding in
      def "secret_key" ~description:title
      @@ union
           [
             case
               (Tag 0)
               Ed25519.Secret_key.encoding
               ~title:"Ed25519"
               (function Ed25519 x -> Some x | _ -> None)
               (function x -> Ed25519 x);
             case
               (Tag 1)
               Secp256k1.Secret_key.encoding
               ~title:"Secp256k1"
               (function Secp256k1 x -> Some x | _ -> None)
               (function x -> Secp256k1 x);
             case
               (Tag 2)
               ~title:"P256"
               P256.Secret_key.encoding
               (function P256 x -> Some x | _ -> None)
               (function x -> P256 x);
             case
               (Tag 3)
               ~title:"Bls"
               Bls.Secret_key.encoding
               (function Bls x -> Some x | _ -> None)
               (function x -> Bls x);
           ]

    let of_b58check = of_b58check

    let of_b58check_opt = of_b58check_opt

    let of_b58check_exn = of_b58check_exn

    let to_b58check = to_b58check

    let to_short_b58check = to_short_b58check
  end)

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)
end

type signature =
  | Ed25519 of Ed25519.t
  | Secp256k1 of Secp256k1.t
  | P256 of P256.t
  | Bls of Bls.t
  | Unknown of Bytes.t

type prefix = Bls_prefix of Bytes.t

type splitted = {prefix : prefix option; suffix : Bytes.t}

type t = signature

let name = "Signature.V3"

let title = "A Ed25519, Secp256k1, P256 or BLS signature"

let to_bytes = function
  | Ed25519 b -> Ed25519.to_bytes b
  | Secp256k1 b -> Secp256k1.to_bytes b
  | P256 b -> P256.to_bytes b
  | Bls b -> Bls.to_bytes b
  | Unknown b -> b

let of_bytes_opt s =
  let len = Bytes.length s in
  if len = Bls.size then Option.map (fun b -> Bls b) (Bls.of_bytes_opt s)
  else if len = Ed25519.size then Some (Unknown s)
  else None

let () =
  assert (Ed25519.size = 64) ;
  assert (Secp256k1.size = 64) ;
  assert (P256.size = 64) ;
  assert (Bls.size = 96)

type Base58.data += Data_unknown of Bytes.t

let unknown_b58check_encoding =
  Base58.register_encoding
    ~prefix:Base58.Prefix.generic_signature
    ~length:Ed25519.size
    ~to_raw:Bytes.to_string
    ~of_raw:(fun s -> Some (Bytes.of_string s))
    ~wrap:(fun x -> Data_unknown x)

let () = Base58.check_encoded_prefix unknown_b58check_encoding "sig" 96

type Base58.data += Data of t (* unused *)

let b58check_encoding =
  (* unused *)
  Base58.register_encoding
    ~prefix:"\255\255"
    ~length:2
    ~to_raw:(fun _ -> assert false)
    ~of_raw:(fun _ -> assert false)
    ~wrap:(fun x -> Data x)

include Compare.Make (struct
  type nonrec t = t

  let compare a b =
    let a = to_bytes a and b = to_bytes b in
    Bytes.compare a b
end)

let of_b58check_opt s =
  if TzString.has_prefix ~prefix:Ed25519.b58check_encoding.encoded_prefix s then
    Option.map (fun x -> Ed25519 x) (Ed25519.of_b58check_opt s)
  else if
    TzString.has_prefix ~prefix:Secp256k1.b58check_encoding.encoded_prefix s
  then Option.map (fun x -> Secp256k1 x) (Secp256k1.of_b58check_opt s)
  else if TzString.has_prefix ~prefix:P256.b58check_encoding.encoded_prefix s
  then Option.map (fun x -> P256 x) (P256.of_b58check_opt s)
  else if TzString.has_prefix ~prefix:Bls.b58check_encoding.encoded_prefix s
  then Option.map (fun x -> Bls x) (Bls.of_b58check_opt s)
  else
    Option.map
      (fun x -> Unknown x)
      (Base58.simple_decode unknown_b58check_encoding s)

let of_b58check_exn s =
  match of_b58check_opt s with
  | Some x -> x
  | None -> Format.kasprintf Stdlib.failwith "Unexpected data (%s)" name

let of_b58check s =
  match of_b58check_opt s with
  | Some x -> Ok x
  | None -> error_with "Failed to read a b58check_encoding data (%s): %S" name s

let to_b58check = function
  | Ed25519 b -> Ed25519.to_b58check b
  | Secp256k1 b -> Secp256k1.to_b58check b
  | P256 b -> P256.to_b58check b
  | Bls b -> Bls.to_b58check b
  | Unknown b -> Base58.simple_encode unknown_b58check_encoding b

let to_short_b58check = function
  | Ed25519 b -> Ed25519.to_short_b58check b
  | Secp256k1 b -> Secp256k1.to_short_b58check b
  | P256 b -> P256.to_short_b58check b
  | Bls b -> Bls.to_short_b58check b
  | Unknown b -> Base58.simple_encode unknown_b58check_encoding b

let raw_encoding =
  conv
    to_bytes
    (fun b ->
      match of_bytes_opt b with
      | None ->
          Format.kasprintf
            Stdlib.failwith
            "Not a valid signature: %a"
            Hex.pp
            (Hex.of_bytes b)
      | Some s -> s)
    Variable.bytes

include Helpers.MakeEncoder (struct
  type nonrec t = t

  let name = name

  let title = title

  let raw_encoding = raw_encoding

  let of_b58check = of_b58check

  let of_b58check_opt = of_b58check_opt

  let of_b58check_exn = of_b58check_exn

  let to_b58check = to_b58check

  let to_short_b58check = to_short_b58check
end)

let to_bytes s = Data_encoding.Binary.to_bytes_exn raw_encoding s

let of_bytes_opt s = Data_encoding.Binary.of_bytes_opt raw_encoding s

let to_string s = Bytes.to_string (to_bytes s)

let of_string_opt s = of_bytes_opt (Bytes.of_string s)

include Helpers.MakeRaw (struct
  type nonrec t = t

  let name = name

  let of_bytes_opt = of_bytes_opt

  let of_string_opt = of_string_opt

  let to_string = to_string
end)

let size t = Data_encoding.Binary.length encoding t

let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)

let of_ed25519 s = Ed25519 s

let of_secp256k1 s = Secp256k1 s

let of_p256 s = P256 s

let of_bls s = Bls s

let zero = of_ed25519 Ed25519.zero

(* NOTE: At the moment, only BLS signatures can be encoded with a tag. We impose
   this restriction so that there is only one valid binary representation for a
   same signature (modulo malleability).

   We reserve the tags 0, 1, 2 and 255 for tags of the other signatures if we
   decide to unify signature representation one day.*)
let prefix_encoding =
  let open Data_encoding in
  def
    "bls_signature_prefix"
    ~description:"The prefix of a BLS signature, i.e. the first 32 bytes."
  @@ union
       [
         case
           (Tag 3)
           ~title:"Bls_prefix"
           (Fixed.bytes (Bls.size - Ed25519.size))
           (function Bls_prefix x -> Some x)
           (function x -> Bls_prefix x);
       ]

let split_signature = function
  | (Ed25519 _ | Secp256k1 _ | P256 _) as s ->
      {prefix = None; suffix = to_bytes s}
  | Bls s ->
      let s = Bls.to_bytes s in
      let prefix = Bytes.sub s 0 32 in
      let suffix = Bytes.sub s 32 64 in
      {prefix = Some (Bls_prefix prefix); suffix}
  | Unknown s ->
      assert (Compare.Int.(Bytes.length s = 64)) ;
      {prefix = None; suffix = s}

let of_splitted {prefix; suffix} =
  let open Option_syntax in
  match prefix with
  | None -> of_bytes_opt suffix
  | Some (Bls_prefix prefix) ->
      let+ s = Bls.of_bytes_opt (Bytes.cat prefix suffix) in
      Bls s

let bytes_of_watermark = function
  | Block_header chain_id ->
      Bytes.cat (Bytes.of_string "\x01") (Chain_id.to_bytes chain_id)
  | Endorsement chain_id ->
      Bytes.cat (Bytes.of_string "\x02") (Chain_id.to_bytes chain_id)
  | Generic_operation -> Bytes.of_string "\x03"
  | Custom bytes -> bytes

let pp_watermark ppf =
  let open Format in
  function
  | Block_header chain_id -> fprintf ppf "Block-header: %a" Chain_id.pp chain_id
  | Endorsement chain_id -> fprintf ppf "Endorsement: %a" Chain_id.pp chain_id
  | Generic_operation -> pp_print_string ppf "Generic-operation"
  | Custom bytes ->
      let hexed = Hex.of_bytes bytes |> Hex.show in
      fprintf
        ppf
        "Custom: 0x%s"
        (try String.sub hexed 0 10 ^ "..." with Invalid_argument _ -> hexed)

let sign ?watermark secret_key message =
  let watermark = Option.map bytes_of_watermark watermark in
  match secret_key with
  | Secret_key.Ed25519 sk -> of_ed25519 (Ed25519.sign ?watermark sk message)
  | Secp256k1 sk -> of_secp256k1 (Secp256k1.sign ?watermark sk message)
  | P256 sk -> of_p256 (P256.sign ?watermark sk message)
  | Bls sk -> of_bls (Bls.sign ?watermark sk message)

let check ?watermark public_key signature message =
  let watermark = Option.map bytes_of_watermark watermark in
  match (public_key, signature) with
  | Public_key.Ed25519 pk, Unknown signature -> (
      match Ed25519.of_bytes_opt signature with
      | Some s -> Ed25519.check ?watermark pk s message
      | None -> false)
  | Public_key.Secp256k1 pk, Unknown signature -> (
      match Secp256k1.of_bytes_opt signature with
      | Some s -> Secp256k1.check ?watermark pk s message
      | None -> false)
  | Public_key.P256 pk, Unknown signature -> (
      match P256.of_bytes_opt signature with
      | Some s -> P256.check ?watermark pk s message
      | None -> false)
  | Public_key.Bls pk, Unknown signature -> (
      match Bls.of_bytes_opt signature with
      | Some s -> Bls.check ?watermark pk s message
      | None -> false)
  | Public_key.Ed25519 pk, Ed25519 signature ->
      Ed25519.check ?watermark pk signature message
  | Public_key.Secp256k1 pk, Secp256k1 signature ->
      Secp256k1.check ?watermark pk signature message
  | Public_key.P256 pk, P256 signature ->
      P256.check ?watermark pk signature message
  | Public_key.Bls pk, Bls signature ->
      Bls.check ?watermark pk signature message
  | _ -> false

type algo = Ed25519 | Secp256k1 | P256 | Bls

let hardcoded_sk =
  let ed, secp, p, bls =
    ( Secret_key.of_b58check_exn
        "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh",
      Secret_key.of_b58check_exn
        "spsk2XJu4wuYsHeuDaCktD3ECnnpn574ceSWHEJVvXTt7JP6ztySCL",
      Secret_key.of_b58check_exn
        "p2sk2k6YAkNJ8CySZCS3vGA5Ht6Lj6LXG3yb8UrHvMKZy7Ab8JUtWh",
      Secret_key.of_b58check_exn
        "BLsk1hfuv6V8JJRaLDBJgPTRGLKusTZnTmWGrvSKYzUaMuzvPLmeGG" )
  in
  function Ed25519 -> ed | Secp256k1 -> secp | P256 -> p | Bls -> bls

let hardcoded_pk =
  (* precompute signatures *)
  let ed, secp, p, bls =
    ( Secret_key.to_public_key (hardcoded_sk Ed25519),
      Secret_key.to_public_key (hardcoded_sk Secp256k1),
      Secret_key.to_public_key (hardcoded_sk P256),
      Secret_key.to_public_key (hardcoded_sk Bls) )
  in
  function Ed25519 -> ed | Secp256k1 -> secp | P256 -> p | Bls -> bls

let hardcoded_msg = Bytes.of_string "Cheers"

let hardcoded_sig =
  (* precompute signatures *)
  let ed, secp, p, bls =
    ( sign (hardcoded_sk Ed25519) hardcoded_msg,
      sign (hardcoded_sk Secp256k1) hardcoded_msg,
      sign (hardcoded_sk P256) hardcoded_msg,
      sign (hardcoded_sk Bls) hardcoded_msg )
  in
  function Ed25519 -> ed | Secp256k1 -> secp | P256 -> p | Bls -> bls

let fast_fake_sign ?watermark:_ (sk : Secret_key.t) _msg =
  let algo =
    match sk with
    | Ed25519 _ -> Ed25519
    | Secp256k1 _ -> Secp256k1
    | P256 _ -> P256
    | Bls _ -> Bls
  in
  hardcoded_sig algo

(* Fast checking does not simulate computation and directly returns true*)
let fast_fake_check ?watermark:_ _pk _signature _msg = true

let fake_check ?watermark:_ (pk : Public_key.t) _signature _msg =
  (* call a successfull [check] to simulate crypto costs *)
  let algo =
    match pk with
    | Ed25519 _ -> Ed25519
    | Secp256k1 _ -> Secp256k1
    | P256 _ -> P256
    | Bls _ -> Bls
  in
  ignore (check (hardcoded_pk algo) (hardcoded_sig algo) hardcoded_msg) ;
  true

(* yes_crypto semantics:
   - Yes: use the normal [sign] and [check] functions to simulate crypto costs,
     but [check] always returns true.
   - Fast: return hardcoded signatures and skip any crypto costs.
   In both modes, verification always succeeds. *)
let sign =
  match Helpers.yes_crypto_kind with
  | Fast -> fast_fake_sign
  | Yes -> sign
  | No -> sign

let check =
  match Helpers.yes_crypto_kind with
  | Fast -> fast_fake_check
  | Yes -> fake_check
  | No -> check

let append ?watermark sk msg = Bytes.cat msg (to_bytes (sign ?watermark sk msg))

let concat msg signature = Bytes.cat msg (to_bytes signature)

let algos = [Ed25519; Secp256k1; P256; Bls]

let fake_generate_key (pkh, pk, _) =
  let sk_of_pk (pk : public_key) : secret_key =
    let pk_b = Data_encoding.Binary.to_bytes_exn Public_key.encoding pk in
    let sk_b = Bytes.sub pk_b 0 33 in
    let sk = Data_encoding.Binary.of_bytes_exn Secret_key.encoding sk_b in
    sk
  in
  let fake_sk = sk_of_pk pk in
  (pkh, pk, fake_sk)

let generate_key ?(algo = Ed25519) ?seed () =
  match algo with
  | Ed25519 ->
      let pkh, pk, sk = Ed25519.generate_key ?seed () in
      (Public_key_hash.Ed25519 pkh, Public_key.Ed25519 pk, Secret_key.Ed25519 sk)
  | Secp256k1 ->
      let pkh, pk, sk = Secp256k1.generate_key ?seed () in
      ( Public_key_hash.Secp256k1 pkh,
        Public_key.Secp256k1 pk,
        Secret_key.Secp256k1 sk )
  | P256 ->
      let pkh, pk, sk = P256.generate_key ?seed () in
      (Public_key_hash.P256 pkh, Public_key.P256 pk, Secret_key.P256 sk)
  | Bls ->
      let pkh, pk, sk = Bls.generate_key ?seed () in
      (Public_key_hash.Bls pkh, Public_key.Bls pk, Secret_key.Bls sk)

let fake_generate_key ?(algo = Ed25519) ?seed () =
  let true_keys = generate_key ~algo ?seed () in
  fake_generate_key true_keys

let generate_key =
  match Helpers.yes_crypto_kind with
  | Fast | Yes ->
      (* We keep the original keys generation to stay as close as possible of the
               initial performance. *)
      fake_generate_key
  | No -> generate_key

let deterministic_nonce sk msg =
  match sk with
  | Secret_key.Ed25519 sk -> Ed25519.deterministic_nonce sk msg
  | Secret_key.Secp256k1 sk -> Secp256k1.deterministic_nonce sk msg
  | Secret_key.P256 sk -> P256.deterministic_nonce sk msg
  | Secret_key.Bls sk -> Bls.deterministic_nonce sk msg

let deterministic_nonce_hash sk msg =
  match sk with
  | Secret_key.Ed25519 sk -> Ed25519.deterministic_nonce_hash sk msg
  | Secret_key.Secp256k1 sk -> Secp256k1.deterministic_nonce_hash sk msg
  | Secret_key.P256 sk -> P256.deterministic_nonce_hash sk msg
  | Secret_key.Bls sk -> Bls.deterministic_nonce_hash sk msg

let pop_verify pubkey ?msg proof =
  match pubkey with
  | Public_key.Bls pk -> Bls12_381_signature.MinPk.Pop.pop_verify pk ?msg proof
  | _ -> false

module Of_V0 = struct
  let public_key_hash : Signature_v0.Public_key_hash.t -> Public_key_hash.t =
    function
    | Signature_v0.Ed25519 k -> Ed25519 k
    | Signature_v0.Secp256k1 k -> Secp256k1 k
    | Signature_v0.P256 k -> P256 k

  let public_key : Signature_v0.Public_key.t -> Public_key.t = function
    | Signature_v0.Ed25519 k -> Ed25519 k
    | Signature_v0.Secp256k1 k -> Secp256k1 k
    | Signature_v0.P256 k -> P256 k

  let secret_key : Signature_v0.Secret_key.t -> Secret_key.t = function
    | Signature_v0.Ed25519 k -> Ed25519 k
    | Signature_v0.Secp256k1 k -> Secp256k1 k
    | Signature_v0.P256 k -> P256 k

  let signature : Signature_v0.t -> t = function
    | Signature_v0.Ed25519 k -> Ed25519 k
    | Signature_v0.Secp256k1 k -> Secp256k1 k
    | Signature_v0.P256 k -> P256 k
    | Signature_v0.Unknown k -> Unknown k
end

module Of_V1 = struct
  let public_key_hash : Signature_v1.Public_key_hash.t -> Public_key_hash.t =
    function
    | Signature_v1.Ed25519 k -> Ed25519 k
    | Signature_v1.Secp256k1 k -> Secp256k1 k
    | Signature_v1.P256 k -> P256 k
    | Signature_v1.Bls k -> Bls k

  let public_key : Signature_v1.Public_key.t -> Public_key.t = function
    | Signature_v1.Ed25519 k -> Ed25519 k
    | Signature_v1.Secp256k1 k -> Secp256k1 k
    | Signature_v1.P256 k -> P256 k
    | Signature_v1.Bls k -> Bls k

  let secret_key : Signature_v1.Secret_key.t -> Secret_key.t = function
    | Signature_v1.Ed25519 k -> Ed25519 k
    | Signature_v1.Secp256k1 k -> Secp256k1 k
    | Signature_v1.P256 k -> P256 k
    | Signature_v1.Bls k -> Bls k

  let signature : Signature_v1.t -> t = function
    | Signature_v1.Ed25519 k -> Ed25519 k
    | Signature_v1.Secp256k1 k -> Secp256k1 k
    | Signature_v1.P256 k -> P256 k
    | Signature_v1.Unknown k -> Unknown k
    | Signature_v1.Bls k -> Bls k
end

module Of_V2 = struct
  let public_key_hash : Signature_v2.Public_key_hash.t -> Public_key_hash.t =
    function
    | Signature_v2.Ed25519 k -> Ed25519 k
    | Signature_v2.Secp256k1 k -> Secp256k1 k
    | Signature_v2.P256 k -> P256 k
    | Signature_v2.Bls k -> Bls k

  let public_key : Signature_v2.Public_key.t -> Public_key.t = function
    | Signature_v2.Ed25519 k -> Ed25519 k
    | Signature_v2.Secp256k1 k -> Secp256k1 k
    | Signature_v2.P256 k -> P256 k
    | Signature_v2.Bls k -> Bls k

  let secret_key : Signature_v2.Secret_key.t -> Secret_key.t = function
    | Signature_v2.Ed25519 k -> Ed25519 k
    | Signature_v2.Secp256k1 k -> Secp256k1 k
    | Signature_v2.P256 k -> P256 k
    | Signature_v2.Bls k -> Bls k

  let signature : Signature_v2.t -> t = function
    | Signature_v2.Ed25519 k -> Ed25519 k
    | Signature_v2.Secp256k1 k -> Secp256k1 k
    | Signature_v2.P256 k -> P256 k
    | Signature_v2.Unknown k -> Unknown k
    | Signature_v2.Bls k -> Bls k
end
