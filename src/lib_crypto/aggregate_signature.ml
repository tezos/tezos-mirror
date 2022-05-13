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

open Error_monad

type public_key_hash = Bls12_381 of Bls.Public_key_hash.t

type public_key = Bls12_381 of Bls.Public_key.t

type secret_key = Bls12_381 of Bls.Secret_key.t

module Public_key_hash = struct
  type t = public_key_hash = Bls12_381 of Bls.Public_key_hash.t

  let name = "Aggregate_signature.Public_key_hash"

  let title = "A Bls12_381 public key hash"

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
    def "aggregate_public_key_hash" ~description:title
    @@ union
         [
           case
             (Tag 0)
             Bls.Public_key_hash.encoding
             ~title:"Bls12_381"
             (function Bls12_381 x -> Some x)
             (function x -> Bls12_381 x);
         ]

  let to_bytes s = Data_encoding.Binary.to_bytes_exn raw_encoding s

  let of_bytes_opt s = Data_encoding.Binary.of_bytes_opt raw_encoding s

  let to_string s = Bytes.to_string (to_bytes s)

  let of_string_opt s = of_bytes_opt (Bytes.of_string s)

  let size = 1 + Bls.Public_key_hash.size

  let zero = Bls12_381 Bls.Public_key_hash.zero

  include Helpers.MakeRaw (struct
    type nonrec t = t

    let name = name

    let of_bytes_opt = of_bytes_opt

    let of_string_opt = of_string_opt

    let to_string = to_string
  end)

  let of_b58check_opt s =
    match Base58.decode s with
    | Some (Bls.Public_key_hash.Data pkh) -> Some (Bls12_381 pkh)
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
    | Bls12_381 pkh -> Bls.Public_key_hash.to_b58check pkh

  let to_short_b58check = function
    | Bls12_381 pkh -> Bls.Public_key_hash.to_short_b58check pkh

  let to_path key l =
    match key with
    | Bls12_381 h -> "bls12_381" :: Bls.Public_key_hash.to_path h l

  let of_path = function
    | "bls12_381" :: q ->
        Bls.Public_key_hash.of_path q |> Option.map (fun pkh -> Bls12_381 pkh)
    | _ -> assert false

  let of_path_exn = function
    | "bls12_381" :: q -> Bls12_381 (Bls.Public_key_hash.of_path_exn q)
    | _ -> assert false

  let path_length = 1 + Bls.Public_key_hash.path_length

  let prefix_path _ = assert false (* unused *)

  let seeded_hash = Stdlib.Hashtbl.seeded_hash

  let hash = Stdlib.Hashtbl.hash

  include Compare.Make (struct
    type nonrec t = t

    let compare a b =
      match (a, b) with
      | Bls12_381 x, Bls12_381 y -> Bls.Public_key_hash.compare x y
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
    RPC_arg.like
      rpc_arg
      ~descr:"An aggregate public key hash (Base58Check-encoded)"
      "pkh"

  module Logging = struct
    let tag = Tag.def ~doc:title name pp
  end
end

module Public_key = struct
  type t = public_key = Bls12_381 of Bls.Public_key.t

  let name = "Aggregate_signature.Public_key"

  let title = "A bls12_381 public key"

  let hash pk =
    match pk with
    | Bls12_381 pk -> Public_key_hash.Bls12_381 (Bls.Public_key.hash pk)

  include Compare.Make (struct
    type nonrec t = t

    let compare a b =
      match (a, b) with Bls12_381 x, Bls12_381 y -> Bls.Public_key.compare x y
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
    | Some (Bls.Public_key.Data public_key) -> Some (Bls12_381 public_key)
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

  let to_b58check = function Bls12_381 pk -> Bls.Public_key.to_b58check pk

  let to_short_b58check = function
    | Bls12_381 pk -> Bls.Public_key.to_short_b58check pk

  let of_bytes_without_validation b =
    let tag = Bytes.(get_int8 b 0) in
    let b = Bytes.(sub b 1 (length b - 1)) in
    match tag with
    | 0 ->
        Option.bind (Bls.Public_key.of_bytes_without_validation b) (fun pk ->
            Some (Bls12_381 pk))
    | _ -> None

  include Helpers.MakeEncoder (struct
    type nonrec t = t

    let name = name

    let title = title

    let raw_encoding =
      let open Data_encoding in
      def "aggregate_public_key" ~description:title
      @@ union
           [
             case
               (Tag 0)
               Bls.Public_key.encoding
               ~title:"Bls12_381"
               (function Bls12_381 x -> Some x)
               (function x -> Bls12_381 x);
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
  type t = secret_key = Bls12_381 of Bls.Secret_key.t

  let name = "Aggregate_signature.Secret_key"

  let title = "A Bls12_381 secret key"

  let to_public_key = function
    | Bls12_381 sk -> Public_key.Bls12_381 (Bls.Secret_key.to_public_key sk)

  include Compare.Make (struct
    type nonrec t = t

    let compare a b =
      match (a, b) with Bls12_381 x, Bls12_381 y -> Bls.Secret_key.compare x y
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
    | Some (Bls.Secret_key.Data sk) -> Some (Bls12_381 sk)
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

  let to_b58check = function Bls12_381 sk -> Bls.Secret_key.to_b58check sk

  let to_short_b58check = function
    | Bls12_381 sk -> Bls.Secret_key.to_short_b58check sk

  include Helpers.MakeEncoder (struct
    type nonrec t = t

    let name = name

    let title = title

    let raw_encoding =
      let open Data_encoding in
      def "aggregate_secret_key" ~description:title
      @@ union
           [
             case
               (Tag 0)
               Bls.Secret_key.encoding
               ~title:"Bls12_381"
               (function Bls12_381 x -> Some x)
               (function x -> Bls12_381 x);
           ]

    let of_b58check = of_b58check

    let of_b58check_opt = of_b58check_opt

    let of_b58check_exn = of_b58check_exn

    let to_b58check = to_b58check

    let to_short_b58check = to_short_b58check
  end)

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)
end

type signature = Bls12_381 of Bls.t | Unknown of Bytes.t

type t = signature

let name = "Aggregate signature"

let title = "A Bls12_381 signature"

let size = Bls.size

let to_bytes = function Bls12_381 b -> Bls.to_bytes b | Unknown b -> b

let of_bytes_opt s = if Bytes.length s = size then Some (Unknown s) else None

let to_string s = Bytes.to_string (to_bytes s)

let of_string_opt s = of_bytes_opt (Bytes.of_string s)

type Base58.data += Data of t

let b58check_encoding =
  Base58.register_encoding
    ~prefix:Base58.Prefix.generic_aggregate_signature
    ~length:size
    ~to_raw:to_string
    ~of_raw:of_string_opt
    ~wrap:(fun x -> Data x)

let () = Base58.check_encoded_prefix b58check_encoding "asig" 141

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

let of_b58check_opt s =
  if TzString.has_prefix ~prefix:Bls.b58check_encoding.encoded_prefix s then
    Option.map (fun x -> Bls12_381 x) (Bls.of_b58check_opt s)
  else Base58.simple_decode b58check_encoding s

let of_b58check_exn s =
  match of_b58check_opt s with
  | Some x -> x
  | None -> Format.kasprintf Stdlib.failwith "Unexpected data (%s)" name

let of_b58check s =
  match of_b58check_opt s with
  | Some x -> Ok x
  | None -> error_with "Failed to read a b58check_encoding data (%s): %S" name s

let to_b58check = function
  | Bls12_381 b -> Bls.to_b58check b
  | Unknown b -> Base58.simple_encode b58check_encoding (Unknown b)

let to_short_b58check = function
  | Bls12_381 b -> Bls.to_short_b58check b
  | Unknown b -> Base58.simple_encode b58check_encoding (Unknown b)

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

let sign (Secret_key.Bls12_381 sk) bytes = Bls12_381 (Bls.sign sk bytes)

let check pk signature message =
  match (pk, signature) with
  | Public_key.Bls12_381 pk, Unknown signature ->
      Bls.of_bytes_opt signature
      |> Option.map (fun signature -> Bls.check pk signature message)
      |> Option.value ~default:false
  | Public_key.Bls12_381 pk, Bls12_381 signature ->
      Bls.check pk signature message

let generate_key ?seed () =
  let pkh, pk, sk = Bls.generate_key ?seed () in
  ( Public_key_hash.Bls12_381 pkh,
    Public_key.Bls12_381 pk,
    Secret_key.Bls12_381 sk )

let aggregate_check pks signature =
  let pks =
    List.map (fun (Public_key.Bls12_381 pk, bytes) -> (pk, bytes)) pks
  in
  match signature with
  | Bls12_381 signature -> Bls.aggregate_check pks signature
  | Unknown signature ->
      Bls.of_bytes_opt signature
      |> Option.map (Bls.aggregate_check pks)
      |> Option.value ~default:false

let aggregate_signature_opt signatures =
  let open Result_syntax in
  let aux acc s =
    match s with
    | Bls12_381 s -> return @@ (s :: acc)
    | Unknown s ->
        let* s = Bls.of_bytes s in
        return (s :: acc)
  in
  match List.fold_left_e aux [] signatures with
  | Ok signatures ->
      Bls.aggregate_signature_opt signatures
      |> Option.map (fun s -> Bls12_381 s)
  | Error _ -> None
