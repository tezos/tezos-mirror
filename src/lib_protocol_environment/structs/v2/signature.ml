(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Tezos_stdlib
open Tezos_rpc
open Tezos_error_monad
open Tezos_crypto

(* ========================== *)
(* we copy paste lib_crypto/signature.ml to override the module P256, once this
   is done directly in lib_crypto this section can be removed. *)

include Signature
open Error_monad

module Public_key_hash_in = struct
  type t = public_key_hash =
    | Ed25519 of Ed25519.Public_key_hash.t
    | Secp256k1 of Secp256k1.Public_key_hash.t
    | P256 of P256.Public_key_hash.t

  let name = "Signature.Public_key_hash"

  let title = "A Ed25519, Secp256k1, or P256 public key hash"

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
         [ case
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
             (function x -> P256 x) ]

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
    | Some (Ed25519.Public_key_hash.Data pkh) ->
        Some (Ed25519 pkh)
    | Some (Secp256k1.Public_key_hash.Data pkh) ->
        Some (Secp256k1 pkh)
    | Some (P256.Public_key_hash.Data pkh) ->
        Some (P256 pkh)
    | _ ->
        None

  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x ->
        x
    | None ->
        Format.kasprintf Stdlib.failwith "Unexpected data (%s)" name

  let of_b58check s =
    match of_b58check_opt s with
    | Some x ->
        Ok x
    | None ->
        generic_error "Failed to read a b58check_encoding data (%s): %S" name s

  let to_b58check = function
    | Ed25519 pkh ->
        Ed25519.Public_key_hash.to_b58check pkh
    | Secp256k1 pkh ->
        Secp256k1.Public_key_hash.to_b58check pkh
    | P256 pkh ->
        P256.Public_key_hash.to_b58check pkh

  let to_short_b58check = function
    | Ed25519 pkh ->
        Ed25519.Public_key_hash.to_short_b58check pkh
    | Secp256k1 pkh ->
        Secp256k1.Public_key_hash.to_short_b58check pkh
    | P256 pkh ->
        P256.Public_key_hash.to_short_b58check pkh

  let to_path key l =
    match key with
    | Ed25519 h ->
        "ed25519" :: Ed25519.Public_key_hash.to_path h l
    | Secp256k1 h ->
        "secp256k1" :: Secp256k1.Public_key_hash.to_path h l
    | P256 h ->
        "p256" :: P256.Public_key_hash.to_path h l

  let of_path = function
    | "ed25519" :: q -> (
      match Ed25519.Public_key_hash.of_path q with
      | Some pkh ->
          Some (Ed25519 pkh)
      | None ->
          None )
    | "secp256k1" :: q -> (
      match Secp256k1.Public_key_hash.of_path q with
      | Some pkh ->
          Some (Secp256k1 pkh)
      | None ->
          None )
    | "p256" :: q -> (
      match P256.Public_key_hash.of_path q with
      | Some pkh ->
          Some (P256 pkh)
      | None ->
          None )
    | _ ->
        assert false

  (* FIXME classification des erreurs *)

  let of_path_exn = function
    | "ed25519" :: q ->
        Ed25519 (Ed25519.Public_key_hash.of_path_exn q)
    | "secp256k1" :: q ->
        Secp256k1 (Secp256k1.Public_key_hash.of_path_exn q)
    | "p256" :: q ->
        P256 (P256.Public_key_hash.of_path_exn q)
    | _ ->
        assert false

  (* FIXME classification des erreurs *)

  let path_length =
    let l1 = Ed25519.Public_key_hash.path_length
    and l2 = Secp256k1.Public_key_hash.path_length
    and l3 = P256.Public_key_hash.path_length in
    assert (Compare.Int.(l1 = l2)) ;
    assert (Compare.Int.(l1 = l3)) ;
    1 + l1

  let prefix_path _ = assert false (* unused *)

  let seeded_hash = Stdlib.Hashtbl.seeded_hash

  let hash = Stdlib.Hashtbl.hash

  include Compare.Make (struct
    type nonrec t = t

    let compare a b =
      match (a, b) with
      | (Ed25519 x, Ed25519 y) ->
          Ed25519.Public_key_hash.compare x y
      | (Secp256k1 x, Secp256k1 y) ->
          Secp256k1.Public_key_hash.compare x y
      | (P256 x, P256 y) ->
          P256.Public_key_hash.compare x y
      | _ ->
          Stdlib.compare a b
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

  let name = "Signature.Public_key"

  let title = "A Ed25519, Secp256k1, or P256 public key"

  let hash pk =
    match pk with
    | Ed25519 pk ->
        Public_key_hash_in.Ed25519 (Ed25519.Public_key.hash pk)
    | Secp256k1 pk ->
        Public_key_hash_in.Secp256k1 (Secp256k1.Public_key.hash pk)
    | P256 pk ->
        Public_key_hash_in.P256 (P256.Public_key.hash pk)

  include Compare.Make (struct
    type nonrec t = t

    let compare a b =
      match (a, b) with
      | (Ed25519 x, Ed25519 y) ->
          Ed25519.Public_key.compare x y
      | (Secp256k1 x, Secp256k1 y) ->
          Secp256k1.Public_key.compare x y
      | (P256 x, P256 y) ->
          P256.Public_key.compare x y
      | _ ->
          Stdlib.compare a b
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
    | Some (Ed25519.Public_key.Data public_key) ->
        Some (Ed25519 public_key)
    | Some (Secp256k1.Public_key.Data public_key) ->
        Some (Secp256k1 public_key)
    | Some (P256.Public_key.Data public_key) ->
        Some (P256 public_key)
    | _ ->
        None

  let of_b58check_exn s =
    match of_b58check_opt s with
    | Some x ->
        x
    | None ->
        Format.kasprintf Stdlib.failwith "Unexpected data (%s)" name

  let of_b58check s =
    match of_b58check_opt s with
    | Some x ->
        Ok x
    | None ->
        generic_error "Failed to read a b58check_encoding data (%s): %S" name s

  let to_b58check = function
    | Ed25519 pk ->
        Ed25519.Public_key.to_b58check pk
    | Secp256k1 pk ->
        Secp256k1.Public_key.to_b58check pk
    | P256 pk ->
        P256.Public_key.to_b58check pk

  let to_short_b58check = function
    | Ed25519 pk ->
        Ed25519.Public_key.to_short_b58check pk
    | Secp256k1 pk ->
        Secp256k1.Public_key.to_short_b58check pk
    | P256 pk ->
        P256.Public_key.to_short_b58check pk

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
    | _ ->
        None

  include Helpers.MakeEncoder (struct
    type nonrec t = t

    let name = name

    let title = title

    let raw_encoding =
      let open Data_encoding in
      def "public_key" ~description:title
      @@ union
           [ case
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
               (function x -> P256 x) ]

    let of_b58check = of_b58check

    let of_b58check_opt = of_b58check_opt

    let of_b58check_exn = of_b58check_exn

    let to_b58check = to_b58check

    let to_short_b58check = to_short_b58check
  end)

  let size pk = Data_encoding.Binary.length encoding pk

  let pp ppf t = Format.fprintf ppf "%s" (to_b58check t)
end

(* ========================== *)

module Public_key_hash = struct
  include Public_key_hash_in

  module Set = struct
    include Set.Legacy

    let random_elt = Set.random_elt

    let encoding = Set.encoding
  end

  module Map = struct
    include Map.Legacy

    let encoding = Map.encoding
  end

  module Table = struct
    include Table.Legacy

    let encoding = Table.encoding
  end
end
