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

(** {2 Hash Types} *)

(** The signature of an abstract hash type, as produced by functor
    {!Make_Blake2B}. The {!t} type is abstracted for separating the
    various kinds of hashes in the system at typing time. Each type is
    equipped with functions to use it as is of as keys in the database
    or in memory sets and maps. *)

module type MINIMAL_HASH = sig
  type t

  val name : string

  val title : string

  val pp : Format.formatter -> t -> unit

  val pp_short : Format.formatter -> t -> unit

  include Compare.S with type t := t

  val hash_bytes : ?key:Bytes.t -> Bytes.t list -> t

  val hash_string : ?key:string -> string list -> t

  val zero : t
end

module type RAW_DATA = sig
  type t

  val size : int (* in bytes *)

  val to_hex : t -> Hex.t

  val of_hex : Hex.t -> t tzresult

  val of_hex_opt : Hex.t -> t option

  val of_hex_exn : Hex.t -> t

  val to_string : t -> string

  val of_string : string -> t tzresult

  val of_string_opt : string -> t option

  val of_string_exn : string -> t

  val to_bytes : t -> Bytes.t

  val of_bytes : Bytes.t -> t tzresult

  val of_bytes_opt : Bytes.t -> t option

  val of_bytes_exn : Bytes.t -> t
end

module type B58_DATA = sig
  type t

  val to_b58check : t -> string

  val to_short_b58check : t -> string

  val of_b58check : string -> t tzresult

  val of_b58check_exn : string -> t

  val of_b58check_opt : string -> t option

  type Base58.data += Data of t

  val b58check_encoding : t Base58.encoding
end

module type ENCODER = sig
  type t

  val encoding : t Data_encoding.t

  val rpc_arg : t RPC_arg.t
end

module type PVSS = sig
  type proof

  module Clear_share : sig
    type t
  end

  module Commitment : sig
    type t
  end

  module Encrypted_share : sig
    type t
  end

  module Public_key : sig
    type t

    include B58_DATA with type t := t

    include ENCODER with type t := t
  end
end

module type INDEXES = sig
  type t

  val hash : t -> int

  val to_path : t -> string list -> string list

  val of_path : string list -> t option

  val of_path_exn : string list -> t

  val prefix_path : string -> string list

  val path_length : int

  module Set : sig
    include Set.S with type elt = t

    val random_elt : t -> elt

    val encoding : t Data_encoding.t
  end

  module Map : sig
    include Map.S with type key = t

    val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
  end

  module Table : sig
    include Hashtbl.S with type key = t

    val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
  end

  module Error_table : sig
    include Tezos_lwt_result_stdlib.Lwtreslib.Hashtbl.S_LWT with type key = t
  end

  module WeakRingTable : sig
    include Ringo.CACHE_MAP with type key = t

    val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
  end
end

module type HASH = sig
  include MINIMAL_HASH

  include RAW_DATA with type t := t

  include B58_DATA with type t := t

  include ENCODER with type t := t

  include INDEXES with type t := t
end

module type MERKLE_TREE = sig
  type elt

  val elt_bytes : elt -> Bytes.t

  include HASH

  val compute : elt list -> t

  val empty : t

  type path = Left of path * t | Right of t * path | Op

  val path_encoding : path Data_encoding.t

  val bounded_path_encoding : ?max_length:int -> unit -> path Data_encoding.t

  val compute_path : elt list -> int -> path

  val check_path : path -> elt -> t * int
end

module type SIGNATURE = sig
  module Public_key_hash : sig
    type t

    val pp : Format.formatter -> t -> unit

    val pp_short : Format.formatter -> t -> unit

    include Compare.S with type t := t

    include RAW_DATA with type t := t

    include B58_DATA with type t := t

    include ENCODER with type t := t

    include INDEXES with type t := t

    val zero : t

    module Logging : sig
      val tag : t Tag.def
    end
  end

  module Public_key : sig
    type t

    val pp : Format.formatter -> t -> unit

    include Compare.S with type t := t

    include B58_DATA with type t := t

    include ENCODER with type t := t

    val hash : t -> Public_key_hash.t

    val size : t -> int (* in bytes *)
  end

  module Secret_key : sig
    type t

    val pp : Format.formatter -> t -> unit

    include Compare.S with type t := t

    include B58_DATA with type t := t

    include ENCODER with type t := t

    val to_public_key : t -> Public_key.t
  end

  type t

  val pp : Format.formatter -> t -> unit

  include Compare.S with type t := t

  include B58_DATA with type t := t

  include ENCODER with type t := t

  val zero : t

  type watermark

  val sign : ?watermark:watermark -> Secret_key.t -> Bytes.t -> t

  val check : ?watermark:watermark -> Public_key.t -> t -> Bytes.t -> bool

  val generate_key :
    ?seed:Bytes.t -> unit -> Public_key_hash.t * Public_key.t * Secret_key.t

  val deterministic_nonce : Secret_key.t -> Bytes.t -> Bytes.t

  val deterministic_nonce_hash : Secret_key.t -> Bytes.t -> Bytes.t
end

module type FIELD = sig
  type t

  val size : int (* in bytes *)

  val to_bytes : t -> bytes

  val of_bytes_opt : bytes -> t option

  val of_bytes_exn : bytes -> t

  (* Check if a point, represented as a byte array, is in the field *)
  val check_bytes : Bytes.t -> bool

  (* The zero point *)
  val zero : t

  (* The one point *)
  val one : t

  (* Add a pair of points *)
  val add : t -> t -> t

  (* Multiply a pair of points *)
  val mul : t -> t -> t

  (* Negate a point *)
  val negate : t -> t

  (* Try to compute the inverse of a point *)
  val inverse_exn : t -> t

  (* Try to compute the inverse of a point *)
  val inverse_opt : t -> t option

  (* Equality check *)
  val eq : t -> t -> bool
end

module type CURVE = sig
  type t

  val size : int (* in bytes *)

  val to_bytes : t -> bytes

  val of_bytes_opt : bytes -> t option

  val of_bytes_exn : bytes -> t

  module Scalar : FIELD

  (* Check if a point, represented as a byte array, is on the curve *)
  val check_bytes : Bytes.t -> bool

  (* The zero point on the curve *)
  val zero : t

  (* A fixed generator of the elliptic curve *)
  val one : t

  (* Add a pair of points on the curve *)
  val add : t -> t -> t

  (* Multiply a point by a scalar *)
  val mul : t -> Scalar.t -> t

  (* Negate a point on the curve *)
  val negate : t -> t

  (* Equality test *)
  val eq : t -> t -> bool
end

module type PAIRING = sig
  module Gt : FIELD

  module G1 : CURVE

  module G2 : CURVE

  val miller_loop : (G1.t * G2.t) list -> Gt.t

  val final_exponentiation : Gt.t -> Gt.t option

  val pairing : G1.t -> G2.t -> Gt.t
end
