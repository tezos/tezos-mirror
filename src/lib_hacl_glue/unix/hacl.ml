(* The MIT License (MIT)
 *
 *   Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>
 *
 *   Permission is hereby granted, free of charge, to any person obtaining a copy
 *   of this software and associated documentation files (the "Software"), to deal
 *   in the Software without restriction, including without limitation the rights
 *   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *   copies of the Software, and to permit persons to whom the Software is
 *   furnished to do so, subject to the following conditions:
 *
 *   The above copyright notice and this permission notice shall be included in all
 *   copies or substantial portions of the Software.
 *
 *   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 *   SOFTWARE. *)

open Hacl_star

type secret

type public

module Rand = struct
  let write out =
    if Hacl.RandomBuffer.Noalloc.randombytes ~out then ()
    else failwith "Error getting random bytes"

  let gen size =
    match Hacl.RandomBuffer.randombytes ~size with
    | Some buf -> buf
    | None -> failwith "Error getting random bytes"
end

module Hash = struct
  open Hacl_star__SharedDefs

  module type HASH_ALG = sig
    val alg : HashDefs.alg

    val size : int
  end

  module type DIRECT_HASH = sig
    val size : int

    val digest : Bytes.t -> Bytes.t
  end

  module type INCREMENTAL_HASH = sig
    type state

    val init : unit -> state

    val update : state -> Bytes.t -> unit

    val finish : state -> Bytes.t
  end

  module Make (S : HASH_ALG) = struct
    type state = EverCrypt.Hash.t

    let size = S.size

    let init () = EverCrypt.Hash.init ~alg:S.alg

    let update st msg = EverCrypt.Hash.update ~st ~msg

    let finish st = EverCrypt.Hash.finish ~st

    let digest msg = EverCrypt.Hash.hash ~alg:S.alg ~msg

    module HMAC = struct
      let digest ~key ~msg = EverCrypt.HMAC.mac ~alg:S.alg ~key ~msg
    end
  end

  module type S = sig
    include DIRECT_HASH

    include INCREMENTAL_HASH

    module HMAC : sig
      val digest : key:Bytes.t -> msg:Bytes.t -> Bytes.t
    end
  end

  module SHA256 = struct
    module H = Make (struct
      let alg = HashDefs.SHA2_256

      let size = 32
    end)

    include H
  end

  module SHA512 = struct
    module H = Make (struct
      let alg = HashDefs.SHA2_512

      let size = 64
    end)

    include H
  end

  module SHA3_256 = struct
    let size = 32

    let digest = Hacl.SHA3_256.hash
  end

  module SHA3_512 = struct
    let size = 64

    let digest = Hacl.SHA3_512.hash
  end

  module Keccak_256 = struct
    let size = 32

    let digest msg =
      let keccak_256 = Hacl.Keccak.keccak ~rate:1088 ~capacity:512 ~suffix:1 in
      keccak_256 ~msg ~size
  end
end

module Blake2b = struct
  type t = Bytes.t

  type hash = Hash of Bytes.t

  let direct ?(key = Bytes.create 0) inbuf len =
    if len < 1 || len > 64 then
      invalid_arg "Blake2b.direct: size must be between 1 and 64" ;
    (* HACL* doesn't yet provide a multiplexing interface for Blake2b so we
     * perform this check here and use the faster version if possible *)
    if AutoConfig2.(has_feature VEC256) then
      Hash (Hacl.Blake2b_256.hash ~key inbuf len)
    else Hash (Hacl.Blake2b_32.hash ~key inbuf len)
end

module Nonce = struct
  type t = Bytes.t

  let size = 24

  let gen () = Rand.gen size

  (* Attention to the endianess here. Ref Tezos_stdlib.TzEndian *)
  let rec incr_byte b step byteno =
    let res = Bytes.get_uint16_be b byteno + step in
    let lo = res land 0xffff in
    let hi = res asr 16 in
    Bytes.set_uint16_be b byteno lo ;
    if hi = 0 || byteno = 0 then () else incr_byte b hi (byteno - 2)

  let increment ?(step = 1) nonce =
    let new_nonce = Bytes.create 24 in
    Bytes.blit nonce 0 new_nonce 0 24 ;
    incr_byte new_nonce step 22 ;
    new_nonce

  let of_bytes buf = if Bytes.length buf <> size then None else Some buf

  let of_bytes_exn buf =
    match of_bytes buf with
    | Some s -> s
    | None -> invalid_arg "Hacl.Nonce.of_bytes_exn: invalid length"
end

module Secretbox = struct
  type key = Bytes.t

  let keybytes = 32

  let tagbytes = 16

  let unsafe_of_bytes buf =
    if Bytes.length buf <> keybytes then
      invalid_arg
        (Printf.sprintf
           "Secretbox.unsafe_of_bytes: buffer must be %d bytes long"
           keybytes) ;
    buf

  let blit_of_bytes buf pos =
    if Bytes.length buf < keybytes then
      invalid_arg
        (Printf.sprintf
           "Secretbox.blit_of_bytes: buffer must be at least %d bytes long"
           keybytes) ;
    let key = Bytes.make keybytes '\x00' in
    Bytes.blit buf pos key 0 keybytes ;
    key

  let genkey () = Rand.gen 32

  let secretbox ~key ~nonce ~msg ~cmsg =
    if Hacl.NaCl.Noalloc.Easy.secretbox ~pt:msg ~n:nonce ~key ~ct:cmsg then ()
    else failwith "Secretbox encryption failed"

  let secretbox_open ~key ~nonce ~cmsg ~msg =
    Hacl.NaCl.Noalloc.Easy.secretbox_open ~ct:cmsg ~n:nonce ~key ~pt:msg
end

module Box = struct
  type combined

  type _ key =
    | Sk : Bytes.t -> secret key
    | Pk : Bytes.t -> public key
    | Ck : Bytes.t -> combined key

  let skbytes = 32

  let pkbytes = 32

  let ckbytes = 32

  let tagbytes = 16

  let unsafe_to_bytes : type a. a key -> Bytes.t = function
    | Pk buf -> buf
    | Sk buf -> buf
    | Ck buf -> buf

  let blit_to_bytes : type a. a key -> ?pos:int -> Bytes.t -> unit =
   fun key ?(pos = 0) buf ->
    match key with
    | Pk pk -> Bytes.blit pk 0 buf pos pkbytes
    | Sk sk -> Bytes.blit sk 0 buf pos skbytes
    | Ck ck -> Bytes.blit ck 0 buf pos ckbytes

  let equal : type a. a key -> a key -> bool =
   fun a b ->
    (* TODO re-group once coverage ppx is updated *)
    match (a, b) with
    | (Pk a, Pk b) -> Bytes.equal a b
    | (Sk a, Sk b) -> Bytes.equal a b
    | (Ck a, Ck b) -> Bytes.equal a b

  let unsafe_sk_of_bytes buf =
    if Bytes.length buf <> skbytes then
      invalid_arg
        (Printf.sprintf
           "Box.unsafe_sk_of_bytes: buffer must be %d bytes long"
           skbytes) ;
    Sk buf

  let unsafe_pk_of_bytes buf =
    if Bytes.length buf <> pkbytes then
      invalid_arg
        (Printf.sprintf
           "Box.unsafe_pk_of_bytes: buffer must be %d bytes long"
           pkbytes) ;
    Pk buf

  let unsafe_ck_of_bytes buf =
    if Bytes.length buf <> ckbytes then
      invalid_arg
        (Printf.sprintf
           "Box.unsafe_ck_of_bytes: buffer must be %d bytes long"
           ckbytes) ;
    Ck buf

  let of_seed ?(pos = 0) buf =
    let buflen = Bytes.length buf in
    if pos < 0 || pos + skbytes > buflen then
      invalid_arg
        (Printf.sprintf
           "Box.of_seed: invalid pos (%d) or buffer size (%d)"
           pos
           buflen) ;
    let sk = Bytes.make skbytes '\x00' in
    Bytes.blit buf pos sk 0 skbytes ;
    Sk sk

  let basepoint = Bytes.init 32 (function 0 -> '\x09' | _ -> '\x00')

  let neuterize (Sk sk) =
    Pk (EverCrypt.Curve25519.scalarmult ~scalar:sk ~point:basepoint)

  let keypair () =
    let sk = Sk (Rand.gen skbytes) in
    (neuterize sk, sk)

  let dh (Pk pk) (Sk sk) =
    match Hacl.NaCl.box_beforenm ~pk ~sk with
    | Some ck -> Ck ck
    | None -> failwith "Error computing box_beforenm"

  let box ~k:(Ck k) ~nonce ~msg ~cmsg =
    if not @@ Hacl.NaCl.Noalloc.Easy.box_afternm ~pt:msg ~n:nonce ~ck:k ~ct:cmsg
    then failwith "Box: encryption error"

  let box_open ~k:(Ck k) ~nonce ~cmsg ~msg =
    Hacl.NaCl.Noalloc.Easy.box_open_afternm ~ct:cmsg ~n:nonce ~ck:k ~pt:msg

  let box_noalloc ~k:(Ck k) ~nonce ~tag ~buf =
    if
      not
      @@ Hacl.NaCl.Noalloc.Detached.box_afternm
           ~pt:buf
           ~n:nonce
           ~ck:k
           ~ct:buf
           ~tag
    then failwith "Box: encryption error"

  let box_open_noalloc ~k:(Ck k) ~nonce ~tag ~buf =
    Hacl.NaCl.Noalloc.Detached.box_open_afternm
      ~pt:buf
      ~n:nonce
      ~ck:k
      ~ct:buf
      ~tag
end

module type SIGNATURE = sig
  type _ key

  val size : int

  val pk_size : int

  val sk_size : int

  val compare : 'a key -> 'a key -> int

  val equal : 'a key -> 'a key -> bool

  val sk_of_bytes : Bytes.t -> secret key option

  val pk_of_bytes : Bytes.t -> public key option

  val pk_of_bytes_without_validation : Bytes.t -> public key option

  val neuterize : 'a key -> public key

  val keypair : unit -> public key * secret key

  val to_bytes : _ key -> Bytes.t

  val blit_to_bytes : _ key -> ?pos:int -> Bytes.t -> unit

  val sign : sk:secret key -> msg:Bytes.t -> Bytes.t

  val verify : pk:public key -> msg:Bytes.t -> signature:Bytes.t -> bool
end

module Ed25519 : SIGNATURE = struct
  type _ key = Sk : Bytes.t -> secret key | Pk : Bytes.t -> public key

  let size = 64

  let pk_size = 32

  let sk_size = 32

  let to_bytes : type a. a key -> Bytes.t = function
    | Pk buf -> Bytes.copy buf
    | Sk buf -> Bytes.copy buf

  let sk_of_bytes seed =
    if Bytes.length seed <> sk_size then None else Some (Sk (Bytes.copy seed))

  let pk_of_bytes pk =
    if Bytes.length pk <> pk_size then None else Some (Pk (Bytes.copy pk))

  let pk_of_bytes_without_validation = pk_of_bytes

  let blit_to_bytes : type a. a key -> ?pos:int -> Bytes.t -> unit =
   fun key ?(pos = 0) buf ->
    match key with
    | Pk pk -> Bytes.blit pk 0 buf pos pk_size
    | Sk sk -> Bytes.blit sk 0 buf pos sk_size

  let compare : type a. a key -> a key -> int =
   fun a b ->
    (* TODO re-group once coverage ppx is updated *)
    match (a, b) with
    | (Pk a, Pk b) -> Bytes.compare a b
    | (Sk a, Sk b) -> Bytes.compare a b

  let equal : type a. a key -> a key -> bool = fun a b -> compare a b = 0

  let neuterize : type a. a key -> public key = function
    | Pk pk -> Pk pk
    | Sk sk -> Pk (Hacl.Ed25519.secret_to_public ~sk)

  let keypair () =
    let sk = Sk (Rand.gen sk_size) in
    (neuterize sk, sk)

  let sign ~sk:(Sk sk) ~msg = Hacl.Ed25519.sign ~sk ~msg

  let verify ~pk:(Pk pk) ~msg ~signature =
    Hacl.Ed25519.verify ~pk ~msg ~signature
end

module P256 : SIGNATURE = struct
  type _ key = Sk : Bytes.t -> secret key | Pk : Bytes.t -> public key

  let size = 64

  (* A public key is an elliptic curve point with 2 32-byte coordinates (x, y).
   * Internally we use 3 formats to represent public keys:
   * - "raw":          64 bytes, representing the concatenation of the 2 components
   * - "compressed":   33 bytes, in which the first component is replaced by a single
   *                   byte (either '\x02' or '\x03'). This is the default representation
   *                   used throughout the interface.
   * - "uncompressed": 65 bytes, same as "raw" but with one additional leading '\x04' byte,
   *                   which identifies it as an uncompressed public key.
   * We bind the HACL* functions which convert between these representations.
   * More details about how they work can be found in Section 2.3.3 of this document:
   * http://www.secg.org/sec1-v2.pdf *)

  let pk_size_raw = 64

  let pk_size = (pk_size_raw / 2) + 1

  let pk_size_uncompressed = pk_size_raw + 1

  let sk_size = 32

  (* A public key is generated from a secret key using the first step of the
   * Elliptic Curve Diffie-Hellman (ECDH) key agreement protocol, in which
   * sk is multiplied with the base point of the curve. *)
  let pk_of_sk sk = Hacl.P256.dh_initiator ~sk

  let valid_pk pk = Hacl.P256.valid_pk ~pk

  (* Generate a random sk_size buffer until it is valid to be used as
   * secret key, i.e. non-zero and smaller than the prime order.
   * This is also used to generate signing secrets. *)
  let rec get_valid_sk () =
    let sk = Rand.gen sk_size in
    if Hacl.P256.valid_sk ~sk then sk else get_valid_sk ()

  let compare : type a. a key -> a key -> int =
   fun a b ->
    (* TODO re-group once coverage ppx is updated *)
    match (a, b) with
    | (Pk a, Pk b) -> Bytes.compare a b
    | (Sk a, Sk b) -> Bytes.compare a b

  let equal : type a. a key -> a key -> bool = fun a b -> compare a b = 0

  let neuterize : type a. a key -> public key = function
    | Pk pk -> Pk pk
    | Sk sk -> (
        match pk_of_sk sk with
        | Some pk -> Pk pk
        | None -> failwith "P256.neuterize: failure")

  (* This function accepts a buffer representing a public key in either the
   * compressed or the uncompressed form. *)
  let pk_of_bytes_without_validation : Bytes.t -> public key option =
   fun buf ->
    Option.map
      (fun pk -> Pk pk)
      (match Bytes.length buf with
      | len when len = pk_size -> Hacl.P256.compressed_to_raw buf
      | len when len = pk_size_uncompressed -> Hacl.P256.uncompressed_to_raw buf
      | _ -> None)

  let pk_of_bytes : Bytes.t -> public key option =
   fun buf ->
    Option.bind (pk_of_bytes_without_validation buf) (fun (Pk pk) ->
        if valid_pk pk then Some (Pk pk) else None)

  let sk_of_bytes : Bytes.t -> secret key option =
   fun buf ->
    if Bytes.length buf = sk_size && Hacl.P256.valid_sk ~sk:buf then
      Some (Sk (Bytes.copy buf))
    else None

  let to_bytes_with_compression : type a. ?compress:bool -> a key -> Bytes.t =
   fun ?compress:(comp = true) -> function
    | Sk sk -> Bytes.copy sk
    | Pk pk ->
        if comp then Hacl.P256.raw_to_compressed pk
        else Hacl.P256.raw_to_uncompressed pk

  let to_bytes : type a. a key -> Bytes.t =
   fun key -> to_bytes_with_compression ~compress:true key

  let blit_to_bytes_with_compression :
      type a. ?compress:bool -> a key -> ?pos:int -> Bytes.t -> unit =
   fun ?compress:(comp = true) key ?(pos = 0) buf ->
    match key with
    | Sk sk ->
        let len = Bytes.length sk in
        Bytes.blit sk 0 buf pos len
    | Pk pk ->
        if pos = 0 then
          if comp then Hacl.P256.Noalloc.raw_to_compressed ~p:pk ~result:buf
          else Hacl.P256.Noalloc.raw_to_uncompressed ~p:pk ~result:buf
        else if comp then
          let out = Hacl.P256.raw_to_compressed pk in
          Bytes.blit out 0 buf pos pk_size
        else
          let out = Hacl.P256.raw_to_uncompressed pk in
          Bytes.blit out 0 buf pos pk_size_uncompressed

  let blit_to_bytes : type a. a key -> ?pos:int -> Bytes.t -> unit =
   fun key ?pos buf ->
    blit_to_bytes_with_compression ~compress:true key ?pos buf

  let keypair () : public key * secret key =
    let sk = get_valid_sk () in
    match pk_of_sk sk with
    | Some pk -> (Pk pk, Sk sk)
    | None -> failwith "P256.keypair: failure"

  let sign ~sk:(Sk sk) ~msg =
    (* A random non-zero signing secret k is generated which, similar to
     * secret keys, needs to be non-zero and smaller than the prime order. *)
    let k = get_valid_sk () in
    let res = Hacl.P256.sign ~sk ~msg ~k in
    Bytes.fill k 0 32 '\x00' ;
    match res with
    | Some signature -> signature
    | None -> failwith "P256.sign: signing failure"

  let verify ~pk:(Pk pk) ~msg ~signature =
    if Bytes.length signature <> size then false
    else Hacl.P256.verify ~pk ~msg ~signature
end
