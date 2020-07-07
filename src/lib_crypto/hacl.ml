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
  let write buf =
    if Hacl.RandomBuffer.randombytes buf then ()
    else failwith "Error getting random bytes"

  let gen len =
    let buf = Bytes.create len in
    write buf ; buf
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

    let init () = EverCrypt.Hash.init S.alg

    let update st msg = EverCrypt.Hash.update st msg

    let finish st =
      let output = Bytes.create S.size in
      EverCrypt.Hash.finish st output ;
      output

    let digest msg =
      let output = Bytes.create S.size in
      EverCrypt.Hash.hash S.alg output msg ;
      output

    module HMAC = struct
      let digest ~key ~msg =
        let output = Bytes.create S.size in
        EverCrypt.HMAC.mac S.alg output key msg ;
        output
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

    let digest msg =
      let output = Bytes.create size in
      Hacl.SHA3_256.hash msg output ;
      output
  end

  module SHA3_512 = struct
    let size = 64

    let digest msg =
      let output = Bytes.create size in
      Hacl.SHA3_512.hash msg output ;
      output
  end

  module Keccak_256 = struct
    let size = 32

    let digest msg =
      let output = Bytes.create size in
      let keccak_256 = Hacl.Keccak.keccak 1088 512 1 in
      keccak_256 msg output ; output
  end
end

module Blake2b = struct
  type t = Bytes.t

  type hash = Hash of Bytes.t

  let direct ?(key = Bytes.create 0) inbuf len =
    if len < 1 || len > 64 then
      invalid_arg "Blake2b.direct: size must be between 1 and 64" ;
    let outbuf = Bytes.create len in
    (* HACL* doesn't yet provide a multiplexing interface for Blake2b so we
     * perform this check here and use the faster version if possible *)
    if AutoConfig2.(has_feature AVX2) then
      Hacl.Blake2b_256.hash key inbuf outbuf
    else Hacl.Blake2b_32.hash key inbuf outbuf ;
    Hash outbuf
end

module Nonce = struct
  type t = Bytes.t

  let size = 24

  let gen () = Rand.gen size

  let rec incr_byte b step byteno =
    let res = TzEndian.get_uint16 b byteno + step in
    let lo = res land 0xffff in
    let hi = res asr 16 in
    TzEndian.set_int16 b byteno lo ;
    if hi = 0 || byteno = 0 then () else incr_byte b hi (byteno - 2)

  let increment ?(step = 1) nonce =
    let new_nonce = Bytes.create 24 in
    Bytes.blit nonce 0 new_nonce 0 24 ;
    incr_byte new_nonce step 22 ;
    new_nonce

  let of_bytes buf = if Bytes.length buf <> size then None else Some buf

  let of_bytes_exn buf =
    match of_bytes buf with
    | Some s ->
        s
    | None ->
        invalid_arg "Hacl.Nonce.of_bytes_exn: invalid length"
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
    if Hacl.NaCl.Easy.secretbox cmsg msg nonce key then ()
    else failwith "Secretbox encryption failed"

  let secretbox_open ~key ~nonce ~cmsg ~msg =
    Hacl.NaCl.Easy.secretbox_open msg cmsg nonce key
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
    | Pk buf ->
        buf
    | Sk buf ->
        buf
    | Ck buf ->
        buf

  let blit_to_bytes : type a. a key -> ?pos:int -> Bytes.t -> unit =
   fun key ?(pos = 0) buf ->
    match key with
    | Pk pk ->
        Bytes.blit pk 0 buf pos pkbytes
    | Sk sk ->
        Bytes.blit sk 0 buf pos skbytes
    | Ck ck ->
        Bytes.blit ck 0 buf pos ckbytes

  let equal : type a. a key -> a key -> bool =
   fun a b ->
    (* TODO re-group once coverage ppx is updated *)
    match (a, b) with
    | (Pk a, Pk b) ->
        Bytes.equal a b
    | (Sk a, Sk b) ->
        Bytes.equal a b
    | (Ck a, Ck b) ->
        Bytes.equal a b

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
    let pk = Bytes.create pkbytes in
    EverCrypt.Curve25519.scalarmult pk sk basepoint ;
    Pk pk

  let keypair () =
    let sk = Sk (Rand.gen skbytes) in
    (neuterize sk, sk)

  let dh (Pk pk) (Sk sk) =
    let combined = Bytes.create ckbytes in
    if Hacl.NaCl.box_beforenm combined pk sk then Ck combined
    else failwith "Error computing box_beforenm"

  let box ~k:(Ck k) ~nonce ~msg ~cmsg =
    if not @@ Hacl.NaCl.Easy.box_afternm cmsg msg nonce k then
      failwith "Box: encryption error"

  let box_open ~k:(Ck k) ~nonce ~cmsg ~msg =
    Hacl.NaCl.Easy.box_open_afternm msg cmsg nonce k

  let box_noalloc ~k:(Ck k) ~nonce ~tag ~buf =
    if not @@ Hacl.NaCl.Detached.box_afternm buf tag buf nonce k then
      failwith "Box: encryption error"

  let box_open_noalloc ~k:(Ck k) ~nonce ~tag ~buf =
    Hacl.NaCl.Detached.box_open_afternm buf buf tag nonce k
end

module Sign = struct
  type _ key = Sk : Bytes.t -> secret key | Pk : Bytes.t -> public key

  let size = 64

  let pkbytes = 32

  let skbytes = 32

  let unsafe_to_bytes : type a. a key -> Bytes.t = function
    | Pk buf ->
        buf
    | Sk buf ->
        buf

  let unsafe_sk_of_bytes seed =
    if Bytes.length seed <> skbytes then
      invalid_arg
        (Printf.sprintf
           "Sign.unsafe_sk_of_bytes: sk must be at least %d bytes long"
           skbytes) ;
    Sk seed

  let unsafe_pk_of_bytes pk =
    if Bytes.length pk <> pkbytes then
      invalid_arg
        (Printf.sprintf
           "Sign.unsafe_pk_of_bytes: pk must be at least %d bytes long"
           pkbytes) ;
    Pk pk

  let blit_to_bytes : type a. a key -> ?pos:int -> Bytes.t -> unit =
   fun key ?(pos = 0) buf ->
    match key with
    | Pk pk ->
        Bytes.blit pk 0 buf pos pkbytes
    | Sk sk ->
        Bytes.blit sk 0 buf pos skbytes

  let equal : type a. a key -> a key -> bool =
   fun a b ->
    (* TODO re-group once coverage ppx is updated *)
    match (a, b) with
    | (Pk a, Pk b) ->
        Bytes.equal a b
    | (Sk a, Sk b) ->
        Bytes.equal a b

  let neuterize : type a. a key -> public key = function
    | Pk pk ->
        Pk pk
    | Sk sk ->
        let pk = Bytes.create pkbytes in
        Hacl.Ed25519.secret_to_public pk sk ;
        Pk pk

  let keypair () =
    let sk = Sk (Rand.gen skbytes) in
    (neuterize sk, sk)

  let sign ~sk:(Sk sk) ~msg ~signature =
    if Bytes.length signature < size then
      invalid_arg
        (Printf.sprintf
           "Sign.write_sign: output buffer must be at least %d bytes long"
           size) ;
    Hacl.Ed25519.sign signature sk msg

  let verify ~pk:(Pk pk) ~msg ~signature = Hacl.Ed25519.verify pk msg signature
end

module ECDSA = struct
  let sk_size = 32

  let pk_size = 64

  let compressed_size = (pk_size / 2) + 1

  let uncompressed_size = pk_size + 1

  let signature_size = 64

  let pk_of_sk sk pk = Hacl.P256.dh_initiator pk sk

  let valid_pk pk = Hacl.P256.valid_pk pk

  (* Generate a random sk_size buffer until it is valid to be used as
   * secret key, i.e. non-zero and smaller than the prime order.
   * This is also used to generate signing secrets. *)
  let rec get_valid_sk () =
    let sk = Rand.gen sk_size in
    if Hacl.P256.valid_sk sk then sk else get_valid_sk ()

  let compressed_from_raw pk cpk = Hacl.P256.compress_c pk cpk

  let uncompressed_from_raw pk cpk = Hacl.P256.compress_n pk cpk

  let raw_from_compressed buf pk =
    if not (Hacl.P256.decompress_c buf pk) then
      failwith "P256.raw_from_compressed: failure"

  let raw_from_uncompressed buf pk =
    if not (Hacl.P256.decompress_n buf pk) then
      failwith "P256.raw_from_uncompressed failure"

  type _ key = Sk : Bytes.t -> secret key | Pk : Bytes.t -> public key

  let equal : type a. a key -> a key -> bool =
   fun k1 k2 ->
    match (k1, k2) with
    | (Sk sk, Sk sk2) ->
        Bytes.equal sk sk2
    | (Pk pk, Pk pk2) ->
        Bytes.equal pk pk2

  let neuterize : type a. a key -> public key = function
    | Pk pk ->
        Pk pk
    | Sk sk ->
        let pk = Bytes.create pk_size in
        let pk_computed_ok = pk_of_sk sk pk in
        let pk_is_valid = valid_pk pk in
        if (not pk_computed_ok) && pk_is_valid then
          invalid_arg "Uecc.neuterize" ;
        Pk pk

  let pk_of_bytes : Bytes.t -> public key option =
   fun buf ->
    let pk = Bytes.create pk_size in
    match Bytes.length buf with
    | len when len = compressed_size ->
        raw_from_compressed buf pk ;
        if valid_pk pk then Some (Pk pk) else None
    | len when len = uncompressed_size ->
        raw_from_uncompressed buf pk ;
        if valid_pk pk then Some (Pk pk) else None
    | _ ->
        None

  let sk_of_bytes : Bytes.t -> (secret key * public key) option =
   fun buf ->
    if not (Bytes.length buf = sk_size && Hacl.P256.valid_sk buf) then None
    else
      let sk = Sk (Bytes.copy buf) in
      try
        let pk = neuterize sk in
        Some (sk, pk)
      with _ -> None

  let to_bytes : type a. ?compress:bool -> a key -> Bytes.t =
   fun ?compress:(comp = true) -> function
    | Sk sk ->
        Bytes.copy sk
    | Pk pk ->
        if comp then (
          let buf = Bytes.create compressed_size in
          compressed_from_raw pk buf ; buf )
        else
          let buf = Bytes.create uncompressed_size in
          uncompressed_from_raw pk buf ;
          assert (Bytes.sub buf 1 pk_size = pk) ;
          buf

  let write_key : type a. ?compress:bool -> Bytes.t -> a key -> int =
   fun ?compress:(comp = true) buf -> function
    | Sk sk ->
        let len = Bytes.length sk in
        Bytes.blit sk 0 buf 0 len ; len
    | Pk pk ->
        if comp then (compressed_from_raw pk buf ; compressed_size)
        else (
          uncompressed_from_raw pk buf ;
          uncompressed_size )

  let keypair () : (secret key * public key) option =
    let pk = Bytes.create pk_size in
    let sk = get_valid_sk () in
    if Hacl.P256.dh_initiator pk sk then Some (Sk sk, Pk pk) else None

  let write_sign (Sk sk) buf ~msg =
    assert (Bytes.length buf = signature_size) ;
    (* A random non-zero signing secret k is generated which, similar to
     * secret keys, needs to be non-zero and smaller than the prime order. *)
    let k = get_valid_sk () in
    let res = Hacl.P256.sign sk msg k buf in
    Bytes.fill k 0 32 '\x00' ; res

  let sign (Sk sk) ~msg =
    let signature = Bytes.create 64 in
    if write_sign (Sk sk) signature ~msg then Some signature else None

  let verify (Pk pk) ~msg ~signature =
    if Bytes.length signature <> signature_size then false
    else Hacl.P256.verify pk msg signature
end
