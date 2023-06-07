type bigstring =
  ( char,
    Bigarray_compat.int8_unsigned_elt,
    Bigarray_compat.c_layout )
  Bigarray_compat.Array1.t

type 'a iter = ('a -> unit) -> unit

type 'a compare = 'a -> 'a -> int

type 'a equal = 'a -> 'a -> bool

type 'a pp = Format.formatter -> 'a -> unit

module By = Digestif_by
module Bi = Digestif_bi
module Eq = Digestif_eq
module Conv = Digestif_conv

let failwith fmt = Format.ksprintf failwith fmt

module type S = sig
  val digest_size : int

  type ctx

  type t

  val empty : ctx

  val init : ?personalisation:Bytes.t -> unit -> ctx

  val feed_bytes : ctx -> ?off:int -> ?len:int -> Bytes.t -> ctx

  val feed_string : ctx -> ?off:int -> ?len:int -> String.t -> ctx

  val feed_bigstring : ctx -> ?off:int -> ?len:int -> bigstring -> ctx

  val feedi_bytes : ctx -> Bytes.t iter -> ctx

  val feedi_string : ctx -> String.t iter -> ctx

  val feedi_bigstring : ctx -> bigstring iter -> ctx

  val get : ctx -> t

  val digest_bytes : ?off:int -> ?len:int -> Bytes.t -> t

  val digest_string : ?off:int -> ?len:int -> String.t -> t

  val digest_bigstring : ?off:int -> ?len:int -> bigstring -> t

  val digesti_bytes : Bytes.t iter -> t

  val digesti_string : String.t iter -> t

  val digesti_bigstring : bigstring iter -> t

  val digestv_bytes : Bytes.t list -> t

  val digestv_string : String.t list -> t

  val digestv_bigstring : bigstring list -> t

  val unsafe_compare : t compare

  val equal : t equal

  val pp : t pp

  val of_hex : string -> t

  val of_hex_opt : string -> t option

  val consistent_of_hex : string -> t

  val consistent_of_hex_opt : string -> t option

  val to_hex : t -> string

  val of_raw_string : string -> t

  val of_raw_string_opt : string -> t option

  val to_raw_string : t -> string
end

module type Desc = sig
  val digest_size : int

  val block_size : int
end

module type Hash = sig
  type ctx

  val init : ?personalisation:Bytes.t -> unit -> ctx

  val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit

  val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit

  val unsafe_get : ctx -> By.t

  val dup : ctx -> ctx
end

module Unsafe (Hash : Hash) (D : Desc) = struct
  open Hash

  let digest_size = D.digest_size

  let empty = init ()

  let init = init

  let unsafe_feed_bytes ctx ?off ?len buf =
    let off, len =
      match (off, len) with
      | Some off, Some len -> (off, len)
      | Some off, None -> (off, By.length buf - off)
      | None, Some len -> (0, len)
      | None, None -> (0, By.length buf)
    in
    if off < 0 || len < 0 || off > By.length buf - len then
      invalid_arg "offset out of bounds"
    else unsafe_feed_bytes ctx buf off len

  let unsafe_feed_string ctx ?off ?len buf =
    unsafe_feed_bytes ctx ?off ?len (By.unsafe_of_string buf)

  let unsafe_feed_bigstring ctx ?off ?len buf =
    let off, len =
      match (off, len) with
      | Some off, Some len -> (off, len)
      | Some off, None -> (off, Bi.length buf - off)
      | None, Some len -> (0, len)
      | None, None -> (0, Bi.length buf)
    in
    if off < 0 || len < 0 || off > Bi.length buf - len then
      invalid_arg "offset out of bounds"
    else unsafe_feed_bigstring ctx buf off len

  let unsafe_get = unsafe_get
end

module Core (Hash : Hash) (D : Desc) = struct
  type t = string

  type ctx = Hash.ctx

  include Unsafe (Hash) (D)
  include Conv.Make (D)
  include Eq.Make (D)

  let get t =
    let t = Hash.dup t in
    unsafe_get t |> By.unsafe_to_string

  let feed_bytes t ?off ?len buf =
    let t = Hash.dup t in
    unsafe_feed_bytes t ?off ?len buf ;
    t

  let feed_string t ?off ?len buf =
    let t = Hash.dup t in
    unsafe_feed_string t ?off ?len buf ;
    t

  let feed_bigstring t ?off ?len buf =
    let t = Hash.dup t in
    unsafe_feed_bigstring t ?off ?len buf ;
    t

  let feedi_bytes t iter =
    let t = Hash.dup t in
    let feed buf = unsafe_feed_bytes t buf in
    iter feed ;
    t

  let feedi_string t iter =
    let t = Hash.dup t in
    let feed buf = unsafe_feed_string t buf in
    iter feed ;
    t

  let feedi_bigstring t iter =
    let t = Hash.dup t in
    let feed buf = unsafe_feed_bigstring t buf in
    iter feed ;
    t

  let digest_bytes ?off ?len buf = feed_bytes empty ?off ?len buf |> get

  let digest_string ?off ?len buf = feed_string empty ?off ?len buf |> get

  let digest_bigstring ?off ?len buf = feed_bigstring empty ?off ?len buf |> get

  let digesti_bytes iter = feedi_bytes empty iter |> get

  let digesti_string iter = feedi_string empty iter |> get

  let digesti_bigstring iter = feedi_bigstring empty iter |> get

  let digestv_bytes lst = digesti_bytes (fun f -> List.iter f lst)

  let digestv_string lst = digesti_string (fun f -> List.iter f lst)

  let digestv_bigstring lst = digesti_bigstring (fun f -> List.iter f lst)
end

module Make (H : Hash) (D : Desc) = struct
  include Core (H) (D)
end

module type Hash_BLAKE2 = sig
  type ctx

  val with_outlen_and_bytes_key :
    ?personalisation:Bytes.t -> int -> By.t -> int -> int -> ctx

  val unsafe_feed_bytes : ctx -> By.t -> int -> int -> unit

  val unsafe_feed_bigstring : ctx -> Bi.t -> int -> int -> unit

  val unsafe_get : ctx -> By.t

  val dup : ctx -> ctx

  val max_outlen : int
end

module Make_BLAKE2 (H : Hash_BLAKE2) (D : Desc) = struct
  let () =
    if D.digest_size > H.max_outlen then
      failwith
        "Invalid digest_size:%d to make a BLAKE2{S,B} implementation"
        D.digest_size

  include
    Make
      (struct
        type ctx = H.ctx

        let init ?personalisation () =
          H.with_outlen_and_bytes_key
          (* XXX(dannywillems): adding personalisation *)
            ?personalisation
            D.digest_size
            By.empty
            0
            0

        let unsafe_feed_bytes = H.unsafe_feed_bytes

        let unsafe_feed_bigstring = H.unsafe_feed_bigstring

        let unsafe_get = H.unsafe_get

        let dup = H.dup
      end)
      (D)
end

module BLAKE2B : sig
  include S
end =
  Make_BLAKE2
    (Baijiu_blake2b.Unsafe)
    (struct
      let digest_size, block_size = (64, 128)
    end)

module BLAKE2S : sig
  include S
end =
  Make_BLAKE2
    (Baijiu_blake2s.Unsafe)
    (struct
      let digest_size, block_size = (32, 64)
    end)

module Make_BLAKE2B (D : sig
  val digest_size : int
end) : S = struct
  include
    Make_BLAKE2
      (Baijiu_blake2b.Unsafe)
      (struct
        let digest_size, block_size = (D.digest_size, 128)
      end)
end

module Make_BLAKE2S (D : sig
  val digest_size : int
end) : S = struct
  include
    Make_BLAKE2
      (Baijiu_blake2s.Unsafe)
      (struct
        let digest_size, block_size = (D.digest_size, 64)
      end)
end

type 'k hash = BLAKE2B : BLAKE2B.t hash | BLAKE2S : BLAKE2S.t hash

let blake2b = BLAKE2B

let blake2s = BLAKE2S

let module_of : type k. k hash -> (module S with type t = k) = function
  | BLAKE2B -> (module BLAKE2B)
  | BLAKE2S -> (module BLAKE2S)

type 'hash t = 'hash

let digest_bytes : type k. k hash -> Bytes.t -> k t =
 fun hash buf ->
  let module H = (val module_of hash) in
  H.digest_bytes buf

let digest_string : type k. k hash -> String.t -> k t =
 fun hash buf ->
  let module H = (val module_of hash) in
  H.digest_string buf

let digest_bigstring : type k. k hash -> bigstring -> k t =
 fun hash buf ->
  let module H = (val module_of hash) in
  H.digest_bigstring buf

let digesti_bytes : type k. k hash -> Bytes.t iter -> k t =
 fun hash iter ->
  let module H = (val module_of hash) in
  H.digesti_bytes iter

let digesti_string : type k. k hash -> String.t iter -> k t =
 fun hash iter ->
  let module H = (val module_of hash) in
  H.digesti_string iter

let digesti_bigstring : type k. k hash -> bigstring iter -> k t =
 fun hash iter ->
  let module H = (val module_of hash) in
  H.digesti_bigstring iter

(* XXX(dinosaure): unsafe part to avoid overhead. *)

let unsafe_compare : type k. k hash -> k t -> k t -> int =
 fun hash a b ->
  let module H = (val module_of hash) in
  H.unsafe_compare a b

let equal : type k. k hash -> k t equal =
 fun hash a b ->
  let module H = (val module_of hash) in
  H.equal a b

let pp : type k. k hash -> k t pp =
 fun hash ppf t ->
  let module H = (val module_of hash) in
  H.pp ppf t

let of_hex : type k. k hash -> string -> k t =
 fun hash hex ->
  let module H = (val module_of hash) in
  H.of_hex hex

let of_hex_opt : type k. k hash -> string -> k t option =
 fun hash hex ->
  let module H = (val module_of hash) in
  H.of_hex_opt hex

let consistent_of_hex : type k. k hash -> string -> k t =
 fun hash hex ->
  let module H = (val module_of hash) in
  H.consistent_of_hex hex

let consistent_of_hex_opt : type k. k hash -> string -> k t option =
 fun hash hex ->
  let module H = (val module_of hash) in
  H.consistent_of_hex_opt hex

let to_hex : type k. k hash -> k t -> string =
 fun hash t ->
  let module H = (val module_of hash) in
  H.to_hex t

let of_raw_string : type k. k hash -> string -> k t =
 fun hash s ->
  let module H = (val module_of hash) in
  H.of_raw_string s

let of_raw_string_opt : type k. k hash -> string -> k t option =
 fun hash s ->
  let module H = (val module_of hash) in
  H.of_raw_string_opt s

let to_raw_string : type k. k hash -> k t -> string =
 fun hash t ->
  let module H = (val module_of hash) in
  H.to_raw_string t

let of_digest (type hash) (module H : S with type t = hash) (hash : H.t) :
    hash t =
  hash

let of_blake2b hash = hash

let of_blake2s hash = hash
