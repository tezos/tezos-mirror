open Bigarray
open Lib.Bigarray

module Chunk = struct
  type t = (int, int8_unsigned_elt, c_layout) Array1.t

  (** Number of bits in an address for the chunk offset *)
  let offset_bits = 12

  (** Size of a chunk in bytes - with 12 bits of address space the chunk is 4KiB *)
  let size = Int64.shift_left 1L offset_bits

  (** Get the chunk index for an address. *)
  let index address = Int64.shift_right address offset_bits

  (** Get the offset within its chunk for a given address. *)
  let offset address = Int64.(logand address (sub size 1L))

  let alloc () =
    let chunk = Array1_64.create Int8_unsigned C_layout size in
    Array1.fill chunk 0;
    chunk

  let of_bytes bytes =
    let chunk = alloc () in
    for i = 0 to Int.max (Int64.to_int size) (Bytes.length bytes) - 1 do
      Array1.set chunk i (Char.code (Bytes.get bytes i))
    done;
    chunk

  let num_needed length =
    if Int64.compare length 0L > 0 then
      (* [pred length] is used to cover the edge cases where [length] is an exact
          multiple of [Chunk.size]. For example [div Chunk.size Chunk.size] is 1
          but would allocate 2 chunks without a [pred] applied to the first
          argument. *)
      Int64.(div (pred length) size |> succ)
    else
      0L
end

module Effect = struct
  module type S = sig
    include Lazy_map.Effect.S

    val join : unit t list -> unit t
  end

  module Identity : S with type 'a t = 'a = struct
    include Lazy_map.Effect.Identity

    let join _ = ()
  end

  module Lwt : S with type 'a t = 'a Lwt.t = struct
    include Lazy_map.Effect.Lwt

    let join = Lwt.join
  end
end

module type S = sig
  type 'a effect

  type t

  val create : ?get_chunk:(int64 -> Chunk.t effect) -> int64 -> t

  val of_string : string -> t effect

  val of_bytes : bytes -> t effect

  val grow : t -> int64 -> unit

  val length : t -> int64

  val load_byte : t -> int64 -> int effect

  val store_byte : t -> int64 -> int -> unit effect

  val store_bytes : t -> int64 -> bytes -> unit effect
end

module Make (Effect : Effect.S) : S with type 'a effect = 'a Effect.t = struct
  module Map = Lazy_map.Mutable.Make (Effect) (Int64)

  type 'a effect = 'a Effect.t

  type t = { mutable length : int64; chunks : Chunk.t Map.t }

  let def_get_chunk _ = Effect.return (Chunk.alloc ())

  let create ?(get_chunk = def_get_chunk) length =
    let chunks =
      Map.create
        ~produce_value:get_chunk
        (Chunk.num_needed length)
    in
    { length; chunks }

  let grow vector size_delta =
    if Int64.compare size_delta 0L > 0 then
      let new_size = Int64.add vector.length size_delta in
      let new_chunks = Chunk.num_needed new_size in
      let current_chunks = Map.num_elements vector.chunks in
      let chunk_count_delta = Int64.sub new_chunks current_chunks in
      if Int64.compare chunk_count_delta 0L > 0 then
        Map.grow chunk_count_delta vector.chunks;
      vector.length <- new_size

  let length vector = vector.length

  let load_byte vector address =
    let open Effect in
    if Int64.compare address vector.length >= 0 then raise Memory_exn.Bounds;
    let+ chunk = Map.get (Chunk.index address) vector.chunks in
    Array1_64.get chunk (Chunk.offset address)

  let store_byte vector address byte =
    let open Effect in
    if Int64.compare address vector.length >= 0 then raise Memory_exn.Bounds;
    let+ chunk = Map.get (Chunk.index address) vector.chunks in
    Array1_64.set chunk (Chunk.offset address) byte

  let store_bytes vector address bytes =
    List.init (Bytes.length bytes) (fun i ->
      let c = Bytes.get bytes i in
      store_byte vector Int64.(of_int i |> add address) (Char.code c))
    |> Effect.join

  let of_string str =
    let open Effect in
    let vector = String.length str |> Int64.of_int |> create in
    let+ () =
      List.init (String.length str) (fun i ->
        let c = String.get str i in
        store_byte vector (Int64.of_int i) (Char.code c))
      |> join
    in
    vector

  let of_bytes bytes =
    let open Effect in
    let vector = Bytes.length bytes |> Int64.of_int |> create in
    let+ () = store_bytes vector 0L bytes in
    vector
end

include Make (Effect.Identity)

module Lwt = Make (Effect.Lwt)
