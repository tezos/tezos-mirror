(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

type error += Cannot_encode_block of L2block.hash

let () =
  register_error_kind
    ~id:"tx_rollup.node.cannot_encode_block"
    ~title:"An L2 block cannot be encoded"
    ~description:"An L2 block cannot be encoded to be stored on disk."
    ~pp:(fun ppf b ->
      Format.fprintf
        ppf
        "The L2 block %a cannot be encoded to be stored on disk."
        L2block.Hash.pp
        b)
    `Permanent
    Data_encoding.(obj1 (req "block" L2block.Hash.encoding))
    (function Cannot_encode_block b -> Some b | _ -> None)
    (fun b -> Cannot_encode_block b)

type error += Cannot_encode_data of string

let () =
  register_error_kind
    ~id:"tx_rollup.node.cannot_encode_data"
    ~title:"Data cannot be encoded"
    ~description:"Data cannot be encoded to be stored on disk."
    ~pp:(fun ppf name ->
      Format.fprintf ppf "Data %s cannot be encoded to be stored on disk." name)
    `Permanent
    Data_encoding.(obj1 (req "name" string))
    (function Cannot_encode_data n -> Some n | _ -> None)
    (fun n -> Cannot_encode_data n)

type error += Cannot_write_file of string

let () =
  register_error_kind
    ~id:"tx_rollup.node.cannot_write_file"
    ~title:"File cannot be written"
    ~description:"File cannot be written to disk."
    ~pp:(fun ppf name ->
      Format.fprintf ppf "File %s cannot be written to disk." name)
    `Permanent
    Data_encoding.(obj1 (req "name" string))
    (function Cannot_write_file n -> Some n | _ -> None)
    (fun n -> Cannot_write_file n)

(* Helper functions to copy byte sequences or integers in [src] to another byte
   sequence [dst] at offset [offset], with named arguments to avoid
   confusion. These functions return the offset in the destination at which to
   copy the more data. *)

let blit ~src ~dst offset =
  let len = Bytes.length src in
  Bytes.blit src 0 dst offset len ;
  offset + len

let bytes_set_int64 ~src ~dst offset =
  Bytes.set_int64_be dst offset src ;
  offset + 8

let bytes_set_int32 ~src ~dst offset =
  Bytes.set_int32_be dst offset src ;
  offset + 4

let bytes_set_int8 ~src ~dst offset =
  Bytes.set_int8 dst offset src ;
  offset + 1

(* Helper functions to read data (strings with a decoding function, or integers)
   from a binary string. These functions return, as the second component, the
   offset in the string at which to read more data. *)

let read_str str ~offset ~len decode =
  let s = String.sub str offset len in
  (decode s, offset + len)

let read_int64 str offset =
  let i = TzEndian.get_int64_string str offset in
  (i, offset + 8)

let read_int32 str offset =
  let i = TzEndian.get_int32_string str offset in
  (i, offset + 4)

let read_int8 str offset =
  let i = TzEndian.get_int8_string str offset in
  (i, offset + 1)

(* Functors to build stores on indexes *)

module type SINGLETON_STORE = sig
  type t

  type value

  val read : t -> value option Lwt.t

  val write : t -> value -> unit tzresult Lwt.t

  val delete : t -> unit Lwt.t
end

module type INDEXABLE_STORE = sig
  type t

  type key

  type value

  val mem : t -> key -> bool Lwt.t

  val find : t -> key -> value option Lwt.t

  val add : ?flush:bool -> t -> key -> value -> unit Lwt.t
end

module type INDEXABLE_REMOVABLE_STORE = sig
  include INDEXABLE_STORE

  val remove : ?flush:bool -> t -> key -> unit Lwt.t
end

module Make_indexable
    (K : Index.Key.S)
    (V : Index.Value.S) (P : sig
      val path : data_dir:string -> string
    end) =
struct
  module I = Index_unix.Make (K) (V) (Index.Cache.Unbounded)

  type t = {index : I.t; scheduler : Lwt_idle_waiter.t}

  let log_size = 10_000

  let mem store k =
    Lwt_idle_waiter.task store.scheduler @@ fun () ->
    Lwt.return (I.mem store.index k)

  let find store k =
    let open Lwt_syntax in
    Lwt_idle_waiter.task store.scheduler @@ fun () ->
    Option.catch_os @@ fun () ->
    let v = I.find store.index k in
    return_some v

  let add ?(flush = true) store k v =
    Lwt_idle_waiter.force_idle store.scheduler @@ fun () ->
    I.replace store.index k v ;
    if flush then I.flush store.index ;
    Lwt.return_unit

  let init ~data_dir ~readonly =
    let index = I.v ~log_size ~readonly (P.path ~data_dir) in
    let scheduler = Lwt_idle_waiter.create () in
    Lwt.return {index; scheduler}

  let close store =
    Lwt_idle_waiter.force_idle store.scheduler @@ fun () ->
    (try I.close store.index with Index.Closed -> ()) ;
    Lwt.return_unit
end

module Make_indexable_removable
    (K : Index.Key.S)
    (V : Index.Value.S) (P : sig
      val path : data_dir:string -> string
    end) =
struct
  module V_opt = struct
    (* The values stored in the index are optional values.  When we "remove" a
       key from the store, we're not really removing it from the index, but
       simply setting its association to [None] (encoded with zero bytes here).
    *)

    type t = V.t option

    let t = Repr.option V.t

    let encoded_size = 1 + V.encoded_size

    let encode v =
      let dst = Bytes.create encoded_size in
      let (tag, value_bytes) =
        match v with
        | None -> (0, Bytes.make V.encoded_size '\000')
        | Some v -> (1, V.encode v |> Bytes.unsafe_of_string)
      in
      let offset = bytes_set_int8 ~dst ~src:tag 0 in
      let _ = blit ~src:value_bytes ~dst offset in
      Bytes.unsafe_to_string dst

    let decode str offset =
      let (tag, offset) = read_int8 str offset in
      match tag with
      | 0 -> None
      | 1 ->
          let value = V.decode str offset in
          Some value
      | _ -> assert false
  end

  include Make_indexable (K) (V_opt) (P)

  let find store k =
    let open Lwt_syntax in
    let+ v = find store k in
    match v with None | Some None -> None | Some (Some v) -> Some v

  let mem store hash =
    let open Lwt_syntax in
    let+ b = find store hash in
    Option.is_some b

  let add ?flush store k v = add ?flush store k (Some v)

  let remove ?(flush = true) store k =
    Lwt_idle_waiter.force_idle store.scheduler @@ fun () ->
    let exists = I.mem store.index k in
    if not exists then Lwt.return_unit
    else (
      I.replace store.index k None ;
      if flush then I.flush store.index ;
      Lwt.return_unit)
end

module Make_singleton (S : sig
  type t

  val name : string

  val encoding : t Data_encoding.t
end) =
struct
  type t = {file : string}

  let read store =
    let open Lwt_syntax in
    let* exists = Lwt_unix.file_exists store.file in
    match exists with
    | false -> return_none
    | true ->
        Lwt_io.with_file
          ~flags:[Unix.O_RDONLY; O_CLOEXEC]
          ~mode:Input
          store.file
        @@ fun channel ->
        let+ bytes = Lwt_io.read channel in
        Data_encoding.Binary.of_bytes_opt
          S.encoding
          (Bytes.unsafe_of_string bytes)

  let write store x =
    let open Lwt_result_syntax in
    let*! res =
      Lwt_utils_unix.with_atomic_open_out ~overwrite:true store.file
      @@ fun fd ->
      let* block_bytes =
        match Data_encoding.Binary.to_bytes_opt S.encoding x with
        | None -> tzfail (Cannot_encode_data S.name)
        | Some bytes -> return bytes
      in
      let*! () = Lwt_utils_unix.write_bytes fd block_bytes in
      return_unit
    in
    match res with
    | Ok res -> Lwt.return res
    | Error _ -> tzfail (Cannot_write_file S.name)

  let delete store =
    let open Lwt_syntax in
    let* exists = Lwt_unix.file_exists store.file in
    match exists with
    | false -> return_unit
    | true -> Lwt_unix.unlink store.file

  let init ~data_dir =
    let file = Filename.Infix.(Node_data.store_dir data_dir // S.name) in
    Lwt.return {file}
end

module L2_block_key = struct
  include L2block.Hash

  (* [hash] in Blake2B.Make is {!Stdlib.Hashtbl.hash} which is 30 bits *)
  let hash_size = 30 (* in bits *)

  let t =
    let open Repr in
    map
      (bytes_of (`Fixed hash_size))
      (fun b -> of_bytes_exn b)
      (fun bh -> to_bytes bh)

  let encode bh = to_string bh

  let encoded_size = size (* in bytes *)

  let decode str off =
    let str = String.sub str off encoded_size in
    of_string_exn str
end

module L2_level_key = struct
  type t = L2block.level

  let to_int32 = Protocol.Alpha_context.Tx_rollup_level.to_int32

  let of_int32 l =
    WithExceptions.Result.get_ok ~loc:__LOC__
    @@ Protocol.Alpha_context.Tx_rollup_level.of_int32 l

  let t =
    let open Repr in
    map int32 of_int32 to_int32

  let equal x y = Compare.Int32.equal (to_int32 x) (to_int32 y)

  let hash = Stdlib.Hashtbl.hash

  (* {!Stdlib.Hashtbl.hash} is 30 bits as per {!Index__.Data.Key} *)
  let hash_size = 30 (* in bits *)

  let encoded_size = 4 (* in bytes *)

  let encode l =
    let b = Bytes.create encoded_size in
    TzEndian.set_int32 b 0 (to_int32 l) ;
    Bytes.unsafe_to_string b

  let decode str i = TzEndian.get_int32_string str i |> of_int32
end

module Operation_key = struct
  include Operation_hash

  (* [hash] in Blake2B.Make is {!Stdlib.Hashtbl.hash} which is 30 bits *)
  let hash_size = 30 (* in bits *)

  let t =
    let open Repr in
    map
      (bytes_of (`Fixed hash_size))
      (fun b -> of_bytes_exn b)
      (fun bh -> to_bytes bh)
end

module Commitment_key = struct
  include Protocol.Alpha_context.Tx_rollup_commitment_hash

  let hash = Stdlib.Hashtbl.hash

  (* {!Stdlib.Hashtbl.hash} is 30 bits *)
  let hash_size = 30 (* in bits *)

  let t =
    let open Repr in
    map
      (bytes_of (`Fixed hash_size))
      (fun b -> of_bytes_exn b)
      (fun bh -> to_bytes bh)

  let encode bh = Bytes.unsafe_to_string (to_bytes bh)

  let encoded_size = size (* in bytes *)

  let decode str off =
    let str = String.sub str off encoded_size in
    of_bytes_exn (Bytes.unsafe_of_string str)
end

module L2_block_info = struct
  type t = {
    offset : int;
    (* subset of L2 block header for efficiency *)
    predecessor : L2block.hash option;
    context : Protocol.Tx_rollup_l2_context_hash.t;
  }

  let encoded_size =
    8 (* offset *) + L2block.Hash.size + Protocol.Tx_rollup_l2_context_hash.size

  let l2_context_hash_repr =
    let open Repr in
    map
      (bytes_of (`Fixed 31))
      (fun c -> Protocol.Tx_rollup_l2_context_hash.of_bytes_exn c)
      (fun ch -> Protocol.Tx_rollup_l2_context_hash.to_bytes ch)

  let t =
    let open Repr in
    map
      (triple int (option L2_block_key.t) l2_context_hash_repr)
      (fun (offset, predecessor, context) -> {offset; predecessor; context})
      (fun {offset; predecessor; context} -> (offset, predecessor, context))

  let encode v =
    let dst = Bytes.create encoded_size in
    let offset = bytes_set_int64 ~src:(Int64.of_int v.offset) ~dst 0 in
    let pred_bytes =
      match v.predecessor with
      | None -> L2block.Hash.(to_bytes zero)
      | Some b -> L2block.Hash.to_bytes b
    in
    let offset = blit ~src:pred_bytes ~dst offset in
    let _ =
      blit
        ~src:(Protocol.Tx_rollup_l2_context_hash.to_bytes v.context)
        ~dst
        offset
    in
    Bytes.unsafe_to_string dst

  let decode str offset =
    let (file_offset, offset) = read_int64 str offset in
    let (predecessor, offset) =
      read_str str ~offset ~len:L2block.Hash.size L2block.Hash.of_string_exn
    in
    let predecessor =
      if L2block.Hash.(predecessor = zero) then None else Some predecessor
    in
    let (context, _) =
      read_str
        str
        ~offset
        ~len:Protocol.Tx_rollup_l2_context_hash.size
        (fun s ->
          Bytes.unsafe_of_string s
          |> Protocol.Tx_rollup_l2_context_hash.of_bytes_exn)
    in
    {offset = Int64.to_int file_offset; predecessor; context}
end

module Tezos_block_info = struct
  type t = {l2_block : L2block.hash; level : int32; predecessor : Block_hash.t}

  let t =
    let open Repr in
    map
      (triple L2_block_key.t int32 Tezos_store.Block_key.t)
      (fun (l2_block, level, predecessor) -> {l2_block; level; predecessor})
      (fun {l2_block; level; predecessor} -> (l2_block, level, predecessor))

  let encoded_size = L2block.Hash.size + 4 (* level *) + Block_hash.size

  let encode v =
    let dst = Bytes.create encoded_size in
    let offset = blit ~src:(L2block.Hash.to_bytes v.l2_block) ~dst 0 in
    let offset = bytes_set_int32 ~dst ~src:v.level offset in
    let _ = blit ~src:(Block_hash.to_bytes v.predecessor) ~dst offset in
    Bytes.unsafe_to_string dst

  let decode str offset =
    let (l2_block, offset) =
      read_str str ~offset ~len:L2block.Hash.size L2block.Hash.of_string_exn
    in
    let (level, offset) = read_int32 str offset in
    let (predecessor, _) =
      read_str str ~offset ~len:Block_hash.size Block_hash.of_string_exn
    in
    {l2_block; level; predecessor}
end

module Commitment_info = struct
  type t = {block : Block_hash.t; operation : Operation_hash.t}

  let t =
    let open Repr in
    map
      (pair Tezos_store.Block_key.t Operation_key.t)
      (fun (block, operation) -> {block; operation})
      (fun {block; operation} -> (block, operation))

  let encoded_size = Block_hash.size + Operation_hash.size

  let encode v =
    let dst = Bytes.create encoded_size in
    let offset = blit ~src:(Block_hash.to_bytes v.block) ~dst 0 in
    let _ = blit ~src:(Operation_hash.to_bytes v.operation) ~dst offset in
    Bytes.unsafe_to_string dst

  let decode str offset =
    let (block, offset) =
      read_str str ~offset ~len:Block_hash.size Block_hash.of_string_exn
    in
    let (operation, _) =
      read_str str ~offset ~len:Operation_hash.size Operation_hash.of_string_exn
    in
    {block; operation}
end

module Tezos_block_store = struct
  type value = Tezos_block_info.t = {
    l2_block : L2block.hash;
    level : int32;
    predecessor : Block_hash.t;
  }

  include
    Make_indexable (Tezos_store.Block_key) (Tezos_block_info)
      (struct
        let path ~data_dir = Node_data.tezos_blocks_index data_dir
      end)
end

module Level_store =
  Make_indexable_removable (L2_level_key) (L2_block_key)
    (struct
      let path ~data_dir = Node_data.levels_index data_dir
    end)

module Commitment_store = struct
  type value = Commitment_info.t = {
    block : Block_hash.t;
    operation : Operation_hash.t;
  }

  include
    Make_indexable_removable (Commitment_key) (Commitment_info)
      (struct
        let path ~data_dir = Node_data.commitments_index data_dir
      end)
end

module L2_block_store = struct
  open L2_block_info

  module Cache =
    Ringo_lwt.Functors.Make_opt
      ((val Ringo.(
              map_maker ~replacement:LRU ~overflow:Strong ~accounting:Precise))
         (L2block.Hash))

  module L2_block_index =
    Index_unix.Make (L2_block_key) (L2_block_info) (Index.Cache.Unbounded)

  module L2_blocks_file = struct
    let encoding = Data_encoding.dynamic_size ~kind:`Uint30 L2block.encoding

    let pread_block_exn fd ~file_offset =
      let open Lwt_syntax in
      (* Read length *)
      let length_bytes = Bytes.create 4 in
      let* () =
        Lwt_utils_unix.read_bytes ~file_offset ~pos:0 ~len:4 fd length_bytes
      in
      let block_length_int32 = Bytes.get_int32_be length_bytes 0 in
      let block_length = Int32.to_int block_length_int32 in
      let block_bytes = Bytes.extend length_bytes 0 block_length in
      let* () =
        Lwt_utils_unix.read_bytes
          ~file_offset:(file_offset + 4)
          ~pos:4
          ~len:block_length
          fd
          block_bytes
      in
      Lwt.return
        ( Data_encoding.Binary.of_bytes_exn encoding block_bytes,
          4 + block_length )

    let pread_block fd ~file_offset =
      Option.catch_s (fun () -> pread_block_exn fd ~file_offset)
  end

  type t = {
    index : L2_block_index.t;
    fd : Lwt_unix.file_descr;
    scheduler : Lwt_idle_waiter.t;
    cache : L2block.t Cache.t;
  }

  (* The log_size corresponds to the maximum size of the memory zone
     allocated in memory before flushing it onto the disk. It is
     basically a cache which is use for the index. The cache size is
     `log_size * log_entry` where a `log_entry` is roughly 56 bytes. *)
  let blocks_log_size = 10_000

  let mem store hash =
    Lwt_idle_waiter.task store.scheduler @@ fun () ->
    Lwt.return (L2_block_index.mem store.index hash)

  let predecessor store hash =
    Lwt_idle_waiter.task store.scheduler @@ fun () ->
    try
      let {predecessor; _} = L2_block_index.find store.index hash in
      Lwt.return predecessor
    with Not_found -> Lwt.return_none

  let context store hash =
    Lwt_idle_waiter.task store.scheduler @@ fun () ->
    try
      let {context; _} = L2_block_index.find store.index hash in
      Lwt.return_some context
    with Not_found -> Lwt.return_none

  let read_block store hash =
    let open Lwt_syntax in
    Lwt_idle_waiter.task store.scheduler @@ fun () ->
    Option.catch_os @@ fun () ->
    let read_from_disk hash =
      let {offset; _} = L2_block_index.find store.index hash in
      let* o = L2_blocks_file.pread_block store.fd ~file_offset:offset in
      match o with
      | Some (block, _) -> Lwt.return_some block
      | None -> Lwt.return_none
    in
    Cache.find_or_replace store.cache hash read_from_disk

  let locked_write_block store ~offset ~block ~hash =
    let open Lwt_result_syntax in
    let* block_bytes =
      match Data_encoding.Binary.to_bytes_opt L2_blocks_file.encoding block with
      | None -> tzfail (Cannot_encode_block hash)
      | Some bytes -> return bytes
    in
    let block_length = Bytes.length block_bytes in
    let*! () =
      Lwt_utils_unix.write_bytes ~pos:0 ~len:block_length store.fd block_bytes
    in
    L2_block_index.replace
      store.index
      hash
      {
        offset;
        predecessor = block.header.predecessor;
        context = block.header.context;
      } ;
    return block_length

  let append_block ?(flush = true) store (block : L2block.t) =
    let open Lwt_syntax in
    Lwt_idle_waiter.force_idle store.scheduler @@ fun () ->
    let hash = block.hash in
    Cache.replace store.cache hash (return_some block) ;
    let* offset = Lwt_unix.lseek store.fd 0 Unix.SEEK_END in
    let* _written_len = locked_write_block store ~offset ~block ~hash in
    if flush then L2_block_index.flush store.index ;
    Lwt.return_unit

  let init ~data_dir ~readonly ~cache_size =
    let open Lwt_syntax in
    let (flag, perms) =
      if readonly then (Unix.O_RDONLY, 0o444) else (Unix.O_RDWR, 0o644)
    in
    let* fd =
      Lwt_unix.openfile
        (Node_data.l2blocks_data data_dir)
        [Unix.O_CREAT; O_CLOEXEC; flag]
        perms
    in
    let index =
      L2_block_index.v
        ~log_size:blocks_log_size
        ~readonly
        (Node_data.l2blocks_index data_dir)
    in
    let scheduler = Lwt_idle_waiter.create () in
    let cache = Cache.create cache_size in
    Lwt.return {index; fd; scheduler; cache}

  let close store =
    let open Lwt_syntax in
    Lwt_idle_waiter.force_idle store.scheduler @@ fun () ->
    (try L2_block_index.close store.index with Index.Closed -> ()) ;
    let* _ignore = Lwt_utils_unix.safe_close store.fd in
    Lwt.return_unit
end

module Head_store = Make_singleton (struct
  type t = L2block.hash

  let name = "head"

  let encoding = L2block.Hash.encoding
end)

module Tezos_head_store = Make_singleton (struct
  type t = Block_hash.t

  let name = "tezos_head"

  let encoding = Block_hash.encoding
end)

type rollup_info = {
  rollup_id : Protocol.Alpha_context.Tx_rollup.t;
  origination_block : Block_hash.t;
  origination_level : int32;
}

module Rollup_info_store = Make_singleton (struct
  type t = rollup_info

  let name = "rollup_info"

  let encoding =
    let open Data_encoding in
    conv
      (fun {rollup_id; origination_block; origination_level} ->
        (rollup_id, origination_block, origination_level))
      (fun (rollup_id, origination_block, origination_level) ->
        {rollup_id; origination_block; origination_level})
    @@ obj3
         (req "rollup_id" Protocol.Alpha_context.Tx_rollup.encoding)
         (req "origination_block" Block_hash.encoding)
         (req "origination_level" int32)
end)

module Finalized_level_store = Make_singleton (struct
  type t = Protocol.Alpha_context.Tx_rollup_level.t

  let name = "finalized_level"

  let encoding = Protocol.Alpha_context.Tx_rollup_level.encoding
end)

type t = {
  blocks : L2_block_store.t;
  tezos_blocks : Tezos_block_store.t;
  levels : Level_store.t;
  commitments : Commitment_store.t;
  head : Head_store.t;
  tezos_head : Tezos_head_store.t;
  rollup_info : Rollup_info_store.t;
  finalized_level : Finalized_level_store.t;
}

let init ~data_dir ~readonly ~blocks_cache_size =
  let open Lwt_syntax in
  let* () = Node_data.mk_store_dir data_dir in
  let* blocks =
    L2_block_store.init ~data_dir ~readonly ~cache_size:blocks_cache_size
  and* tezos_blocks = Tezos_block_store.init ~data_dir ~readonly
  and* levels = Level_store.init ~data_dir ~readonly
  and* commitments = Commitment_store.init ~data_dir ~readonly
  and* head = Head_store.init ~data_dir
  and* tezos_head = Tezos_head_store.init ~data_dir
  and* rollup_info = Rollup_info_store.init ~data_dir
  and* finalized_level = Finalized_level_store.init ~data_dir in
  return
    {
      blocks;
      tezos_blocks;
      commitments;
      levels;
      head;
      tezos_head;
      rollup_info;
      finalized_level;
    }

let close stores =
  let open Lwt_syntax in
  let* () = L2_block_store.close stores.blocks
  and* () = Tezos_block_store.close stores.tezos_blocks
  and* () = Level_store.close stores.levels
  and* () = Commitment_store.close stores.commitments in
  return_unit
