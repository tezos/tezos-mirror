(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Store_types

(** Block representation effectively stored on disk and its accessors. *)

(** {1 Type definitions and encodings} *)

(** The type for the effective [contents] of a block is its header and
    the [operations] it contains. Their metadata hashes are also
    present. *)
type contents = {
  header : Block_header.t;
  operations : Operation.t list list;
  block_metadata_hash : Block_metadata_hash.t option;
  operations_metadata_hashes : Operation_metadata_hash.t list list option;
}

(** The type for a block's [metadata] stored on disk. This
    representation is tightly linked to
    {!Tezos_validation.Block_validation.type-result} which also has a
    strong dependency to
    {!Tezos_protocol_environment.validation_result}.

    Some fields exposed by {!Tezos_validation.Block_validation.type-result}
    are unnecessary hence the lack of direct link. *)
type metadata = {
  message : string option;
  max_operations_ttl : int;
  last_preserved_block_level : Int32.t;
  block_metadata : Bytes.t;
  operations_metadata : Block_validation.operation_metadata list list;
}

(** The type for a [block] stored on disk.

    The [hash] of the block is also stored to improve efficiency by
    not forcing the user to hash the header. This also allows to store
    fake hashes (e.g. sandbox's genesis blocks) but should be
    prevented by the API.

    The [metadata] might not be present. The mutability flag allows
    users to re-use the same structure to store freshly loaded
    metadata. *)
type block = {
  hash : Block_hash.t;
  contents : contents;
  mutable metadata : metadata option;
}

type t = block

(**/**)

type legacy_metadata = {
  legacy_message : string option;
  legacy_max_operations_ttl : int;
  legacy_last_allowed_fork_level : Int32.t;
  legacy_block_metadata : Bytes.t;
  legacy_operations_metadata : Bytes.t list list;
}

type legacy_block = {
  legacy_hash : Block_hash.t;
  legacy_contents : contents;
  mutable legacy_metadata : legacy_metadata option;
}

val legacy_metadata_encoding : legacy_metadata Data_encoding.t

val legacy_encoding : legacy_block Data_encoding.t

(**/**)

(** {1 Genesis} *)

(** [create_genesis_block ~genesis context_hash] creates a default
    genesis block for the given [genesis] and its [context_hash] that
    contains metadata. *)
val create_genesis_block : genesis:Genesis.t -> Context_hash.t -> t

(** Encoding for {!type-contents}. *)
val contents_encoding : contents Data_encoding.t

(** Encoding for {!type-metadata}. *)
val metadata_encoding : metadata Data_encoding.t

(** Equality on {!block} *)
val equal : t -> t -> bool

(** Encoding for {!t} (and {!block}).

    {b Important} An encoded block is prefixed by 4 bytes which stands
    for the length of the data. This is the case with
    [Data_encoding.dynamic_size ~kind:`Uint30] encodings. This will be
    expected to be present to improve the store efficiency. *)
val encoding : t Data_encoding.t

(** [pp_json] pretty-print a block as JSON. *)
val pp_json : Format.formatter -> t -> unit

(** {1 Accessors} *)

(** [descriptor block] returns the pair (hash x level) of [block]. *)
val descriptor : t -> block_descriptor

(** [hash block] returns the stored [block]'s hash. It is not
    guaranteed to be the same as [Block_header.hash (header block)]
    (e.g. in sandbox, the genesis block might have a fake hash). *)
val hash : t -> Block_hash.t

(** [operations block] returns the list of list of operations
    contained in the [block]. *)
val operations : t -> Operation.t list list

(** {2 Block header accessors} *)

val header : t -> Block_header.t

val shell_header : t -> Block_header.shell_header

val level : t -> Int32.t

val proto_level : t -> int

val predecessor : t -> Block_hash.t

val timestamp : t -> Time.Protocol.t

val validation_passes : t -> int

val operations_hash : t -> Operation_list_list_hash.t

val fitness : t -> Fitness.t

val context : t -> Context_hash.t

val protocol_data : t -> Bytes.t

val block_metadata_hash : t -> Block_metadata_hash.t option

val operations_metadata_hashes : t -> Operation_metadata_hash.t list list option

(** {2 Metadata accessors} *)

val metadata : t -> metadata option

val message : metadata -> string option

val max_operations_ttl : metadata -> int

val last_preserved_block_level : metadata -> Int32.t

val block_metadata : metadata -> bytes

val operations_metadata :
  metadata -> Block_validation.operation_metadata list list

(** {1 Utility functions} *)

(** [check_block_consistency ?genesis_hash ?pred_block block] checks
    that the stored data is consistent:

    - Does the [hash] stored equals the result of [Block_header.hash]
      of its header and, if not, is this the stored [genesis_hash]?
    - Is the [block] a successor of [pred_block] with regards to its
      level and its predecessor's hash?
    - Are the stored operations hashes consistent regarding the stored
      operations hashes? *)
val check_block_consistency :
  ?genesis_hash:Block_hash.t -> ?pred_block:t -> t -> unit tzresult Lwt.t

(** [decode_metadata data] decodes metadata from [data] encoded either
    with the new encoding or the legacy one. *)
val decode_metadata : string -> metadata option
