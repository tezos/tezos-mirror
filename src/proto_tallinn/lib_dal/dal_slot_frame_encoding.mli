(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech  <contact@trili.tech>                       *)
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

(* Library for encoding messages from distinct rollups into a slot. This
   library contains encoding and decoding functions for different versions of
   the slot-frame encoding. Currently, only one version, V0, exists. As the
   structure of slot-frame progresses, new versions will be added. *)

open Protocol
open Alpha_context

type error +=
  | Slot_size_is_too_big of {actual_size : int; max_size : int}
  | Wrong_slot_frame_version of {expected : int; provided : int}
  | Could_not_deserialize_slot

type version = int

module Rollups_map : Map.S with type key = Sc_rollup.Address.t

type message = string

(* The type that will be used to serialize and deserialize a slot into a
   slot-frame. It consists of a map from rollup addresses to list of
   messages. *)
type t = message list Rollups_map.t

(* Common interface for all slot versions. *)
module type Slot_version = sig
  (* [version_prefix] denotes the version of the slot-frame encoding.
     It must be a value between 0 and 255, and two different slot-frame
     econdings cannot have the same version prefix. *)
  val version_prefix : version

  (* [expected_slot_size rollups_messages] returns the value of the
     size (in bytes) of a slot-frame that includes all the messages
     in [rollups_messages]. It must satisfy the following property:
     `expected_slot_size rollups_messages =
      String.length @@ serialize rollups_messages`.
  *)
  val expected_slot_size : t -> int

  (* [serialize ~max_size rollups_messages] returns the encoding
      of [rollups_messages] as a string, provided that the result
      does not occupy more then [~max_size] byted.
      When it succeeds, it must satisfy the following property:
      `deserialize ~max_size @@ serialize ~max_size rollups_messages =
       rollups_messages`

      May fail with:
     {ul
       {li [Slot_size_is_too_big {actual_size; max_size}] if the encoding of the
         slot would take [actual_size] bytes, where `actual_size > max_size`}
     }
  *)
  val serialize : max_size:int -> t -> string tzresult Lwt.t

  (* [deserialize ~max_size slot_frame] returns the rollup-address indexed map
      of messages whose encoding corresponds to [slot_frame], as long as the
      [slot_frame] does not occupy more then [~max_size] bytes, and [slot_frame]
      contains the correct version_prefix, i.e. the first byte [slot_frame]
      is set to `\000`. When it succeeds, it must satisfy the following property:
      `serialize ~max_size @@ deserialize ~max_size slot_frame =
       slot_frame`

      May fail with:
     {ul
       {li [Slot_size_is_too_big {actual_size; max_size}] if
         [String.length slot_frame = actual_size], where
         `actual_size > max_size`.
        }
       {li [Wrong_slot_frame_version of {expected; provided = 0}] if the first
         byte of [slot_frame] is set to the binary encoding of [expected].
       }
       {li [Could_not_deserialize_slot] if [slot_frame] does not correspond to
         the serialization of a valid rollup-address indexed map of messages.
       }
     }
  *)
  val deserialize : max_size:int -> string -> t tzresult Lwt.t
end

(*'V0' version of the slot-frame encoding.
   Suppose that we want to include messages from rollups `r_1, ..., r_n`.
   Specifically, for `i=1, ..., n`, suppose that rollup `r_i` wants to include
   an arbitrary number j of messages `[m_{i,1}; ... m_{i, j} ]` into the slot.
   The number of messages j to be included in a slot may be different for each
   rollup. The encoded slot will consist of a string where:
   {ul
     {li The first byte contains the slot-frame version, currently set to `0x00`,}
     {li The next `n * 24 + 4` bytes contain the `rollups-frame`, which contains
         the information about where the messages for the rollups
         `r_1, .., r_n` are stored. Specifically, the first 4 bytes denote the
         length (in bytes) of the remaining part of the rollups-frame, which is
         `n * 24`. This is followed by `n` entries of `24` bytes each. For each
         entry, the first `20` bytes contain the encoded address of the rollup
         node, while the other `4` contain the offset - from the start of the
         slot - to the start of the rollup's messages-frame (described next).
       }
     {li The `4` bytes following the rollups frame contain the length of the
         rest of the encoded slot, }
     {li Next, there are n messages-frames, one for each rollup. For
         `i = 1, ..., n`, the size of the i-th messages-frame is
         `4 * n_i + (\sum_{j=1}^{n_i} |m_{i, j}|) + 4` bytes. The first four
         bytes denote the length of the messages-frame. Then we have the
         sequence of the encoded messages `m_{i,1}, ..., m_{i, j}`. The encoded
         message `m_{i,j}` consists of 4 bytes representing `|m_{i,j}|`,
         followed by `m_{i,j}` itself.
       }
   }

   As an example, suppose that we have two rollups `r1` and `r2`. For
   simplicity, let's assume that the binary represenation of `r1` and `r2` are
   `ROLLUP_ADDRESS_1XXXX` and `ROLLUP_ADDRESS_2YYYY`. Suppose that we want to
   include in the slot messages [["hello"; "world"]] from `r1`, and messages
   [["CAFEBABE"; "CAFEDEAD"]] from `r2` (in this order). The overall encoded
   frame will be
   "\000
    \000\000\000\048
    ROLLUP_ADDRESS_1XXXX\000\000\000\057
    ROLLUP_ADDRESS_2YYYY\000\000\000\079
    \000\000\000\050
    \000\000\000\018\000\000\000\005hello\000\000\000\005world
    \000\000\000\024\000\000\000\008CAFEBABE\000\000\000\008CAFEDEAD".
*)
module V0 : sig
  include Slot_version

  (* Functions used internally by the V0 version of the slot. These functions
     are exposed so that they can be used in tests. This is necessary as we
     will have implementations of the deserialization for the slot functions in
     different programming languages. Checking the values returned by these
     functions in tests will serve as documentation for developers wanting to
     implement their own version of the V0 slot-frame deserialization. *)
  module Internal : sig
    (* [messages_size message] returns the size in bytes that [message]
       would take in a slot-frame. *)
    val message_size : message -> int

    (* [messages_frame_size messages] returns the size in bytes that the
       encoding of messages as a messages frame would take in a slot-frame. *)
    val messages_frame_size : message list -> int

    (* [all_messages_frame all_messages] returns the size in bytes
       that the encoding of [all_messages] would take in a slot-frame.
       This value includes the 4 bytes that separate the rollups-frame
       from the rest of the slot frame in the encoding. *)
    val all_messages_frames_size : message list list -> int

    (* [rollup_entry_size] returns the size in bytes
       that one rollup would take in the rollups-frame.
    *)
    val rollup_entry_size : int

    (* [rollups_frame_size number_of_rollups] returns the size
       in bytes that the rollups-frame would take in a slot-frame,
       if the latter contains messages for [number_of_rollups] rollups. *)
    val rollups_frame_size : int -> int
  end
end
