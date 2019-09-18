(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* this module is a temporary fix waiting for ocaml 4.08 *)

(** {1 Binary encoding/decoding of integers} *)

external get_uint8 : bytes -> int -> int = "%bytes_safe_get"

external get_uint16_ne : bytes -> int -> int = "%caml_bytes_get16"

external get_int32_ne : bytes -> int -> int32 = "%caml_bytes_get32"

external get_int64_ne : bytes -> int -> int64 = "%caml_bytes_get64"

external set_int8 : bytes -> int -> int -> unit = "%bytes_safe_set"

external set_int16_ne : bytes -> int -> int -> unit = "%caml_bytes_set16"

external set_int32_ne : bytes -> int -> int32 -> unit = "%caml_bytes_set32"

external set_int64_ne : bytes -> int -> int64 -> unit = "%caml_bytes_set64"

external swap16 : int -> int = "%bswap16"

external swap32 : int32 -> int32 = "%bswap_int32"

external swap64 : int64 -> int64 = "%bswap_int64"

let get_int8 b i = (get_uint8 b i lsl (Sys.int_size - 8)) asr (Sys.int_size - 8)

let get_uint16_le b i =
  if Sys.big_endian then swap16 (get_uint16_ne b i) else get_uint16_ne b i

let get_uint16_be b i =
  if not Sys.big_endian then swap16 (get_uint16_ne b i) else get_uint16_ne b i

let get_int16_ne b i =
  (get_uint16_ne b i lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int16_le b i =
  (get_uint16_le b i lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int16_be b i =
  (get_uint16_be b i lsl (Sys.int_size - 16)) asr (Sys.int_size - 16)

let get_int32_le b i =
  if Sys.big_endian then swap32 (get_int32_ne b i) else get_int32_ne b i

let get_int32_be b i =
  if not Sys.big_endian then swap32 (get_int32_ne b i) else get_int32_ne b i

let get_int64_le b i =
  if Sys.big_endian then swap64 (get_int64_ne b i) else get_int64_ne b i

let get_int64_be b i =
  if not Sys.big_endian then swap64 (get_int64_ne b i) else get_int64_ne b i

let set_int16_le b i x =
  if Sys.big_endian then set_int16_ne b i (swap16 x) else set_int16_ne b i x

let set_int16_be b i x =
  if not Sys.big_endian then set_int16_ne b i (swap16 x)
  else set_int16_ne b i x

let set_int32_le b i x =
  if Sys.big_endian then set_int32_ne b i (swap32 x) else set_int32_ne b i x

let set_int32_be b i x =
  if not Sys.big_endian then set_int32_ne b i (swap32 x)
  else set_int32_ne b i x

let set_int64_le b i x =
  if Sys.big_endian then set_int64_ne b i (swap64 x) else set_int64_ne b i x

let set_int64_be b i x =
  if not Sys.big_endian then set_int64_ne b i (swap64 x)
  else set_int64_ne b i x

let set_uint8 = set_int8

let set_uint16_ne = set_int16_ne

let set_uint16_be = set_int16_be

let set_uint16_le = set_int16_le

module type S = sig
  (** {1 Binary encoding/decoding of integers} *)

  (** The functions in this section binary encode and decode integers to
      and from byte sequences.
      All following functions raise [Invalid_argument] if the space
      needed at index [i] to decode or encode the integer is not
      available.
      Little-endian (resp. big-endian) encoding means that least
      (resp. most) significant bytes are stored first.  Big-endian is
      also known as network byte order.  Native-endian encoding is
      either little-endian or big-endian depending on {!Sys.big_endian}.
      32-bit and 64-bit integers are represented by the [int32] and
      [int64] types, which can be interpreted either as signed or
      unsigned numbers.
      8-bit and 16-bit integers are represented by the [int] type,
      which has more bits than the binary encoding.  These extra bits
      are handled as follows:
        {ul
          {- Functions that decode signed (resp. unsigned) 8-bit or 16-bit
          integers represented by [int] values sign-extend
          (resp. zero-extend) their result.}
          {- Functions that encode 8-bit or 16-bit integers represented by
          [int] values truncate their input to their least significant
          bytes.}
        }
  *)

  (** [get_uint8 b i] is [b]'s unsigned 8-bit integer starting at byte index [i].
      @since 4.08
  *)
  val get_uint8 : bytes -> int -> int

  (** [get_int8 b i] is [b]'s signed 8-bit integer starting at byte index [i].
      @since 4.08
  *)
  val get_int8 : bytes -> int -> int

  (** [get_uint16_ne b i] is [b]'s native-endian unsigned 16-bit integer
      starting at byte index [i].
      @since 4.08
  *)
  val get_uint16_ne : bytes -> int -> int

  (** [get_uint16_be b i] is [b]'s big-endian unsigned 16-bit integer
      starting at byte index [i].
      @since 4.08
  *)
  val get_uint16_be : bytes -> int -> int

  (** [get_uint16_le b i] is [b]'s little-endian unsigned 16-bit integer
      starting at byte index [i].
      @since 4.08
  *)
  val get_uint16_le : bytes -> int -> int

  (** [get_int16_ne b i] is [b]'s native-endian signed 16-bit integer
      starting at byte index [i].
      @since 4.08
  *)
  val get_int16_ne : bytes -> int -> int

  (** [get_int16_be b i] is [b]'s big-endian signed 16-bit integer
      starting at byte index [i].
      @since 4.08
  *)
  val get_int16_be : bytes -> int -> int

  (** [get_int16_le b i] is [b]'s little-endian signed 16-bit integer
      starting at byte index [i].
      @since 4.08
  *)
  val get_int16_le : bytes -> int -> int

  (** [get_int32_ne b i] is [b]'s native-endian 32-bit integer
      starting at byte index [i].
      @since 4.08
  *)
  val get_int32_ne : bytes -> int -> int32

  (** [get_int32_be b i] is [b]'s big-endian 32-bit integer
      starting at byte index [i].
      @since 4.08
  *)
  val get_int32_be : bytes -> int -> int32

  (** [get_int32_le b i] is [b]'s little-endian 32-bit integer
      starting at byte index [i].
      @since 4.08
  *)
  val get_int32_le : bytes -> int -> int32

  (** [get_int64_ne b i] is [b]'s native-endian 64-bit integer
      starting at byte index [i].
      @since 4.08
  *)
  val get_int64_ne : bytes -> int -> int64

  (** [get_int64_be b i] is [b]'s big-endian 64-bit integer
      starting at byte index [i].
      @since 4.08
  *)
  val get_int64_be : bytes -> int -> int64

  (** [get_int64_le b i] is [b]'s little-endian 64-bit integer
      starting at byte index [i].
      @since 4.08
  *)
  val get_int64_le : bytes -> int -> int64

  (** [set_uint8 b i v] sets [b]'s unsigned 8-bit integer starting at byte index
      [i] to [v].
      @since 4.08
  *)
  val set_uint8 : bytes -> int -> int -> unit

  (** [set_int8 b i v] sets [b]'s signed 8-bit integer starting at byte index
      [i] to [v].
      @since 4.08
  *)
  val set_int8 : bytes -> int -> int -> unit

  (** [set_uint16_ne b i v] sets [b]'s native-endian unsigned 16-bit integer
      starting at byte index [i] to [v].
      @since 4.08
  *)
  val set_uint16_ne : bytes -> int -> int -> unit

  (** [set_uint16_be b i v] sets [b]'s big-endian unsigned 16-bit integer
      starting at byte index [i] to [v].
      @since 4.08
  *)
  val set_uint16_be : bytes -> int -> int -> unit

  (** [set_uint16_le b i v] sets [b]'s little-endian unsigned 16-bit integer
      starting at byte index [i] to [v].
      @since 4.08
  *)
  val set_uint16_le : bytes -> int -> int -> unit

  (** [set_int16_ne b i v] sets [b]'s native-endian signed 16-bit integer
      starting at byte index [i] to [v].
      @since 4.08
  *)
  val set_int16_ne : bytes -> int -> int -> unit

  (** [set_int16_be b i v] sets [b]'s big-endian signed 16-bit integer
      starting at byte index [i] to [v].
      @since 4.08
  *)
  val set_int16_be : bytes -> int -> int -> unit

  (** [set_int16_le b i v] sets [b]'s little-endian signed 16-bit integer
      starting at byte index [i] to [v].
      @since 4.08
  *)
  val set_int16_le : bytes -> int -> int -> unit

  (** [set_int32_ne b i v] sets [b]'s native-endian 32-bit integer
      starting at byte index [i] to [v].
      @since 4.08
  *)
  val set_int32_ne : bytes -> int -> int32 -> unit

  (** [set_int32_be b i v] sets [b]'s big-endian 32-bit integer
      starting at byte index [i] to [v].
      @since 4.08
  *)
  val set_int32_be : bytes -> int -> int32 -> unit

  (** [set_int32_le b i v] sets [b]'s little-endian 32-bit integer
      starting at byte index [i] to [v].
      @since 4.08
  *)
  val set_int32_le : bytes -> int -> int32 -> unit

  (** [set_int64_ne b i v] sets [b]'s native-endian 64-bit integer
      starting at byte index [i] to [v].
      @since 4.08
  *)
  val set_int64_ne : bytes -> int -> int64 -> unit

  (** [set_int64_be b i v] sets [b]'s big-endian 64-bit integer
      starting at byte index [i] to [v].
      @since 4.08
  *)
  val set_int64_be : bytes -> int -> int64 -> unit

  (** [set_int64_le b i v] sets [b]'s little-endian 64-bit integer
      starting at byte index [i] to [v].
      @since 4.08
  *)
  val set_int64_le : bytes -> int -> int64 -> unit
end
