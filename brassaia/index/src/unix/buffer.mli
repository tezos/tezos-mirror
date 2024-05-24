(* The MIT License

   Copyright (c) 2021 Clément Pascutto <clement@tarides.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software. *)

(** Extensible buffers with non-allocating access to the buffer's contents. *)

type t
(** The type of buffers. *)

val create : int -> t
(** [create n] is a fresh buffer with initial size [n]. *)

val length : t -> int
(** [length b] is the number of bytes contained in the buffer. *)

val is_empty : t -> bool
(** [is_empty t] iff [t] contains 0 characters. *)

val clear : t -> unit
(** [clear t] clears the data contained in [t]. It does not reset the buffer to
    its initial size. *)

val add_substring : t -> string -> off:int -> len:int -> unit
(** [add_substring t s ~off ~len] appends the substring
    [s.(off) .. s.(off + len - 1)] at the end of [t], resizing [t] if necessary. *)

val add_string : t -> string -> unit
(** [add_string t s] appends [s] at the end of [t], resizing [t] if necessary. *)

val write_with : (string -> int -> int -> unit) -> t -> unit
(** [write_with writer t] uses [writer] to write the contents of [t]. [writer]
    takes a string to write, an offset and a length. *)

val blit : src:t -> src_off:int -> dst:bytes -> dst_off:int -> len:int -> unit
(** [blit] copies [len] bytes from the buffer [src], starting at offset
    [src_off], into the supplied bytes [dst], starting at offset [dst_off]. *)
