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

open! Import

type t = { mutable buffer : bytes; mutable position : int }

external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
  = "caml_blit_string"
[@@noalloc]
(** Bytes.unsafe_blit_string not available in OCaml 4.08. *)

let create n = { buffer = Bytes.create n; position = 0 }

let write_with (write : string -> int -> int -> unit) b =
  write (Bytes.unsafe_to_string b.buffer) 0 b.position

let length b = b.position
let is_empty b = b.position = 0
let clear b = b.position <- 0

let resize b more =
  let old_pos = b.position in
  let old_len = Bytes.length b.buffer in
  let new_len = ref old_len in
  while old_pos + more > !new_len do
    new_len := 2 * !new_len
  done;
  let new_buffer = Bytes.create !new_len in
  Bytes.blit b.buffer 0 new_buffer 0 b.position;
  b.buffer <- new_buffer

let add_substring b s ~off ~len =
  let new_position = b.position + len in
  if new_position > Bytes.length b.buffer then resize b len;
  unsafe_blit_string s off b.buffer b.position len;
  b.position <- new_position

let blit ~src ~src_off ~dst ~dst_off ~len =
  assert (src_off + len <= src.position);
  Bytes.blit src.buffer src_off dst dst_off len

let add_string b s = add_substring b s ~off:0 ~len:(String.length s)
