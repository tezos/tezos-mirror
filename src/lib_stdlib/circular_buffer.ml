(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* The buffer is just an array of bytes.
   We remember the segment which is full in the buffer. *)
type t = {
  buffer : Bytes.t;
  fresh_buf_size : int;
  mutable data_start : int;
  mutable data_end : int;
  mutable full : bool;
}

(* A piece of data is just an offset in the buffer with its length or a
   fresh buffer if the buffer is full. *)
type data = {offset : int; length : int; buf : Bytes.t}

let create ?(maxlength = 1 lsl 15) ?(fresh_buf_size = 2000) () =
  {
    buffer = Bytes.create maxlength;
    data_start = 0;
    data_end = 0;
    full = false;
    fresh_buf_size;
  }

(* Invariant:
   - There is no two concurrent write at the same time
   - read should be called in the same order than write
 *)

(* [get_buf_with_offset t write_len] Find a place where [write_len] data can be written onto the buffer [t].

   Multiple situtation can arise

   1) STARTS preceeds END,
    _____START____________________END_______
   [_______|ddddddddddddddddddddddd|________]
   |<--Y-->|<----- data ---------->|<---X-->|

      1.1) either X zone can contain [write_len],
      1.2) or Y zone can contain [write_len],
      1.3) or neither is big enough,we create a temporary buffer of size [write_len]

   2) END  preceeds START ,
    ______END____________________START______
   [ddddddd|_______________________|dddddd__]

      2.1) either the free zone between END and START can contain [write_len],
      2.2) or we create a temporary buffer of size [write_len]

   3) START and END are identical
     3.1)
    ____START_END___________________________
   [_______|________________________________]=> t.full = false

     3.2)
    ____END_START___________________________
   [ddddddd|dddddddddddddddddddddddddddddd__] => t.full = true
*)
(* Pre-condition: write_len > 0 *)
let get_buf_with_offset t write_len =
  (* Case 3.1 -> put the pointers at the beginning of the buffer which
     may save some space. *)
  if t.data_start = t.data_end && not t.full then (
    t.data_start <- 0 ;
    t.data_end <- 0) ;
  if t.data_start < t.data_end || (t.data_start = t.data_end && not t.full) then
    if t.data_end + write_len <= Bytes.length t.buffer then
      (* case 1.1 and 3.1: we write after END *)
      (t.buffer, t.data_end)
    else if t.data_start >= write_len then
      (* case 1.2: we write before START *)
      (t.buffer, 0)
    else (* case 1.3:  not enough space *)
      (Bytes.create t.fresh_buf_size, 0)
  else if t.data_end < t.data_start then
    if t.data_end + write_len <= t.data_start then
      (* case 2.1: we write between END and START *)
      (t.buffer, t.data_end)
    else (* case 2.2: not enough space *)
      (Bytes.create t.fresh_buf_size, 0)
  else
    (* case 3.3: t.data_start =t.data_end && t.full *)
    (* not enough space *)
    (Bytes.create t.fresh_buf_size, 0)

(* [write ~maxlen ~fill_using:f buffer]
   - first ask a buffer,offset pair with enough space for writting maxlen data
   - calls [fill_using buf offset maxlen] and get the [written] bytes count
   - if we used the circular buffer and not a freshly allocated one, we update the data_end field

   After a correct write the following property holds:

   'o' stands for old data
   '_' for free zone
   'w' for just written data
   'r' is the returned record

   - initial situation STARTS preceeds END,
    ___________________START___________END_________
   [_____________________|oooooooooooooo|__________]
   |<--------X zone----->|              |<-Y zone->|

      - either X zone can contain [write_len],
         ___________________START_________OLD_END__NEW_END
        [_____________________|oooooooooooooo|wwwwwwww|_] = r.buf
                                             |<------>|
                                             |r.length|
                                             |
                                             r.offset

      - or Y zone can contain [write_len],
         _________NEW_END____START_________OLD_END______
        [wwwwwwwwwwww|________|oooooooooooooo|__________] = r.buf
        |<-r.length->|
        |
        r.offset=0

      - or neither is big enough,we create a temporary buffer of size [write_len]
         ___________________START___________END_________    |<---max_len---->|
        [_____________________|oooooooooooooo|__________]   [wwwwwwwwwwww____]= r.buf
                                                            |<-r.length->|
                                                            |
                                                            r.offset=0
   -  END  preceeds START ,
       ______END____________________START______
      [ooooooo|_______________________|oooooo__]

      - either empty zone can contain [write_len] and a little bit more,
         _____OLD__END______NEW_END___START______
        [ooooooo|wwwwwwwwwwwwwwww|______|oooooo__]
                |<------>|
                |r.length|
                |
                r.offset
      - either empty zone can contain [write_len],
         _____OLD__END___________NEW_END_START___
        [ooooooo|wwwwwwwwwwwwwwwwwwwwwww|oooooo__]
                |<------>|
                |r.length|
                |
                r.offset
                t.full = true

      - or we create a temporary buffer of size [max_len]
        ______END____________________START______  |<--------max_len----------->|
       [ooooooo|_______________________|oooooo__] [wwwwwwwwwwwwwwwwwwwwwwww____]
                                                  |<-------r.length----------->|
                                                  |
                                                  r.offset=0
*)
let write ~maxlen ~fill_using t =
  if maxlen < 0 then invalid_arg "Circular_buffer.write: negative length." ;
  if maxlen = 0 then
    Lwt.return {offset = t.data_end; length = 0; buf = t.buffer}
  else
    let open Lwt.Syntax in
    let (buf, offset) = get_buf_with_offset t maxlen in
    let maxlen =
      if buf == t.buffer then maxlen else min t.fresh_buf_size maxlen
    in
    let* written = fill_using buf offset maxlen in
    if written > maxlen then
      invalid_arg "Circular_buffer.write: written more bytes than maxlen" ;
    if t.buffer == buf then (
      t.data_end <- written + offset ;
      if t.data_end = t.data_start then t.full <- true) ;
    Lwt.return {offset; length = written; buf}

(* [read data ?len t ~into ~offset]  will read [len] data from
   [data.buf]  and update [t.data_start] pointer accordingly.
   data are blit into buffer [into] at [offset].

   if data.buf is not the circular buffer, it is supposed to be a
   dedicated buffer allocated at write time and we have no bookkeeping
   to do on the circular buffer.

   Else starting from

    ______START____________END_____
   [________|ddddd|ddddddddd|______]  [dddddddddddddddddddddddd____]
            |<--->|<------->|         |<---------------------->|
              d1       d3                         d2
   It is required to read fully d1, d2, and then d3 in that order.

   We can have a parial read for each chunk leading to a new data chunk d1'
    _________START_________END_____
   [___________|dd|ddddddddd|______]  [dddddddddddddddddddddddd____]
               |<>|<------->|         |<---------------------->|
                d1'    d3                         d2
   but the remainder has to be consumed to ensure that further readings
    will succeed.
    ____________START______END_____
   [______________|ddddddddd|______]  [dddddddddddddddddddddddd____]
                  |<------->|         |<---------------------->|
                       d3                         d2
   When reading extra allocated chunk we don't have to do any bookkeeping
    ____________START______END_____
   [______________|ddddddddd|______]  [___________|dddddddddddd____]
                  |<------->|                     |<---------->|
                       d3                               d2'

   Each time we read a chunk in the circular buffer we move start at the
   end of chunk we just read.

   Most of the time START points to the begining of the next chunk to
   read, but in one case starting from this situation (where d2
    was to big to fit after d1)

    _______END____START____
   [dddd|dddd|_____|dddd|__]
   | d2 | d3 |     | d1 |

   reading d1 then d2 leads to

    _______END___________START
   [dddd|dddd|_____________|__]

   Thats why we do
         t.data_start <- data.offset + len ;
   and not
         t.data_start <- t.data_start + len ;

   An alternative would be to remember that the last bytes of the buffer
   where not used, and to check whether start should be set at the
   begining of the buffer at each read.
*)
let read data ?(len = data.length) t ~into ~offset =
  if len > data.length then
    invalid_arg "Circular_buffer.read: len > (length data)." ;
  if len < 0 then invalid_arg "Circular_buffer.read: negative length." ;
  if len = 0 && data.length = 0 then None
  else if len = 0 then Some data
  else (
    (* copying data *)
    Bytes.blit data.buf data.offset into offset len ;
    (* updating data_start pointer *)
    if data.buf == t.buffer then (
      t.full <- false ;
      t.data_start <- data.offset + len ;
      (*
       In the buffer, data is always contiguous (in particular it is not
       splitted when reaching the end: we just leave unused space at
       the end and write at the beginning).
       If [data] is well formed, it is the result of a write into the
       buffer, and [len] is at most the length of data, so we have
       previously written [len] data in [t.buffer] starting at
       [offset]. So the following assertion must hold. *)
      assert (t.data_start <= Bytes.length t.buffer)) ;
    (* computing remainder *)
    if len = data.length then None
    else
      (* Return a new handler if we did not read the whole chunck *)
      Some
        {offset = data.offset + len; length = data.length - len; buf = data.buf})

let length {length; _} = length
