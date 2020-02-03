(*****************************************************************************)
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

type 'a t = {
  hash : 'a -> bytes;
  hashes : int;
  index_bits : int;
  countdown_bits : int;
  filter : bytes;
}

(* Reads [bits] bits of [bytes] at offset [ofs] as an OCaml int in big endian order.
   The function proceeds by iteratively blitting the bytes overlapping the sought
   bit interval into [v]. The superfluous bits at the beginning and at the end
   are them removed from [v], yielding the returned value.
 *)
let peek bytes ofs bits =
  if bits <= 0 then invalid_arg "Bloomer.peek: non positive bits value" ;
  if ofs < 0 then invalid_arg "Bloomer.peek: negative offset" ;
  if bits > Sys.word_size - 2 then
    invalid_arg "Bloomer.peek: indexes out of bounds" ;
  if bits + ofs > Bytes.length bytes * 8 then
    invalid_arg "Bloomer.peek: indexes out of bounds" ;
  let first = ofs / 8 in
  let last = first + (((ofs mod 8) + bits + 7) / 8) in
  let v = ref 0 in
  for i = last - 1 downto first do
    v := (!v lsl 8) lor Char.code (Bytes.get bytes i)
  done ;
  v := !v lsr (ofs mod 8) ;
  v := !v land ((1 lsl bits) - 1) ;
  !v

(* blits [bits] bits of [bytes] at offset [ofs] from an OCaml int in big endian order *)
let poke bytes ofs bits v =
  if v lsr bits <> 0 then invalid_arg "Bloomer.poke: value too large" ;
  if bits <= 0 then invalid_arg "Bloomer.poke: non positive bits value" ;
  if ofs < 0 then invalid_arg "Bloomer.poke: negative bits value" ;
  if bits > Sys.word_size - 2 then
    invalid_arg "Bloomer.peek: indexes out of bounds" ;
  if bits + ofs > Bytes.length bytes * 8 then
    invalid_arg "Bloomer.poke: indexes out of bounds" ;
  let first = ofs / 8 in
  let last = first + (((ofs mod 8) + bits + 7) / 8) in
  let cur = ref 0 in
  for i = last - 1 downto first do
    cur := (!cur lsl 8) lor Char.code (Bytes.get bytes i)
  done ;
  let mask = lnot (((1 lsl bits) - 1) lsl (ofs mod 8)) in
  let v = !cur land mask lor (v lsl (ofs mod 8)) in
  for i = first to last - 1 do
    Bytes.set bytes i (Char.chr ((v lsr ((i - first) * 8)) land 0xFF))
  done

let create ~hash ~hashes ~index_bits ~countdown_bits =
  if index_bits <= 0 || index_bits > 24 then
    invalid_arg "Bloomer.create: invalid value for index_bits" ;
  if countdown_bits <= 0 || countdown_bits > 24 then
    invalid_arg "Bloomer.create: invalid value for countdown_bits" ;
  let filter =
    Bytes.make ((((1 lsl index_bits) * countdown_bits) + 7) / 8) '\000'
  in
  {hash; hashes; index_bits; countdown_bits; filter}

let mem {hash; hashes; index_bits; countdown_bits; filter} x =
  let h = hash x in
  try
    for i = 0 to hashes - 1 do
      let j = peek h (index_bits * i) index_bits in
      if peek filter (j * countdown_bits) countdown_bits = 0 then raise Exit
    done ;
    true
  with Exit -> false

let add {hash; hashes; index_bits; countdown_bits; filter} x =
  let h = hash x in
  for i = 0 to hashes - 1 do
    let j = peek h (index_bits * i) index_bits in
    poke filter (j * countdown_bits) countdown_bits ((1 lsl countdown_bits) - 1)
  done

let rem {hash; hashes; index_bits; countdown_bits; filter} x =
  let h = hash x in
  for i = 0 to hashes - 1 do
    let j = peek h (index_bits * i) index_bits in
    poke filter (j * countdown_bits) countdown_bits 0
  done

let countdown {hash = _; hashes = _; index_bits; countdown_bits; filter} =
  for j = 0 to (1 lsl index_bits) - 1 do
    let cur = peek filter (j * countdown_bits) countdown_bits in
    if cur > 0 then poke filter (j * countdown_bits) countdown_bits (cur - 1)
  done

let clear {hash = _; hashes = _; index_bits = _; countdown_bits = _; filter} =
  Bytes.fill filter 0 (Bytes.length filter) '\000'
