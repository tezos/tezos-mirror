(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

module Array = Ctypes.CArray

type t = {
  raw : Unsigned.uint8 Array.t;
  min : Unsigned.uint32;
  max : Unsigned.uint32 option;
}

let get mem = Array.get mem.raw

let get_string mem ~address ~length =
  (* TODO: Add bounds check *)
  String.init length @@ fun i ->
  get mem (i + address) |> Unsigned.UInt8.to_int |> Char.chr

let set mem = Array.set mem.raw

let set_string mem ~address ~data =
  (* TODO: Add bounds check *)
  for offset = 0 to String.length data - 1 do
    set
      mem
      (offset + address)
      (String.get_uint8 data offset |> Unsigned.UInt8.of_int)
  done

let length mem = Array.length mem.raw

module Internal_for_tests = struct
  let of_list (content : Unsigned.uint8 list) =
    let mem_length = List.length content in
    let page_size = 0x10000 in
    let pages = mem_length / page_size in

    assert (Int.rem mem_length page_size = 0) ;

    {
      raw = Array.of_list Ctypes.uint8_t content;
      min = Unsigned.UInt32.of_int pages;
      max = None;
    }

  let to_list (mem : t) = Array.to_list mem.raw
end
