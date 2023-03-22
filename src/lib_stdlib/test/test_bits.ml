(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Utils.Infix

(** Testing
    -------
    Component:    stdlib
    Invocation:   dune exec src/lib_stdlib/test/main.exe
    Subject:      On fast numbits implementation
 *)

let property x v =
  if x = 0 then v = 0
  else if x > max_int / 2 then v = Sys.int_size - 1
  else 1 lsl (v - 1) <= x && x < 1 lsl v

let check x =
  let nx = Bits.numbits x in
  if not (property x nx) then
    Alcotest.fail (Printf.sprintf "2 ^ (%d - 1) <= %d < 2 ^ %d" nx x nx)

let check_values _ =
  List.iter
    check
    ([max_int; 1 lsl 15; 1 lsl 16; 1 lsl 17; 1 lsl 31; 1 lsl 32; 1 lsl 33]
    @ (0 -- ((1 lsl 8) + 1)))

let exhaustive_check () =
  if Bits.numbits 0 <> 0 then Alcotest.fail "numbits 0 <> 0" ;
  for log2 = 1 to 31 do
    for i = 1 lsl (log2 - 1) to (1 lsl log2) - 1 do
      if Bits.numbits i <> log2 then
        Alcotest.fail (Printf.sprintf "numbits %d <> %d" i log2)
    done
  done

let tests =
  [
    ("numbits_on_samples", `Quick, check_values);
    ("numbits_correct", `Slow, exhaustive_check);
  ]

let () = Alcotest.run "stdlib" [("numbits", tests)]
