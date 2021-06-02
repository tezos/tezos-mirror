(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

let numbits_table =
  "\000\001\002\002\003\003\003\003\004\004\004\004\004\004\004\004\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008\008"

let numbits x =
  let x = ref x and n = ref 0 in
  (let y = (!x lsr 16) lsr 16 in
   if y <> 0 then (
     n := !n + 32 ;
     x := y)) ;
  (let y = !x lsr 16 in
   if y <> 0 then (
     n := !n + 16 ;
     x := y)) ;
  (let y = !x lsr 8 in
   if y <> 0 then (
     n := !n + 8 ;
     x := y)) ;
  !n + Char.code numbits_table.[!x]
