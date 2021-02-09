(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Test_fuzzing_tests

module SeqWithBase = struct
  type 'a elt = 'a

  include Support.Lib.Seq

  let of_list = List.to_seq

  let to_list = List.of_seq

  let name = "Seq"

  let pp fmt s = Crowbar.(pp_list pp_int) fmt (to_list s)
end

(* Internal consistency *)
module TestSeqIterFold = TestIterFold (SeqWithBase)

(* consistency w.r.t. Stdlib *)
module Filter = TestFilterAgainstStdlibList (SeqWithBase)
module Filtermap = TestFiltermapAgainstStdlibList (SeqWithBase)
module Fold = TestFoldAgainstStdlibList (SeqWithBase)
module Iter = TestIterAgainstStdlibList (SeqWithBase)
module Iterp = TestIterMonotoneAgainstStdlibList (SeqWithBase)
module Map = TestMapAgainstStdlibList (SeqWithBase)
module Mapp = TestMappAgainstStdlibList (SeqWithBase)
module Find = TestFindStdlibList (SeqWithBase)
