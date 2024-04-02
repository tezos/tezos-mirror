(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let () =
  Random.self_init () ;
  Alcotest.run
    "tezos-data-encoding"
    [
      ("success", Success.tests);
      ("invalid_encoding", Invalid_encoding.tests);
      ("read_failure", Read_failure.tests);
      ("write_failure", Write_failure.tests);
      ("randomized", Randomized.tests);
      ("versioned", Versioned.tests);
      ("registration", Registrationed.tests);
      ("mu", Mu.tests);
      ("slice", Slice_test.tests);
      ("conv_with_guard", Guarded_conv.tests);
      ("with_decoding_guard", Guarded_decode.tests);
      ("int31_int32", Int31_int32.tests);
      ("inline_phantom", Reference_check.Inline_phantom.tests);
      ("mu_phantom", Reference_check.Mu_phantom.tests);
      ("fixed_list", Fixed_list.tests);
      ("fixed_array", Fixed_array.tests);
      ("compact", Compact.tests);
      ("check-size", Check_size_negative.tests);
      ("uint-like-n", Uint_like_n.tests);
      ("int-like-z", Int_like_z.tests);
      ("safer-encoding", Test_safer_encoding.tests);
      ("lazy-bytes", Lazy_bytes.tests);
    ]
