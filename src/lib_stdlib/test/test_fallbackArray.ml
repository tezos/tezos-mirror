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

(** Testing
    -------
    Component:    stdlib
    Invocation:   dune exec src/lib_stdlib/test/main.exe
    Subject:      Fallback arrays
 *)

module Tests = struct
  include Test_arrays.Make (struct
    include FallbackArray

    let set a i v =
      set a i v ;
      a
  end)

  open Alcotest

  let check_of_list () =
    let l = Array.to_list (Array.init 100 (fun i -> i)) in
    let open FallbackArray in
    let fallback = -1 in
    let farr = of_list ~fallback ~proj:(fun e -> e) l in
    let farr_fallback = FallbackArray.fallback farr in
    if not Compare.Int.(farr_fallback = fallback) then
      fail
        (Printf.sprintf "fallback should be %d (is %d)" fallback farr_fallback) ;
    let i = ref 0 in
    iter
      (fun e ->
        let expected = List.nth l !i in
        if not Compare.Int.(expected = e) then
          fail
            (Printf.sprintf
               "at index %d, fallback array (%d) disagrees with original list \
                (%d)"
               !i
               e
               expected)
        else incr i)
      farr

  let tests = ("of_list", `Quick, check_of_list) :: tests
end

let () = Alcotest.run ~__FILE__ "stdlib" [("FallbackArray", Tests.tests)]
