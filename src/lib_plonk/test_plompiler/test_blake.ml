(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Plompiler
open Plonk_test
module CS = Plonk.Circuit
open Helpers

module Blake (L : LIB) = struct
  open L

  open Utils (L)

  module B = Plompiler.Blake2s (L)

  let test_mixing (constants : Bytes.bl Input.t Array.t) z exp () =
    let* constants =
      foldM
        (fun acc c ->
          let* w = input ~kind:`Public c in
          ret @@ (w :: acc))
        []
        (Array.to_list constants)
    in
    let constants = Array.of_list (List.rev constants) in
    let* z = input z in
    let* exp =
      foldM
        (fun acc c ->
          let* w = input c in
          ret @@ (w :: acc))
        []
        (Array.to_list exp)
    in
    let exp = Array.of_list (List.rev exp) in
    let* constants = B.mixing_g constants 0 4 8 12 z z in
    fold2M
      (fun _ a b -> assert_equal a b)
      unit
      (Array.to_list constants)
      (Array.to_list exp)

  let input_bitlist l = Input.list (List.map Input.bool l)

  let tests_mixing =
    let constants =
      Array.of_list
      @@ List.map
           (fun s ->
             input_bitlist Plompiler.Utils.(bitlist ~le:false @@ bytes_of_hex s))
           (List.init 16 (fun _ -> "00000001"))
    in
    let z =
      input_bitlist
        Plompiler.Utils.(bitlist ~le:false @@ bytes_of_hex "00000001")
    in
    let exp =
      Array.of_list
      @@ List.map
           (fun s ->
             input_bitlist Plompiler.Utils.(bitlist ~le:false @@ bytes_of_hex s))
           [
             "00000024";
             "00000001";
             "00000001";
             "00000001";
             "42480404";
             "00000001";
             "00000001";
             "00000001";
             "24020201";
             "00000001";
             "00000001";
             "00000001";
             "24000200";
             "00000001";
             "00000001";
             "00000001";
           ]
    in
    [test ~valid:true ~name:"Blake.test_mixing" @@ test_mixing constants z exp]

  let test_compression h m exp () =
    let* h =
      foldM
        (fun acc c ->
          let* w = input ~kind:`Public c in
          ret @@ (w :: acc))
        []
        (Array.to_list h)
    in
    let h = Array.of_list (List.rev h) in
    let* m =
      foldM
        (fun acc c ->
          let* w = input c in
          ret @@ (w :: acc))
        []
        (Array.to_list m)
    in
    let m = Array.of_list (List.rev m) in
    let* exp =
      foldM
        (fun acc c ->
          let* w = input c in
          ret @@ (w :: acc))
        []
        (Array.to_list exp)
    in
    let exp = Array.of_list (List.rev exp) in
    let* h = B.compression h m (Stdint.Uint64.of_int 65) false in
    (* assert_equal (to_list @@ List.rev @@ of_list @@ h.(0)) exp.(0) *)
    fold2M
      (fun _ a b -> assert_equal a b)
      unit
      (Array.to_list h)
      (Array.to_list exp)

  let tests_compression =
    let h =
      Array.of_list
      @@ List.map
           (fun s ->
             input_bitlist Plompiler.Utils.(bitlist ~le:false @@ bytes_of_hex s))
           (List.init 8 (fun _ -> "00000001"))
    in
    let m =
      Array.of_list
      @@ List.map
           (fun s ->
             input_bitlist Plompiler.Utils.(bitlist ~le:false @@ bytes_of_hex s))
           (List.init 16 (fun _ -> "00000001"))
    in
    let exp =
      Array.of_list
      @@ List.map
           (fun s ->
             input_bitlist Plompiler.Utils.(bitlist ~le:false @@ bytes_of_hex s))
           [
             "6f226bc1";
             "40e44908";
             "d271fbed";
             "0f593d3a";
             "4ea6db39";
             "32c6318c";
             "a8e058a1";
             "16c949ad";
           ]
    in

    [
      test ~valid:true ~name:"Blake.test_compression" @@ test_compression h m exp;
    ]

  let test_blake2s i exp () =
    let* i = input ~kind:`Public i in
    let* exp = input exp in
    let personalization = Plompiler.Utils.bytes_of_hex "0000000000000000" in
    let* o = B.blake2s i personalization in
    assert_equal exp o

  let tests_blake2s =
    let i =
      input_bitlist
        Plompiler.Utils.(bitlist ~le:false (Stdlib.Bytes.make 65 '\xff'))
    in
    let exp =
      input_bitlist
        Plompiler.Utils.(
          bitlist ~le:false
          @@ bytes_of_hex
               "d63a602a01d5982f054e07431bb0f93139c135f296f7fc4c131a3d2a8f28dc2c")
    in
    [test ~valid:true ~name:"Blake.test_blake2s" @@ test_blake2s i exp]

  let tests = tests_mixing @ tests_compression @ tests_blake2s
end

let tests =
  [
    Alcotest.test_case "Blake2s" `Quick (to_test (module Blake : Test));
    Alcotest.test_case
      "Blake2s plonk"
      `Slow
      (to_test ~plonk:(module Plonk.Main_protocol) (module Blake : Test));
  ]
