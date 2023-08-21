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

module Bytes_lookup : functor (L : LIB) -> sig
  open L

  type bl = bool list

  val bor : bl repr -> bl repr -> bl repr t

  val xor : bl repr -> bl repr -> bl repr t

  val band : bl repr -> bl repr -> bl repr t

  val not : bl repr -> bl repr t
end =
functor
  (L : LIB)
  ->
  struct
    open L

    type bl = bool list

    let bor a b =
      let* l = map2M Bool.Internal.bor_lookup (of_list a) (of_list b) in
      ret @@ to_list l

    let xor a b =
      let* l = map2M Bool.Internal.xor_lookup (of_list a) (of_list b) in
      ret @@ to_list l

    let band a b =
      let* l = map2M Bool.Internal.band_lookup (of_list a) (of_list b) in
      ret @@ to_list l

    let not b =
      let* l = mapM Bool.Internal.bnot_lookup (of_list b) in
      ret @@ to_list l
  end

module Bool : Test =
functor
  (L : LIB)
  ->
  struct
    open L
    open L.Bool

    open Utils (L)

    module Limbs4 = Limbs (struct
      let nb_bits = 4
    end)

    module Bytes_lookup = Bytes_lookup (L)

    let test_bor x y z () =
      (* A dummy input with the value of zero needs to be added if the number of
         rows in the table is smaller than the number of wires architecture *)
      let* _ = input ~kind:`Public (Input.bool false) in
      let* x = input ~kind:`Public x in
      let* y = input y in
      let* z = input z in
      let* z' = Internal.bor_lookup x y in
      assert_equal z z'

    let tests_bor =
      List.map
        (test ~valid:true ~name:"Bool.test_bor_lookup")
        Input.
          [
            test_bor (bool false) (bool false) (bool false);
            test_bor (bool false) (bool true) (bool true);
            test_bor (bool true) (bool false) (bool true);
            test_bor (bool true) (bool true) (bool true);
          ]

    let test_bor_bytes a b z () =
      let* a = input ~kind:`Public a in
      let* b = input b in
      let* z = input z in
      let* z' = Bytes_lookup.bor a b in
      assert_equal z z'

    let tests_bor_bytes =
      List.map
        (fun (valid, a, b, o) ->
          let a = Bytes.(input_bytes ~le:false @@ Stdlib.Bytes.of_string a) in
          let b = Bytes.(input_bytes ~le:false @@ Stdlib.Bytes.of_string b) in
          let o = Bytes.(input_bytes ~le:false @@ Stdlib.Bytes.of_string o) in
          test ~valid ~name:"Bool.test_bor_bytes" @@ test_bor_bytes a b o)
        [(true, "\0011", "\0101", "\0111"); (false, "\0000", "\0000", "\0001")]

    let test_xor x y z () =
      let* x = input ~kind:`Public x in
      let* y = input y in
      let* z = input z in
      let* z' = Internal.xor_lookup x y in
      assert_equal z z'

    let tests_xor =
      List.map
        (test ~valid:true ~name:"Bool.test_xor_lookup")
        Input.
          [
            test_xor (bool false) (bool false) (bool false);
            test_xor (bool false) (bool true) (bool true);
            test_xor (bool true) (bool false) (bool true);
            test_xor (bool true) (bool true) (bool false);
          ]

    let test_xor_bytes a b z () =
      let* a = input ~kind:`Public a in
      let* b = input b in
      let* z = input z in
      let* z' = Bytes_lookup.xor a b in
      assert_equal z z'

    let test_xor_bytes4 a b z () =
      let* a = input ~kind:`Public a in
      let* b = input b in
      let* z = input z in
      let* z' = Limbs4.xor a b in
      assert_equal z z'

    let bytes_of_hex = Plompiler.Utils.bytes_of_hex

    let bool_list_input_bytes x = Bytes.input_bytes ~le:false x

    let limb_list_input_bytes4 x = Limbs4.input_bytes ~le:false x

    let tests_xor_bytes str input_bytes f =
      List.map
        (fun (valid, a, b, o) ->
          let a = input_bytes @@ bytes_of_hex a in
          let b = input_bytes @@ bytes_of_hex b in
          let o = input_bytes @@ bytes_of_hex o in
          test ~valid ~name:("Bool.test_xor_bytes" ^ str) @@ f a b o)
        [
          (true, "00", "00", "00");
          (true, "0F", "00", "0F");
          (true, "00", "0F", "0F");
          (true, "0F", "0F", "00");
          (true, "F0", "00", "F0");
          (true, "00", "F0", "F0");
          (true, "F0", "F0", "00");
          (false, "0F", "00", "00");
        ]

    let test_bnot x z () =
      let* x = input ~kind:`Public x in
      let* z = input z in
      let* z' = Bool.Internal.bnot_lookup x in
      assert_equal z z'

    let tests_bnot =
      List.map
        (test ~valid:true ~name:"Bool.test_bnot_lookup")
        Input.
          [
            test_bnot (bool false) (bool true);
            test_bnot (bool true) (bool false);
          ]

    let test_bnot_bytes b z () =
      let* b = input ~kind:`Public b in
      let* z = input z in
      let* z' = Bytes_lookup.not b in
      assert_equal z z'

    let test_bnot_bytes4 b z () =
      let* b = input ~kind:`Public b in
      let* z = input z in
      let* z' = Limbs4.not b in
      assert_equal z z'

    let tests_bnot_bytes str input_bytes f =
      List.map
        (fun (valid, b, o) ->
          let b = input_bytes @@ bytes_of_hex b in
          let o = input_bytes @@ bytes_of_hex o in
          test ~valid ~name:("Bool.test_bnot_bytes" ^ str) @@ f b o)
        [
          (true, "00", "FF");
          (true, "0F", "F0");
          (true, "F0", "0F");
          (true, "FF", "00");
          (false, "0F", "00");
        ]

    let test_rotate_right4 l i z () =
      let* l = input ~kind:`Public l in
      let* z = input z in
      let* o = Limbs4.rotate_right l i in
      assert_equal o z

    let tests_rotate_right =
      List.map
        (fun (i, a, b) ->
          let a = limb_list_input_bytes4 @@ Stdlib.Bytes.of_string a in
          let b = limb_list_input_bytes4 @@ Stdlib.Bytes.of_string b in
          test ~valid:true ~name:"Bytes.test_rotate_right4"
          @@ test_rotate_right4 a i b)
        [
          (0, "\001", "\001");
          (1, "\001", "\128");
          (2, "\001", "\064");
          (3, "\001", "\032");
          (4, "\001", "\016");
          (5, "\001", "\008");
          (6, "\001", "\004");
          (7, "\001", "\002");
          (8, "\001", "\001");
          (0, "\000\001", "\000\001");
          (1, "\000\001", "\128\000");
          (2, "\000\001", "\064\000");
          (3, "\000\001", "\032\000");
          (4, "\000\001", "\016\000");
          (5, "\000\001", "\008\000");
          (6, "\000\001", "\004\000");
          (7, "\000\001", "\002\000");
          (8, "\000\001", "\001\000");
          (9, "\000\001", "\000\128");
          (1, "\001\000", "\000\128");
          (0, "\000\000\001", "\000\000\001");
          (1, "\000\000\001", "\128\000\000");
          (2, "\000\000\001", "\064\000\000");
          (3, "\000\000\001", "\032\000\000");
          (4, "\000\000\001", "\016\000\000");
          (5, "\000\000\001", "\008\000\000");
          (6, "\000\000\001", "\004\000\000");
          (7, "\000\000\001", "\002\000\000");
          (8, "\000\000\001", "\001\000\000");
          (9, "\000\000\001", "\000\128\000");
        ]

    let test_shift_right4 l i z () =
      let* l = input ~kind:`Public l in
      let* z = input z in
      (* We use this conversion to make sure that
         we do not have unused inputs *)
      let* l = Limbs4.of_bool_list l in
      let* o = Limbs4.shift_right l i in
      assert_equal o z

    let tests_shift_right =
      List.map
        (fun (i, a, b) ->
          let a = bool_list_input_bytes @@ bytes_of_hex a in
          let b = limb_list_input_bytes4 @@ bytes_of_hex b in
          test ~valid:true ~name:"Bytes.test_shift_right4"
          @@ test_shift_right4 a i b)
        [
          (0, "B0", "B0");
          (1, "B0", "58");
          (2, "B0", "2C");
          (3, "B0", "16");
          (4, "B0", "0B");
          (5, "B0", "05");
          (6, "B0", "02");
          (7, "B0", "01");
          (8, "B0", "00");
        ]

    let tests =
      tests_bor @ tests_bor_bytes @ tests_xor
      @ tests_xor_bytes "1" bool_list_input_bytes test_xor_bytes
      @ tests_xor_bytes "4" limb_list_input_bytes4 test_xor_bytes4
      @ tests_bnot
      @ tests_bnot_bytes "1" bool_list_input_bytes test_bnot_bytes
      @ tests_bnot_bytes "4" limb_list_input_bytes4 test_bnot_bytes4
      @ tests_rotate_right @ tests_shift_right
  end

let tests =
  let both = [("Bool", (module Bool : Test))] in
  List.map (fun (name, m) -> Alcotest.test_case name `Quick (to_test m)) both
  @ List.map
      (fun (name, m) ->
        Alcotest.test_case
          (name ^ " plonk")
          `Slow
          (to_test ~plonk:(module Plonk.Main_protocol) m))
      both
