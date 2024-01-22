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

module Num : Test =
functor
  (L : LIB)
  ->
  struct
    open L
    open L.Num

    open Utils (L)

    let test_bop f x y z () =
      let* x = input ~kind:`Public x in
      let* y = input y in
      let* z = input z in
      let* o = f x y in
      assert_equal o z

    let test_op f x z () =
      let* x = input ~kind:`Public x in
      let* z = input z in
      let* o = f x in
      assert_equal o z

    let tests_add =
      let x1, y1, z1 = (si 6, si 2, si 8) in
      let x2, y2, z2 = (si 6, si 0, si 6) in
      [
        test ~valid:true ~name:"Num.test_add" @@ test_bop add x1 y1 z1;
        test ~valid:true ~name:"Num.test_add" @@ test_bop add x2 y2 z2;
        test ~valid:false @@ test_bop add x1 y1 (si 4);
        test ~valid:false @@ test_bop add x2 y2 y2;
      ]

    let tests_add_constant =
      let x1, z1 = (si 6, si 7) in
      [
        test ~valid:true ~name:"Num.test_add_constant"
        @@ test_op (add_constant S.one) x1 z1;
        test ~valid:true ~name:"Num.test_add_constant"
        @@ test_op (add_constant S.zero) x1 x1;
        test ~valid:false @@ test_op (add_constant S.one) x1 x1;
        test ~valid:false @@ test_op (add_constant S.zero) x1 z1;
      ]

    let tests_sub =
      let sub = add ~qr:S.mone in
      let x1, y1, z1 = (si 6, si 2, si 4) in
      let x2, y2, z2 = (si 6, si 0, si 6) in
      [
        test ~valid:true ~name:"Num.test_sub" @@ test_bop sub x1 y1 z1;
        test ~valid:true ~name:"Num.test_sub" @@ test_bop sub x2 y2 z2;
        test ~valid:false @@ test_bop sub x1 y1 (si 8);
        test ~valid:false @@ test_bop sub x2 y2 y2;
      ]

    let tests_mul =
      let x1, y1, z1 = (si 6, si 2, si 12) in
      let x2, y2, z2 = (si 6, si 0, si 0) in
      let x3, y3, z3 = (si 6, si 1, si 6) in

      [
        test ~valid:true ~name:"Num.test_mul" @@ test_bop mul x1 y1 z1;
        test ~valid:true ~name:"Num.test_mul" @@ test_bop mul x2 y2 z2;
        test ~valid:true ~name:"Num.test_mul" @@ test_bop mul x3 y3 z3;
        test ~valid:false @@ test_bop mul x1 y1 (si 1);
        test ~valid:false @@ test_bop mul x2 y2 x2;
      ]

    let tests_div =
      let x1, y1, z1 = (si 6, si 2, si 3) in
      [
        test ~valid:true ~name:"Num.test_div" @@ test_bop div x1 y1 z1;
        test ~valid:false @@ test_bop div x1 y1 (si 4);
        test ~valid:false @@ test_bop div (si 0) (si 0) (si 4);
      ]

    let test_num x y z () =
      let* x = input ~kind:`Public x in
      let* y = input y in
      let* z = input z in
      let* o = add x y in
      let* o = mul o y in
      let* o = add_constant S.one o in
      let* o = div o y in
      assert_equal o z

    let tests_num =
      let i = si 1 in
      let o = si 3 in
      let bad_o = si 4 in
      [
        test ~valid:true ~name:"Num.test_num" @@ test_num i i o;
        test ~valid:false @@ test_num i i bad_o;
      ]

    let test_pow5 x z () =
      let* x = input ~kind:`Public x in
      let* z = input z in
      let* z' = pow5 x in
      assert_equal z z'

    let tests_pow5 =
      [
        test ~valid:true ~name:"pow5" @@ test_pow5 (si 0) (si 0);
        test ~valid:true @@ test_pow5 (si 1) (si 1);
        test ~valid:true @@ test_pow5 (si 2) (si 32);
        test ~valid:true @@ test_pow5 (si (-1)) (si (-1));
        test ~valid:false @@ test_pow5 (si 0) (si 1);
      ]

    let test_pow x n_list expected_res () =
      let* x = input ~kind:`Public x in
      let* expected_res = input ~kind:`Public expected_res in
      let* n_list = mapM input n_list in
      let* o = pow x n_list in
      assert_equal o expected_res

    let tests_pow =
      let open Input in
      [
        test ~valid:true ~name:"pow" @@ test_pow (si 0) [bool true] (si 0);
        test ~valid:true ~name:"pow" @@ test_pow (si 0) [bool false] (si 1);
        test ~valid:true ~name:"pow"
        @@ test_pow (si 1) [bool true; bool true] (si 1);
        test ~valid:true ~name:"pow"
        @@ test_pow (si 3) [bool true; bool true] (si 27);
        test ~valid:true ~name:"pow"
        @@ test_pow
             (si 2)
             [bool false; bool true; bool true; bool false]
             (si 64);
        test ~valid:true ~name:"pow"
        @@ test_pow
             (si 2)
             [
               bool true;
               bool false;
               bool false;
               bool true;
               bool true;
               bool false;
             ]
             (si 33554432);
        test ~valid:true ~name:"pow"
        @@ test_pow (si (-1)) [bool false; bool true; bool true] (si 1);
        test ~valid:false @@ test_pow (si 2) [bool true] (si 1);
      ]

    let tests_equal =
      [
        test ~valid:true ~name:"Num.test_equal" @@ test_equal (si 0) (si 0);
        test ~valid:false @@ test_equal (si 10) (si 0);
      ]

    let test_bits_of_scalar sx expected () =
      let nb_bits = List.length expected in
      let* x = input sx in
      let* xbits = bits_of_scalar ~nb_bits x in
      iterM
        (fun (xbit, e) ->
          if e = 0 then Bool.assert_false xbit else Bool.assert_true xbit)
        (List.combine (of_list xbits) expected)

    let tests_bits_of_scalar =
      let test = test ~name:"Num.bits_of_scalar" in
      [
        test ~valid:true @@ test_bits_of_scalar (si 9) [1; 0; 0; 1; 0];
        test ~valid:true @@ test_bits_of_scalar (si 30) [0; 1; 1; 1; 1];
        test ~valid:false @@ test_bits_of_scalar (si 3) [1; 1; 1; 0; 0];
      ]

    let test_upper_bound_unsafe ~bound sx () =
      let* x = input sx in
      with_bool_check @@ is_upper_bounded_unsafe ~bound x

    let test_upper_bound ~bound sx () =
      let* x = input sx in
      let* b = is_upper_bounded ~bound x in
      Bool.assert_true b

    let tests_upper_bound ~safe =
      let test = test ~name:"Num.is_upper_bounded" in
      let test_upper_bound =
        if safe then test_upper_bound else test_upper_bound_unsafe
      in
      [
        test ~valid:true @@ test_upper_bound ~bound:Z.one (si 0);
        test ~valid:true @@ test_upper_bound ~bound:(Z.of_int 5) (si 4);
        test ~valid:true @@ test_upper_bound ~bound:(Z.of_int 101) (si 100);
        test ~valid:true @@ test_upper_bound ~bound:Z.(S.order - one) (si (-2));
        test ~valid:true
        @@ test_upper_bound ~bound:Z.one (Input.scalar @@ S.(of_z order));
        test ~valid:false @@ test_upper_bound ~bound:Z.one (si 1);
        test ~valid:false @@ test_upper_bound ~bound:(Z.of_int 5) (si 5);
        test ~valid:false @@ test_upper_bound ~bound:(Z.of_int 5) (si 100);
        test ~valid:false @@ test_upper_bound ~bound:Z.one (si (-1));
        test ~valid:false @@ test_upper_bound ~bound:Z.(S.order - one) (si (-1));
      ]

    let test_upper_bound_totality is_upper_bounded ~bound sx () =
      let* x = input sx in
      let* _ = is_upper_bounded ~bound x in
      ret unit

    let tests_upper_bound_totality ~safe =
      let test = test ~name:"Num.is_upper_bounded_totality" in
      let test_upper_bound =
        test_upper_bound_totality (fun ~bound x ->
            if safe then is_upper_bounded ~bound x
            else is_upper_bounded_unsafe ~bound x)
      in
      [
        test ~valid:safe @@ test_upper_bound ~bound:(Z.of_int 5) (si 100);
        test ~valid:safe @@ test_upper_bound ~bound:Z.one (si (-1));
        test ~valid:safe
        @@ test_upper_bound ~bound:Z.(div S.order (of_int 2)) (si (-1));
      ]

    let test_geq (a, bound_a) (b, bound_b) () =
      let* a = input a in
      let* b = input b in
      let* r = Num.geq (a, bound_a) (b, bound_b) in
      Bool.assert_true r

    let tests_geq =
      let test = test ~name:"Num.geq" in
      [
        test ~valid:true @@ test_geq (si 0, Z.of_int 100) (si 0, Z.of_int 100);
        test ~valid:true @@ test_geq (si 5, Z.of_int 6) (si 3, Z.of_int 100);
        test ~valid:false @@ test_geq (si 5, Z.of_int 6) (si 6, Z.of_int 7);
      ]

    let tests =
      tests_add @ tests_add_constant @ tests_sub @ tests_mul @ tests_num
      @ tests_div @ tests_pow5 @ tests_pow @ tests_equal @ tests_bits_of_scalar
      @ tests_upper_bound ~safe:false
      @ tests_upper_bound ~safe:true
      @ tests_upper_bound_totality ~safe:false
      @ tests_upper_bound_totality ~safe:true
      @ tests_geq
  end

module Bool : Test =
functor
  (L : LIB)
  ->
  struct
    open L
    open L.Bool

    open Utils (L)

    let test_assert_true b () =
      let* b = input ~kind:`Public b in
      assert_true b

    let test_assert_false b () =
      let* b = input ~kind:`Public b in
      assert_false b

    let tests_assert_true_and_false =
      [
        test ~valid:true ~name:"Bool.test_assert_true_and_false"
        @@ test_assert_true (Input.bool true);
        test ~valid:false @@ test_assert_true (Input.bool false);
        test ~valid:true ~name:"Bool.test_assert_true_and_false"
        @@ test_assert_false (Input.bool false);
        test ~valid:false @@ test_assert_false (Input.bool true);
      ]

    let tests_equal =
      [
        test ~valid:true ~name:"Bool.test_equal"
        @@ test_equal (Input.bool true) (Input.bool true);
        test ~valid:false @@ test_equal (Input.bool false) (Input.bool true);
      ]

    let test_is_zero x () =
      let* x = input ~kind:`Public x in
      let* b = Num.is_zero x in
      assert_true b

    let tests_is_zero =
      [
        test ~valid:true ~name:"Bool.test_is_zero" @@ test_is_zero (si 0);
        test ~valid:false @@ test_is_zero (si 1);
      ]

    let test_is_not_zero x () =
      let* x = input ~kind:`Public x in
      let* b = Num.is_not_zero x in
      assert_true b

    let tests_is_not_zero =
      [
        test ~valid:true ~name:"Bool.test_is_not_zero" @@ test_is_not_zero (si 5);
        test ~valid:false @@ test_is_not_zero (si 0);
      ]

    let test_assert_bool x () =
      let* x = input ~kind:`Public x in
      Num.assert_bool x

    let tests_assert_bool =
      [
        test ~valid:true ~name:"Bool.test_assert_bool" @@ test_assert_bool (si 0);
        test ~valid:true ~name:"Bool.test_assert_bool"
        @@ test_assert_bool (si 1);
        test ~valid:false @@ test_assert_bool (si (2 + Random.int 1000));
      ]

    let test_band x y z () =
      let* x = input ~kind:`Public x in
      let* y = input y in
      let* z = input z in
      let* z' = band x y in
      assert_equal z z'

    let tests_band =
      List.map
        (test ~valid:true ~name:"Bool.test_band")
        Input.
          [
            test_band (bool false) (bool false) (bool false);
            test_band (bool false) (bool true) (bool false);
            test_band (bool true) (bool false) (bool false);
            test_band (bool true) (bool true) (bool true);
          ]

    let test_xor x y z () =
      let* x = input ~kind:`Public x in
      let* y = input y in
      let* z = input z in
      let* z' = xor x y in
      assert_equal z z'

    let tests_xor =
      List.map
        (test ~valid:true ~name:"Bool.test_xor")
        Input.
          [
            test_xor (bool false) (bool false) (bool false);
            test_xor (bool false) (bool true) (bool true);
            test_xor (bool true) (bool false) (bool true);
            test_xor (bool true) (bool true) (bool false);
          ]

    let test_bor x y z () =
      let* x = input ~kind:`Public x in
      let* y = input y in
      let* z = input z in
      let* z' = bor x y in
      assert_equal z z'

    let tests_bor =
      List.map
        (test ~valid:true ~name:"Bool.test_bor")
        Input.
          [
            test_bor (bool false) (bool false) (bool false);
            test_bor (bool false) (bool true) (bool true);
            test_bor (bool true) (bool false) (bool true);
            test_bor (bool true) (bool true) (bool true);
          ]

    let test_bnot x z () =
      let* x = input ~kind:`Public x in
      let* z = input z in
      let* z' = bnot x in
      assert_equal z z'

    let tests_bnot =
      List.map
        (test ~valid:true ~name:"Bool.test_bnot")
        Input.
          [
            test_bnot (bool false) (bool true);
            test_bnot (bool true) (bool false);
          ]

    let test_ifthenelse b x y z () =
      let* b = input ~kind:`Public b in
      let* x = input ~kind:`Public x in
      let* y = input ~kind:`Public y in
      let* z = input z in
      let* z' = ifthenelse b x y in
      assert_equal z z'

    let tests_ifthenelse =
      List.map
        (test ~valid:true ~name:"Bool.test_ifthenelse")
        Input.
          [
            test_ifthenelse (bool true) (si 0) (si 1) (si 0);
            test_ifthenelse (bool false) (si 0) (si 1) (si 1);
            test_ifthenelse (bool true) (bool true) (bool false) (bool true);
            test_ifthenelse (bool false) (bool true) (bool false) (bool false);
            test_ifthenelse
              (bool true)
              (pair (si 0) (si 1))
              (pair (si 2) (si 3))
              (pair (si 0) (si 1));
            test_ifthenelse
              (bool false)
              (pair (si 0) (si 1))
              (pair (si 2) (si 3))
              (pair (si 2) (si 3));
            test_ifthenelse
              (bool true)
              (list [si 0; si 1])
              (list [si 2; si 3])
              (list [si 0; si 1]);
            test_ifthenelse
              (bool false)
              (list [si 0; si 1])
              (list [si 2; si 3])
              (list [si 2; si 3]);
          ]
      @ List.map
          (test ~valid:false)
          Input.
            [
              test_ifthenelse (bool true) (si 0) (si 1) (si 1);
              test_ifthenelse (bool false) (si 0) (si 1) (si 0);
            ]

    let test_swap b x y u v () =
      let* b = input ~kind:`Public b in
      let* x = input ~kind:`Public x in
      let* y = input ~kind:`Public y in
      let* u = input u in
      let* v = input v in
      let* res = swap b x y in
      let res_u, res_v = of_pair res in
      let* _ = assert_equal res_u u in
      assert_equal res_v v

    let tests_swap =
      List.map
        (test ~valid:true ~name:"Bool.test_swap")
        Input.
          [
            test_swap (bool true) (si 0) (si 1) (si 1) (si 0);
            test_swap (bool false) (si 0) (si 1) (si 0) (si 1);
            test_swap
              (bool true)
              (bool true)
              (bool false)
              (bool false)
              (bool true);
            test_swap
              (bool false)
              (bool true)
              (bool false)
              (bool true)
              (bool false);
            test_swap
              (bool true)
              (pair (si 0) (si 1))
              (pair (si 2) (si 3))
              (pair (si 2) (si 3))
              (pair (si 0) (si 1));
            test_swap
              (bool false)
              (pair (si 0) (si 1))
              (pair (si 2) (si 3))
              (pair (si 0) (si 1))
              (pair (si 2) (si 3));
            test_swap
              (bool true)
              (list [si 0; si 1])
              (list [si 2; si 3])
              (list [si 2; si 3])
              (list [si 0; si 1]);
            test_swap
              (bool false)
              (list [si 0; si 1])
              (list [si 2; si 3])
              (list [si 0; si 1])
              (list [si 2; si 3]);
          ]
      @ List.map
          (test ~valid:false)
          Input.
            [
              test_swap (bool true) (si 0) (si 1) (si 0) (si 1);
              test_swap (bool false) (si 0) (si 1) (si 1) (si 0);
            ]

    let test_full_adder l r c_in c_out s_out () =
      let* l = input ~kind:`Public l in
      let* r = input ~kind:`Public r in
      let* c_in = input c_in in
      let* s_out = input s_out in
      let* c_out = input c_out in
      let* p = Bool.full_adder l r c_in in
      let s, c = of_pair p in
      assert_equal s s_out >* assert_equal c c_out

    let tests_full_adder =
      (* table from from https://en.wikipedia.org/wiki/Adder_(electronics)#Full_adder *)
      List.map
        (test ~valid:true ~name:"Bool.test_full_adder")
        Input.
          [
            test_full_adder
              (bool false)
              (bool false)
              (bool false)
              (bool false)
              (bool false);
            test_full_adder
              (bool false)
              (bool false)
              (bool true)
              (bool false)
              (bool true);
            test_full_adder
              (bool false)
              (bool true)
              (bool false)
              (bool false)
              (bool true);
            test_full_adder
              (bool false)
              (bool true)
              (bool true)
              (bool true)
              (bool false);
            test_full_adder
              (bool true)
              (bool false)
              (bool false)
              (bool false)
              (bool true);
            test_full_adder
              (bool true)
              (bool false)
              (bool true)
              (bool true)
              (bool false);
            test_full_adder
              (bool true)
              (bool true)
              (bool false)
              (bool true)
              (bool false);
            test_full_adder
              (bool true)
              (bool true)
              (bool true)
              (bool true)
              (bool true);
          ]
      @ List.map
          (test ~valid:false)
          Input.
            [
              test_full_adder
                (bool true)
                (bool true)
                (bool true)
                (bool true)
                (bool false);
              test_full_adder
                (bool false)
                (bool false)
                (bool false)
                (bool true)
                (bool false);
            ]

    let tests =
      tests_assert_true_and_false @ tests_equal @ tests_is_zero
      @ tests_is_not_zero @ tests_assert_bool @ tests_band @ tests_xor
      @ tests_bor @ tests_bnot @ tests_ifthenelse @ tests_swap
      @ tests_full_adder
  end

module List_ : Test =
functor
  (L : LIB)
  ->
  struct
    open L

    open Utils (L)

    let test_add_list x z () =
      let* x = input ~kind:`Public x in
      let* z = input z in
      let* y = Num.add_list x in
      let y = to_list [y] in
      let* z' = Num.add_list y in
      assert_equal z z'

    let i = Input.(list @@ List.init 4 (fun _ -> scalar S.one))

    let o = si 4

    let i2 = Input.(list @@ List.init 5 (fun _ -> scalar S.one))

    let o2 = si 5

    let tests_add_list =
      [
        test ~valid:true ~name:"List.test_add_list" @@ test_add_list i o;
        test ~valid:true ~name:"List.test_add_list" @@ test_add_list i2 o2;
        test ~valid:false @@ test_add_list i o2;
      ]

    let test_mul_list x z () =
      let* x = input ~kind:`Public x in
      let* z = input z in
      let* y = Num.mul_list x in
      let y = to_list [y] in
      let* z' = Num.mul_list y in
      assert_equal z z'

    let i = Input.(list @@ List.init 4 (fun _ -> scalar S.(one + one)))

    let o = si 16

    let i2 = Input.(list @@ List.init 5 (fun _ -> scalar S.mone))

    let o2 = si (-1)

    let tests_mul_list =
      [
        test ~valid:true ~name:"List.test_mul_list" @@ test_mul_list i o;
        test ~valid:true ~name:"List.test_mul_list" @@ test_mul_list i2 o2;
        test ~valid:false @@ test_mul_list i o2;
      ]

    let tests_equal =
      let i = Input.(list [si 10; si 2]) in
      let j = Input.(list [si 2; si 10]) in
      [
        test ~valid:true ~name:"List.test_equal" @@ test_equal i i;
        test ~valid:false @@ test_equal i j;
      ]

    let test_hd l z () =
      let* l = input ~kind:`Public l in
      let* z = input z in
      let* x = hd l in
      assert_equal x z

    let tests_hd =
      let i = Input.(list [si 2; si 20]) in
      let o = si 2 in
      [test ~valid:true ~name:"List.test_hd" @@ test_hd i o]

    let tests = tests_add_list @ tests_equal @ tests_hd @ tests_mul_list
  end

module Tuple : Test =
functor
  (L : LIB)
  ->
  struct
    open L

    open Utils (L)

    let tests_equal =
      let i = Input.(pair (si 0) (si 1)) in
      let j = Input.(pair (si 1) (si 0)) in
      [
        test ~valid:true ~name:"Tuple.test_equal" @@ test_equal i i;
        test ~valid:false @@ test_equal i j;
      ]

    let test_point x y z () =
      let* x = input ~kind:`Public x in
      let* y = input y in
      let* z = input z in
      let* z' = add2 x y in
      assert_equal z z'

    let i = Input.(pair (si 1) (si 0))

    let o = Input.(pair (si 2) (si 0))

    let tests_point =
      [
        test ~valid:true ~name:"Tuple.test_point" @@ test_point i i o;
        test ~valid:false @@ test_point i i i;
      ]

    let tests = tests_equal @ tests_point
  end

module Bytes : Test =
functor
  (L : LIB)
  ->
  struct
    open L
    open L.Bytes

    open Utils (L)

    let bytes_of_hex = Plompiler.Utils.bytes_of_hex

    let test_constant ~le b () =
      let b = bytes_of_hex b in
      let* o' = input @@ input_bytes ~le (bytes_of_hex "0f") in
      let* o = Bytes.constant ~le b in
      assert_equal o o'

    let tests_constant =
      let name = "Bytes.test_constant" in
      [
        test ~valid:true ~name @@ test_constant ~le:false "0f";
        test ~valid:true ~name @@ test_constant ~le:true "0f";
        test ~valid:false ~name @@ test_constant ~le:true "00";
      ]

    let test_add a b z () =
      let* a = input ~kind:`Public a in
      let* b = input b in
      let* z = input z in
      let* z' = add ~ignore_carry:true a b in
      assert_equal z z'

    let tests_add =
      let i = input_bytes ~le:false @@ Stdlib.Bytes.of_string "\008" in
      let o = input_bytes ~le:false @@ Stdlib.Bytes.of_string "\016" in
      [
        test ~valid:true ~name:"Bytes.test_add" @@ test_add i i o;
        test ~valid:false ~name:"Bytes.test_add" @@ test_add o o i;
      ]

    let test_xor a b z () =
      let* a = input ~kind:`Public a in
      let* b = input b in
      let* z = input z in
      let* z' = xor a b in
      assert_equal z z'

    let tests_xor =
      let i = input_bytes ~le:false @@ bytes_of_hex "08" in
      let o = input_bytes ~le:false @@ bytes_of_hex "00" in
      let i1 = input_bytes ~le:false @@ bytes_of_hex "510e527f" in
      let i2 = input_bytes ~le:false @@ bytes_of_hex "00000041" in
      let o1 = input_bytes ~le:false @@ bytes_of_hex "510e523e" in
      [
        test ~valid:true ~name:"Bytes.test_xor" @@ test_xor i i o;
        test ~valid:true ~name:"Bytes.blake" @@ test_xor i1 i2 o1;
        test ~valid:false ~name:"Bytes.test_xor" @@ test_xor o o i;
      ]

    let test_concat a b o () =
      let* a = input ~kind:`Public a in
      let* b = input b in
      let* o = input o in
      let o' = Bytes.concat [|a; b|] in
      assert_equal o o'

    let tests_concat =
      let a = input_bytes ~le:false @@ bytes_of_hex "08" in
      let b = input_bytes ~le:false @@ bytes_of_hex "01" in
      let o = input_bytes ~le:false @@ bytes_of_hex "0801" in
      [test ~valid:true ~name:"Bytes.test_concat" @@ test_concat a b o]

    let test_ifthenelse_bytes b l r z () =
      let* b = input ~kind:`Public b in
      let* l = input ~kind:`Public l in
      let* r = input ~kind:`Public r in
      let* z = input z in
      let* o = Bool.ifthenelse b l r in
      assert_equal z o

    let tests_ifthenelse_bytes =
      let l = input_bytes ~le:false @@ bytes_of_hex "01" in
      let r = input_bytes ~le:false @@ bytes_of_hex "00" in
      [
        test ~valid:true ~name:"Bytes.test_ifthenelse"
        @@ test_ifthenelse_bytes (Input.bool true) l r l;
        test ~valid:true ~name:"Bytes.test_ifthenelse"
        @@ test_ifthenelse_bytes (Input.bool false) l r r;
        test ~valid:false ~name:"Bytes.test_ifthenelse"
        @@ test_ifthenelse_bytes (Input.bool false) l r l;
      ]

    let test_rotate_left l i z () =
      let* l = input ~kind:`Public l in
      let* z = input z in
      let* o = rotate_left l i in
      assert_equal o z

    let tests_rotate_left =
      List.map
        (fun (i, a, b) ->
          let a = input_bytes ~le:false @@ Stdlib.Bytes.of_string a in
          let b = input_bytes ~le:false @@ Stdlib.Bytes.of_string b in
          test ~valid:true ~name:"Bytes.test_rotate_left"
          @@ test_rotate_left a i b)
        [
          (0, "\001", "\001");
          (1, "\001", "\002");
          (2, "\001", "\004");
          (3, "\001", "\008");
          (4, "\001", "\016");
          (5, "\001", "\032");
          (6, "\001", "\064");
          (7, "\001", "\128");
          (8, "\001", "\001");
          (0, "\000\001", "\000\001");
          (1, "\000\001", "\000\002");
          (2, "\000\001", "\000\004");
          (3, "\000\001", "\000\008");
          (4, "\000\001", "\000\016");
          (5, "\000\001", "\000\032");
          (6, "\000\001", "\000\064");
          (7, "\000\001", "\000\128");
          (8, "\000\001", "\001\000");
          (1, "\000\128", "\001\000");
          (1, "\128\000", "\000\001");
          (0, "\000\000\001", "\000\000\001");
          (1, "\000\000\001", "\000\000\002");
          (2, "\000\000\001", "\000\000\004");
          (3, "\000\000\001", "\000\000\008");
          (4, "\000\000\001", "\000\000\016");
          (5, "\000\000\001", "\000\000\032");
          (6, "\000\000\001", "\000\000\064");
          (7, "\000\000\001", "\000\000\128");
          (8, "\000\000\001", "\000\001\000");
          (1, "\000\000\128", "\000\001\000");
          (1, "\128\000\000", "\000\000\001");
        ]

    let test_rotate_right l i z () =
      let* l = input ~kind:`Public l in
      let* z = input z in
      let* o = rotate_right l i in
      assert_equal o z

    let tests_rotate_right =
      List.map
        (fun (i, a, b) ->
          let a = input_bytes ~le:false @@ Stdlib.Bytes.of_string a in
          let b = input_bytes ~le:false @@ Stdlib.Bytes.of_string b in
          test ~valid:true ~name:"Bytes.test_rotate_right"
          @@ test_rotate_right a i b)
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

    let test_not a z () =
      let* a = input ~kind:`Public a in
      let* z = input z in
      let* z' = not a in
      assert_equal z z'

    let tests_not =
      let i = Bytes.input_bytes ~le:false @@ bytes_of_hex "0F" in
      let o = Bytes.input_bytes ~le:false @@ bytes_of_hex "F0" in
      [
        test ~valid:true ~name:"Bytes.test_not" @@ test_not i o;
        test ~valid:true ~name:"Bytes.test_not" @@ test_not o i;
      ]

    let test_band a b o () =
      let* a = input ~kind:`Public a in
      let* b = input ~kind:`Public b in
      let* o = input o in
      let* o' = band a b in
      assert_equal o o'

    let tests_band =
      List.map
        (fun (valid, a, b, o) ->
          let a = Bytes.input_bytes ~le:false @@ bytes_of_hex a in
          let b = Bytes.input_bytes ~le:false @@ bytes_of_hex b in
          let o = Bytes.input_bytes ~le:false @@ bytes_of_hex o in
          test ~valid ~name:"Bytes.test_band" @@ test_band a b o)
        [
          (true, "00", "00", "00");
          (true, "0F", "00", "00");
          (true, "00", "0F", "00");
          (true, "0F", "0F", "0F");
          (true, "F0", "00", "00");
          (true, "00", "F0", "00");
          (true, "F0", "F0", "F0");
          (false, "0F", "00", "0F");
        ]

    let test_shift_left l i z () =
      let* l = input ~kind:`Public l in
      let* z = input z in
      let* o = shift_left l i in
      assert_equal o z

    let tests_shift_left =
      List.map
        (fun (i, a, b) ->
          let a = Bytes.input_bytes ~le:false @@ bytes_of_hex a in
          let b = Bytes.input_bytes ~le:false @@ bytes_of_hex b in
          test ~valid:true ~name:"Bytes.test_shift_left"
          @@ test_shift_left a i b)
        [
          (0, "B0", "B0");
          (1, "B0", "60");
          (2, "B0", "C0");
          (3, "B0", "80");
          (4, "B0", "00");
          (5, "B0", "00");
          (6, "B0", "00");
          (7, "B0", "00");
          (8, "B0", "00");
        ]

    let test_shift_right l i z () =
      let* l = input ~kind:`Public l in
      let* z = input z in
      let* o = shift_right l i in
      assert_equal o z

    let tests_shift_right =
      List.map
        (fun (i, a, b) ->
          let a = Bytes.input_bytes ~le:false @@ bytes_of_hex a in
          let b = Bytes.input_bytes ~le:false @@ bytes_of_hex b in
          test ~valid:true ~name:"Bytes.test_shift_right"
          @@ test_shift_right a i b)
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
      tests_constant @ tests_add @ tests_xor @ tests_concat
      @ tests_ifthenelse_bytes @ tests_rotate_left @ tests_rotate_right
      @ tests_not @ tests_band @ tests_shift_left @ tests_shift_right
  end

module Limbs : Test =
functor
  (L : LIB)
  ->
  struct
    open L

    open Utils (L)

    let bytes_of_hex = Plompiler.Utils.bytes_of_hex

    module Limbs4 = Limbs (struct
      let nb_bits = 4
    end)

    let test_constant4_bytes ~le b c () =
      let* lo' = input @@ Limbs4.input_bytes ~le c in
      let* o = Bytes.constant ~le b in
      let* lo = Limbs4.of_bool_list o in
      assert_equal lo lo'

    let test_constant4_limbs ~le b c () =
      let* lo' = input @@ Limbs4.input_bytes ~le c in
      let* o = Limbs4.constant ~le b in
      assert_equal o lo'

    let tests_constant str f =
      List.map
        (fun (valid, le, b, c) ->
          let name = "Limbs.test_constant" ^ str in
          let b = bytes_of_hex b in
          let c = bytes_of_hex c in
          test ~valid ~name @@ f ~le b c)
        [
          (true, false, "0f", "0f");
          (true, true, "0f", "0f");
          (true, true, "becd", "becd");
          (false, true, "00", "0f");
        ]

    let test_limbs_of_scalar ~total_nb_bits ~nb_bits sx expected () =
      let* x = input ~kind:`Public sx in
      let* expected = input expected in
      let* xlimbs = limbs_of_scalar ~total_nb_bits ~nb_bits x in
      assert_equal xlimbs expected

    let test_limbs_of_scalar_bytes ~total_nb_bits ~nb_bits sx expected () =
      let module Limbs_n = Limbs (struct
        let nb_bits = nb_bits
      end) in
      let* x = input ~kind:`Public sx in
      let* expected = input expected in
      let* xbits = bits_of_scalar ~nb_bits:total_nb_bits x in
      let* xlimbs = Limbs_n.of_bool_list xbits in
      assert_equal xlimbs expected

    let tests_limbs_of_scalar str f =
      List.map
        (fun (valid, sx, expected, total_nb_bits, nb_bits) ->
          let sx = si sx in
          let expected = Input.list @@ List.map si expected in
          test ~valid ~name:("Limbs.test_limbs_of_scalar" ^ str)
          @@ f ~total_nb_bits ~nb_bits sx expected)
        [
          (true, 9, [1; 0; 0; 1; 0], 5, 1);
          (true, 30, [0; 1; 1; 1; 1], 5, 1);
          (false, 3, [1; 1; 1; 0; 0], 5, 1);
          (true, 128, [0; 0; 0; 0; 0; 0; 0; 1], 8, 1);
          (true, 128, [0; 0; 0; 2], 8, 2);
          (true, 128, [0; 8], 8, 4);
          (true, 255, [15; 15], 8, 4);
          (true, 3225, [9; 9; 12], 12, 4);
        ]

    let tests =
      tests_constant "4_bytes" test_constant4_bytes
      @ tests_constant "4_limbs" test_constant4_limbs
      @ tests_limbs_of_scalar "_limbs" test_limbs_of_scalar
      @ tests_limbs_of_scalar "_bytes" test_limbs_of_scalar_bytes
  end

module ECC : Test =
functor
  (L : LIB)
  ->
  struct
    open L
    open L.Bool
    open L.Ecc

    open Utils (L)

    module W = Mec.Curve.Jubjub.AffineWeierstrass
    module E = Mec.Curve.Jubjub.AffineEdwards

    let test_weierstrass_add x y z () =
      let* x = input ~kind:`Public x in
      let* y = input y in
      let* z = input z in
      let* z' = weierstrass_add x y in
      let* b = equal z z' in
      assert_true b

    let tests_weierstrass_add =
      let s_of_base s = S.of_z (W.Base.to_z s) in
      let p1 = W.random () in
      let p2 = W.random () in
      let p1_i =
        Input.(
          pair
            (scalar @@ s_of_base @@ W.get_x_coordinate p1)
            (scalar @@ s_of_base @@ W.get_y_coordinate p1))
      in
      let p2_i =
        Input.(
          pair
            (scalar @@ s_of_base @@ W.get_x_coordinate p2)
            (scalar @@ s_of_base @@ W.get_y_coordinate p2))
      in
      let o =
        let p = W.add p1 p2 in
        Input.(
          pair
            (scalar @@ s_of_base @@ W.get_x_coordinate p)
            (scalar @@ s_of_base @@ W.get_y_coordinate p))
      in
      [
        test ~valid:true ~name:"ECC.test_weierstrass_add"
        @@ test_weierstrass_add p1_i p2_i o;
        test ~valid:false ~name:"ECC.test_weierstrass_add"
        @@ test_weierstrass_add p1_i p2_i p1_i;
      ]

    let test_edwards_add x y z () =
      let* x = input ~kind:`Public x in
      let* y = input y in
      let* z = input z in
      let* z' = edwards_add x y in
      let* b = equal z z' in
      assert_true b

    let tests_edwards_add =
      let s_of_base s = S.of_z (E.Base.to_z s) in
      let p1 = E.random () in
      let p2 = E.random () in
      let p1_i =
        Input.(
          pair
            (scalar @@ s_of_base @@ E.get_u_coordinate p1)
            (scalar @@ s_of_base @@ E.get_v_coordinate p1))
      in
      let p2_i =
        Input.(
          pair
            (scalar @@ s_of_base @@ E.get_u_coordinate p2)
            (scalar @@ s_of_base @@ E.get_v_coordinate p2))
      in
      let o =
        let p = E.add p1 p2 in
        Input.(
          pair
            (scalar @@ s_of_base @@ E.get_u_coordinate p)
            (scalar @@ s_of_base @@ E.get_v_coordinate p))
      in
      [
        test ~valid:true ~name:"ECC.test_edwards_add"
        @@ test_edwards_add p1_i p2_i o;
        test ~valid:false @@ test_edwards_add p1_i p2_i p1_i;
      ]

    let tests = tests_weierstrass_add @ tests_edwards_add
  end

module Rest : Test =
functor
  (L : LIB)
  ->
  struct
    open L
    open L.Bool
    open L.Num

    open Utils (L)

    let test_full_adder l r c_in o () =
      let* l = input ~kind:`Public l in
      let* r = input r in
      let* o = input o in
      let* c_in = input c_in in
      let* o' = full_adder l r c_in in
      assert_equal o o'

    let tests_full_adder =
      (* table from from https://en.wikipedia.org/wiki/Adder_(electronics)#Full_adder *)
      List.map
        (test ~valid:true ~name:"Rest.test_full_adder")
        Input.
          [
            test_full_adder
              (bool false)
              (bool false)
              (bool false)
              (pair (bool false) (bool false));
            test_full_adder
              (bool false)
              (bool false)
              (bool true)
              (pair (bool true) (bool false));
            test_full_adder
              (bool false)
              (bool true)
              (bool false)
              (pair (bool true) (bool false));
            test_full_adder
              (bool false)
              (bool true)
              (bool true)
              (pair (bool false) (bool true));
            test_full_adder
              (bool true)
              (bool false)
              (bool false)
              (pair (bool true) (bool false));
            test_full_adder
              (bool true)
              (bool false)
              (bool true)
              (pair (bool false) (bool true));
            test_full_adder
              (bool true)
              (bool true)
              (bool false)
              (pair (bool false) (bool true));
            test_full_adder
              (bool true)
              (bool true)
              (bool true)
              (pair (bool true) (bool true));
          ]

    let test_scalar_of_bytes b s () =
      let* b = input ~kind:`Public b in
      let* s = input s in
      let* s' = scalar_of_bytes b in
      assert_equal s s'

    let tests_scalar_of_bytes =
      List.map
        (fun s ->
          let bs = Bytes.input_bytes ~le:true @@ Stdlib.Bytes.of_string s in
          let scalar =
            Input.scalar (S.of_bytes_exn @@ Stdlib.Bytes.of_string s)
          in
          test ~valid:true ~name:"Rest.test_scalar_of_bytes"
          @@ test_scalar_of_bytes bs scalar)
        ["\x010101"]

    let tests = tests_full_adder @ tests_scalar_of_bytes
  end

let tests =
  let both =
    [
      ("Num", (module Num : Test));
      ("Bool", (module Bool : Test));
      ("List", (module List_ : Test));
      ("Rest", (module Rest : Test));
      ("Tuple", (module Tuple : Test));
      ("ECC", (module ECC : Test));
      ("Bytes", (module Bytes : Test));
      ("Limbs", (module Limbs : Test));
    ]
  in
  (* This test uses plonk and it is marked quick so that it
     is always run by the CI *)
  Alcotest.test_case
    ("Num" ^ " plonk")
    `Quick
    (to_test ~plonk:(module Plonk.Main_protocol) (module Num : Test))
  :: List.map (fun (name, m) -> Alcotest.test_case name `Quick (to_test m)) both
  @ List.map
      (fun (name, m) ->
        Alcotest.test_case
          (name ^ " plonk")
          `Slow
          (to_test ~plonk:(module Plonk.Main_protocol) m))
      both
