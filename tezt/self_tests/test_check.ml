(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let register () =
  ( Test.register ~__FILE__ ~title:"Check.(=)" ~tags:["check"; "eq"] @@ fun () ->
    let error_msg = "expected %L = %R" in
    Check.((() = ()) unit) ~error_msg ;
    Check.((true = true) bool) ~error_msg ;
    Check.(('a' = 'a') char) ~error_msg ;
    Check.((42 = 42) int) ~error_msg ;
    Check.((0xFFFF_FFFFl = 0xFFFF_FFFFl) int32) ~error_msg ;
    Check.((0xFFFF_FFFF_FFFF_FFFFL = 0xFFFF_FFFF_FFFF_FFFFL) int64) ~error_msg ;
    Check.((42. = 42.) float) ~error_msg ;
    Check.((42. = 42.09) (float_epsilon 0.1)) ~error_msg ;
    Check.(("hello" = "hello") string) ~error_msg ;
    Check.((Some 42 = Some 42) (option int)) ~error_msg ;
    Check.(([1; 2; 3] = [1; 2; 3]) (list int)) ~error_msg ;
    Check.(([|1; 2; 3|] = [|1; 2; 3|]) (array int)) ~error_msg ;
    Check.(((1, 2.) = (1, 2.)) (tuple2 int float)) ~error_msg ;
    Check.(((1, 2., ()) = (1, 2., ())) (tuple3 int float unit)) ~error_msg ;
    Check.(
      ((1, 2., (), (true, "")) = (1, 2., (), (true, "")))
        (tuple4 int float unit (tuple2 bool string)))
      ~error_msg ;
    Check.((42 = 42) (convert float_of_int float)) ~error_msg ;
    unit ) ;

  ( Test.register ~__FILE__ ~title:"Check.(<>)" ~tags:["check"; "neq"]
  @@ fun () ->
    let error_msg = "expected %L <> %R" in
    Check.((true <> false) bool) ~error_msg ;
    Check.(('a' <> '0') char) ~error_msg ;
    Check.((42 <> 40) int) ~error_msg ;
    Check.((0xFFFF_FFFFl <> 0xFFFF_FFF0l) int32) ~error_msg ;
    Check.((0xFFFF_FFFF_FFFF_FFFFL <> 0xFFFF_FFFF_FFFF_FFF0L) int64) ~error_msg ;
    Check.((42. <> 42.1) float) ~error_msg ;
    Check.((42. <> 42.11) (float_epsilon 0.1)) ~error_msg ;
    Check.(("hello" <> "ola") string) ~error_msg ;
    Check.((Some 42 <> Some 11) (option int)) ~error_msg ;
    Check.(([1; 2; 3] <> [1; 2]) (list int)) ~error_msg ;
    Check.(([|1; 2; 3|] <> [|1; 2; 4|]) (array int)) ~error_msg ;
    Check.(((1, 2.) <> (1, 3.)) (tuple2 int float)) ~error_msg ;
    Check.(((1, 2., ()) <> (0, 2., ())) (tuple3 int float unit)) ~error_msg ;
    Check.(
      ((1, 2., (), (true, "")) <> (1, 2., (), (false, "")))
        (tuple4 int float unit (tuple2 bool string)))
      ~error_msg ;
    Check.((42 <> 40) (convert float_of_int float)) ~error_msg ;
    unit ) ;

  ( Test.register ~__FILE__ ~title:"Check.(<)" ~tags:["check"; "lt"] @@ fun () ->
    let error_msg = "expected %L < %R" in
    Check.((false < true) bool) ~error_msg ;
    Check.(('a' < 'z') char) ~error_msg ;
    Check.(('\000' < 'z') char) ~error_msg ;
    Check.(('9' < '\255') char) ~error_msg ;
    Check.((17 < 18) int) ~error_msg ;
    Check.((17. < 18.) float) ~error_msg ;
    Check.((17. < 18.) (float_epsilon 0.1)) ~error_msg ;
    Check.(("abc" < "abcdef") string) ~error_msg ;
    Check.(([1; 2] < [1; 2; 3]) (list int)) ~error_msg ;
    Check.(([1; 2; 3] < [1; 3]) (list int)) ~error_msg ;
    Check.(([|1; 2|] < [|1; 2; 3|]) (array int)) ~error_msg ;
    Check.(([|1; 2; 3|] < [|1; 3|]) (array int)) ~error_msg ;
    Check.(((1, "") < (1, "x")) (tuple2 int string)) ~error_msg ;
    Check.(((1, "") < (2, "")) (tuple2 int string)) ~error_msg ;
    unit ) ;

  ( Test.register ~__FILE__ ~title:"Check.(<=)" ~tags:["check"; "le"]
  @@ fun () ->
    let error_msg = "expected %L <= %R" in
    Check.((false <= true) bool) ~error_msg ;
    Check.((false <= false) bool) ~error_msg ;
    Check.(('a' <= 'z') char) ~error_msg ;
    Check.(('a' <= 'a') char) ~error_msg ;
    Check.(('\000' <= 'z') char) ~error_msg ;
    Check.(('9' <= '\255') char) ~error_msg ;
    Check.((17 <= 18) int) ~error_msg ;
    Check.((17 <= 17) int) ~error_msg ;
    Check.((17. <= 18.) float) ~error_msg ;
    Check.((17. <= 17.) float) ~error_msg ;
    Check.((17. <= 18.) (float_epsilon 0.1)) ~error_msg ;
    Check.((17. <= 17.09) (float_epsilon 0.1)) ~error_msg ;
    Check.((17.09 <= 17.) (float_epsilon 0.1)) ~error_msg ;
    Check.(("abc" <= "abcdef") string) ~error_msg ;
    Check.(("abc" <= "abc") string) ~error_msg ;
    Check.(([1; 2] <= [1; 2; 3]) (list int)) ~error_msg ;
    Check.(([1; 2; 3] <= [1; 3]) (list int)) ~error_msg ;
    Check.(([1; 2] <= [1; 2]) (list int)) ~error_msg ;
    Check.(([|1; 2|] <= [|1; 2; 3|]) (array int)) ~error_msg ;
    Check.(([|1; 2; 3|] <= [|1; 3|]) (array int)) ~error_msg ;
    Check.(([|1; 2|] <= [|1; 2|]) (array int)) ~error_msg ;
    Check.(((1, "") <= (1, "x")) (tuple2 int string)) ~error_msg ;
    Check.(((1, "") <= (2, "")) (tuple2 int string)) ~error_msg ;
    Check.(((1, "") <= (1, "")) (tuple2 int string)) ~error_msg ;
    unit ) ;

  ( Test.register ~__FILE__ ~title:"Check.(>)" ~tags:["check"; "gt"] @@ fun () ->
    let error_msg = "expected %L > %R" in
    Check.((true > false) bool) ~error_msg ;
    Check.(('z' > 'a') char) ~error_msg ;
    Check.(('z' > '\000') char) ~error_msg ;
    Check.(('\255' > '9') char) ~error_msg ;
    Check.((19 > 18) int) ~error_msg ;
    Check.((19. > 18.) float) ~error_msg ;
    Check.((19. > 18.) (float_epsilon 0.1)) ~error_msg ;
    Check.(("ac" > "abcdef") string) ~error_msg ;
    Check.(([1; 2; 3; 4] > [1; 2; 3]) (list int)) ~error_msg ;
    Check.(([2] > [1; 3]) (list int)) ~error_msg ;
    Check.(([|1; 3|] > [|1; 2; 3|]) (array int)) ~error_msg ;
    Check.(([|1; 3; 4|] > [|1; 3|]) (array int)) ~error_msg ;
    Check.(((1, "x") > (1, "")) (tuple2 int string)) ~error_msg ;
    Check.(((2, "") > (1, "")) (tuple2 int string)) ~error_msg ;
    unit ) ;

  ( Test.register ~__FILE__ ~title:"Check.(>=)" ~tags:["check"; "ge"]
  @@ fun () ->
    let error_msg = "expected %L >= %R" in
    Check.((true >= false) bool) ~error_msg ;
    Check.((false >= false) bool) ~error_msg ;
    Check.(('z' >= 'a') char) ~error_msg ;
    Check.(('a' >= 'a') char) ~error_msg ;
    Check.(('z' >= '\000') char) ~error_msg ;
    Check.(('\255' >= '9') char) ~error_msg ;
    Check.((19 >= 18) int) ~error_msg ;
    Check.((19 >= 19) int) ~error_msg ;
    Check.((19. >= 18.) float) ~error_msg ;
    Check.((19. >= 19.) float) ~error_msg ;
    Check.((19. >= 18.) (float_epsilon 0.1)) ~error_msg ;
    Check.((19.05 >= 19.) (float_epsilon 0.1)) ~error_msg ;
    Check.((19. >= 19.05) (float_epsilon 0.1)) ~error_msg ;
    Check.((19. >= 19.) (float_epsilon 0.1)) ~error_msg ;
    Check.(("ac" >= "abcdef") string) ~error_msg ;
    Check.(("ac" >= "ac") string) ~error_msg ;
    Check.(([1; 2; 3; 4] >= [1; 2; 3]) (list int)) ~error_msg ;
    Check.(([2] >= [1; 3]) (list int)) ~error_msg ;
    Check.(([2] >= [2]) (list int)) ~error_msg ;
    Check.(([|1; 3|] >= [|1; 2; 3|]) (array int)) ~error_msg ;
    Check.(([|1; 3; 4|] >= [|1; 3|]) (array int)) ~error_msg ;
    Check.(([|1; 3; 4|] >= [|1; 3; 4|]) (array int)) ~error_msg ;
    Check.(((1, "x") >= (1, "")) (tuple2 int string)) ~error_msg ;
    Check.(((2, "") >= (1, "")) (tuple2 int string)) ~error_msg ;
    Check.(((1, "") >= (1, "")) (tuple2 int string)) ~error_msg ;
    unit ) ;

  ( Test.register ~__FILE__ ~title:"Check.(=~)" ~tags:["check"; "like"]
  @@ fun () ->
    Check.("abcd42" =~ rex "[0-9]") ~error_msg:"expected %L =~ %R" ;
    unit ) ;

  Test.register ~__FILE__ ~title:"Check.(=~!)" ~tags:["check"; "not_like"]
  @@ fun () ->
  Check.("abcd" =~! rex "[0-9]") ~error_msg:"expected %L =~! %R" ;
  unit
