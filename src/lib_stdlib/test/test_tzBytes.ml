(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda,jp>                 *)
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
    _______

    Invocation: dune exec src/lib_stdlib/test/main.exe \
                  -- --file test_tzBytes.ml
    Subject: Bitwise byte operation and Bytes <=> nat/int conversion
*)

module Bitwise = struct
  module Reference : sig
    val logand : bytes -> bytes -> bytes

    val logor : bytes -> bytes -> bytes

    val logxor : bytes -> bytes -> bytes

    val lognot : bytes -> bytes

    val shift_left : bytes -> int -> bytes

    val shift_right : bytes -> int -> bytes
  end = struct
    (* Reference implementations *)

    open Bytes

    let logand a b =
      let len_a = length a in
      let len_b = length b in
      let len = min len_a len_b in
      let a = if len_a > len then sub a (len_a - len) len else a in
      let b = if len_b > len then sub b (len_b - len) len else b in
      init len @@ fun i ->
      Char.chr ((Char.code @@ get a i) land (Char.code @@ get b i))

    let bytes_logical_binop_for_or f a b =
      (* The shorter bytes is 0-padded on the left *)
      let len_a = length a in
      let len_b = length b in
      let len = max len_a len_b in
      let a = if len_a < len then cat (make (len - len_a) '\000') a else a in
      let b = if len_b < len then cat (make (len - len_b) '\000') b else b in
      init len @@ fun i ->
      Char.chr (f (Char.code @@ get a i) (Char.code @@ get b i))

    let logor = bytes_logical_binop_for_or ( lor )

    let logxor = bytes_logical_binop_for_or ( lxor )

    (* We cannot use lnot: [lnot 1 = -2], not [254] *)
    let lognot = map (fun c -> Char.(chr (code c lxor 255)))

    let shift_left bs n =
      if n < 0 then invalid_arg "shift_left" ;
      let nbytes = n / 8 in
      let nbits = n mod 8 in
      if nbits = 0 then cat bs @@ make nbytes '\000'
      else
        cat
          (init
             (length bs + 1)
             (fun i ->
               let c1 = if i = 0 then 0 else Char.code @@ get bs (i - 1) in
               let c2 = if i = length bs then 0 else Char.code @@ get bs i in
               Char.chr ((((c1 lsl 8) + c2) lsr (8 - nbits)) land 255)))
        @@ make nbytes '\000'

    let shift_right bs n =
      if n < 0 then invalid_arg "shift_left" ;
      let nbytes = n / 8 in
      let nbits = n mod 8 in
      if nbits = 0 then sub bs 0 (max 0 (length bs - nbytes))
      else
        init
          (max 0 (length bs - nbytes))
          (fun i ->
            let c1 = if i = 0 then 0 else Char.code @@ get bs (i - 1) in
            let c2 = Char.code @@ get bs i in
            Char.chr ((((c1 lsl 8) + c2) lsr nbits) land 255))
  end

  let unit_test_reference () =
    let open Reference in
    let ( ! ) s = Option.get @@ Hex.to_bytes (`Hex s) in
    let test_bin f a b r = assert (f !a !b = !r) in
    (* logand *)
    test_bin logand "ff00" "00ff" "0000" ;
    test_bin logand "ff00" "ff" "00" ;
    test_bin logand "00ff00" "ff00" "ff00" ;
    (* logor *)
    test_bin logor "f000" "0f" "f00f" ;
    test_bin logor "f000" "000f" "f00f" ;
    test_bin logor "00f000" "0f" "00f00f" ;
    (* logxor *)
    test_bin logxor "f0ff" "0f" "f0f0" ;
    test_bin logxor "f0ff" "000f" "f0f0" ;
    test_bin logxor "00f0ff" "0f" "00f0f0" ;
    (* lognot *)
    assert (lognot !"ff00" = !"00ff") ;
    (* shift_left *)
    assert (shift_left !"1234" 0 = !"1234") ;
    assert (shift_left !"1234" 1 = !"002468") ;
    assert (shift_left !"1234" 8 = !"123400") ;
    assert (shift_left !"001234" 1 = !"00002468") ;
    assert (shift_left !"" 1 = !"00") ;
    (* shift_right *)
    assert (shift_right !"1234" 0 = !"1234") ;
    assert (shift_right !"1234" 1 = !"091a") ;
    assert (shift_right !"1234" 8 = !"12") ;
    assert (shift_right !"123499" 9 = !"091a") ;
    assert (shift_right !"" 1 = !"")

  let random_bytes rng len =
    Bytes.init len (fun _ -> Char.chr @@ Random.State.int rng 256)

  let test_bin rng ref_f f len_a len_b =
    let a = random_bytes rng len_a in
    let b = random_bytes rng len_b in
    if ref_f a b <> f a b then
      Format.eprintf
        "r : %S@.f : %S@."
        (Bytes.to_string (ref_f a b))
        (Bytes.to_string (f a b)) ;
    assert (ref_f a b = f a b)

  let test_uni rng f ref_f len_a =
    let a = random_bytes rng len_a in
    assert (f a = ref_f a)

  let test_shift rng ref_f f len_a =
    for n = 0 to 256 do
      let a = random_bytes rng len_a in
      if ref_f a n <> f a n then (
        Format.eprintf "a : %S n : %d@." (Bytes.to_string a) n ;
        Format.eprintf "r : %S@." (Bytes.to_string (ref_f a n)) ;
        Format.eprintf "f : %S@." (Bytes.to_string (f a n))) ;
      assert (ref_f a n = f a n)
    done

  (* We once had a bug in tzBytes_c.c, which converted OCaml 63bit int to
     C 32bit int using Int_val(vn) by mistake and caused overflow.

     This test checks the parameter no longer overflows in C.
  *)
  let test_left_shift_huge () =
    match Sys.word_size with
    | 32 -> ()
    | 64 ->
        let n = (1 lsl 31) + 1 in
        assert (Bytes.length (TzBytes.shift_left Bytes.empty n) = (n + 7) / 8)
    | _ -> assert false

  let test_shift_zero () =
    (* shift_* returns a copy even if the bits = 0 *)
    let b = Bytes.of_string "hello" in
    let bl = TzBytes.shift_left b 0 in
    assert (b = bl && b != bl) ;
    let br = TzBytes.shift_right b 0 in
    assert (b = br && b != br)

  (* Test all the combinations (len_a,len_b) of {(0,0),..,(256,256)} *)
  let test rng () =
    for len_a = 0 to 256 do
      for len_b = 0 to 256 do
        test_bin rng Reference.logand TzBytes.logand len_a len_b ;
        test_bin rng Reference.logor TzBytes.logor len_a len_b ;
        test_bin rng Reference.logxor TzBytes.logxor len_a len_b
      done ;
      test_uni rng Reference.lognot TzBytes.lognot len_a ;
      test_shift rng Reference.shift_left TzBytes.shift_left len_a ;
      test_shift rng Reference.shift_right TzBytes.shift_right len_a
    done

  (* Test some huge sizes *)
  let test_huge rng () =
    for len_a = 1024 to 1088 do
      for len_b = 1024 to 1088 do
        test_bin rng Reference.logand TzBytes.logand len_a len_b ;
        test_bin rng Reference.logor TzBytes.logor len_a len_b ;
        test_bin rng Reference.logxor TzBytes.logxor len_a len_b
      done ;
      test_uni rng Reference.lognot TzBytes.lognot len_a ;
      test_shift rng Reference.shift_left TzBytes.shift_left len_a ;
      test_shift rng Reference.shift_right TzBytes.shift_right len_a
    done

  (* Check the original bytes are never modified *)
  let test_functional_binary rng f len_a len_b =
    let a = random_bytes rng len_a in
    let b = random_bytes rng len_b in
    let a' = Bytes.copy a in
    let b' = Bytes.copy b in
    ignore (f a b) ;
    assert (a = a' && b = b')

  (* Check the original bytes are never modified *)
  let test_functional_unary rng f len_a =
    let a = random_bytes rng len_a in
    let a' = Bytes.copy a in
    ignore (f a) ;
    assert (a = a')

  let test_functional rng () =
    let len_a = 100 in
    let len_b = 100 in
    (* We do not test LSL and LSR since we are sure they never overwrite
       the inputs. *)
    test_functional_binary rng TzBytes.logand len_a len_b ;
    test_functional_binary rng TzBytes.logor len_a len_b ;
    test_functional_binary rng TzBytes.logxor len_a len_b ;
    test_functional_unary rng TzBytes.lognot len_a

  let tests get_rng =
    [
      ("bitwise_reference", [("unit_test", `Quick, unit_test_reference)]);
      ("bitwise_small", [("random_contents_test", `Quick, test (get_rng ()))]);
      ( "bitwise_huge",
        [("random_contents_test", `Quick, test_huge (get_rng ()))] );
      ("bitwise_very_huge", [("int64_shift", `Quick, test_left_shift_huge)]);
      ("bitwise_test_shift_zero", [("test_shift_zero", `Quick, test_shift_zero)]);
      ( "bitwise_test_functional",
        [("test_functional", `Quick, test_functional (get_rng ()))] );
    ]
end

let () =
  let seed =
    try int_of_string @@ Sys.getenv "TEST_SEED"
    with Not_found ->
      let rng = Random.State.make_self_init () in
      Random.State.bits rng
  in
  Format.eprintf "TEST_SEED=%d@." seed ;
  (* To run the same test for the same seed, even if not all the tests are selected *)
  let get_rng () = Random.State.make [|seed|] in
  Alcotest.run ~__FILE__ "TzBytes" (Bitwise.tests get_rng)
