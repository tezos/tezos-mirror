(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda.com>                *)
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
    Component:    pbt for bytes <=> nat/int conversions
    Invocation:   dune exec src/proto_019_PtParisA/lib_protocol/test/pbt/main.exe \
                  -- --file test_bytes_conversion.ml
    Subject:      Test the conversions between bytes and int/nat
*)

let failwith = Stdlib.failwith

open Protocol
open Script_int
open Script_bytes

let gen_n =
  let open QCheck2.Gen in
  let* n1 = int in
  let+ n2 = int in
  Z.(abs (of_int n1 * of_int n2))

let gen_z =
  let open QCheck2.Gen in
  let* n1 = int in
  let+ n2 = int in
  Z.(of_int n1 * of_int n2)

let gen_bytes =
  let open QCheck2.Gen in
  bytes_size small_nat

let test_bytes_nat_conversion_unit () =
  let test z h =
    (* nat => bytes *)
    let (`Hex h') = Hex.of_bytes (bytes_of_nat_be (abs (of_zint z))) in
    if h <> h' then
      failwith (Format.asprintf "%a => %s <> %s" Z.pp_print z h' h) ;
    (* bytes => nat *)
    let z' = to_zint @@ nat_of_bytes_be (Hex.to_bytes_exn (`Hex h)) in
    if Z.Compare.(z <> z') then
      failwith (Format.asprintf "%a <> %a <= %s" Z.pp_print z Z.pp_print z' h) ;
    (* "00" ^ bytes => nat *)
    let h'' = "00" ^ h in
    let z'' = to_zint @@ nat_of_bytes_be (Hex.to_bytes_exn (`Hex h'')) in
    if Z.Compare.(z <> z'') then
      failwith (Format.asprintf "%a <> %a <= %s" Z.pp_print z Z.pp_print z' h)
  in
  let test' h = test (Z.of_string ("0x" ^ h)) h in
  test (Z.of_int 0) "" ;
  test' "123456" ;
  test' "010000000000000000"

(* Tests of nat => bytes *)
let test_bytes_of_nat_random () =
  let gen =
    let open QCheck2.Gen in
    let* n = gen_n in
    let+ leading_bytes = small_nat in
    (n, leading_bytes)
  in
  QCheck_alcotest.to_alcotest
  @@ QCheck2.Test.make ~name:"bytes_of_nat" gen
  @@ fun (n, leading_zeros) ->
  let bytes = bytes_of_nat_be @@ abs (of_zint n) in
  (* [bytes_of_nat] encodes any [nat] to the shortest representation in [bytes],
     without leading zeros. *)
  if Bytes.length bytes > 0 then assert (Bytes.get bytes 0 <> '\000') ;
  (* [nat_of_bytes @@ bytes_of_nat n = n] *)
  Z.Compare.(to_zint (nat_of_bytes_be bytes) = n)
  (* Leading zero chars do not affect the decoding *)
  &&
  let leading_zeros = Bytes.make leading_zeros '\000' in
  Z.Compare.(to_zint (nat_of_bytes_be (Bytes.cat leading_zeros bytes)) = n)

(* Tests of bytes => nat *)
let test_nat_of_bytes_random () =
  QCheck_alcotest.to_alcotest
  @@ QCheck2.Test.make ~name:"nat_of_bytes" gen_bytes
  @@ fun b ->
  (* [nat_of_bytes] decodes any [bytes] to a [nat] *)
  let n = nat_of_bytes_be b in
  (* [bytes_of_nat] encodes the [nat] back to the original [bytes]
     but without its leading zeros.
  *)
  Z.Compare.(to_zint n >= Z.zero)
  &&
  let b' = bytes_of_nat_be n in
  let diff = Bytes.length b - Bytes.length b' in
  let leading_zeros = Bytes.make diff '\000' in
  b = Bytes.cat leading_zeros b'

let test_bytes_int_conversion_unit () =
  let test z h =
    let z = Z.of_string z in
    (* int => bytes *)
    let (`Hex h') = Hex.of_bytes (bytes_of_int_be (of_zint z)) in
    if h <> h' then
      failwith (Format.asprintf "%a => %s <> %s" Z.pp_print z h' h) ;
    (* bytes => int *)
    let z' = to_zint @@ int_of_bytes_be (Hex.to_bytes_exn (`Hex h)) in
    if Z.Compare.(z <> z') then
      failwith (Format.asprintf "%a <> %a <= %s" Z.pp_print z Z.pp_print z' h) ;
    (* ("00"|"ff") ^ bytes => int
       Adding 00 or ff prefixes (00 for positive and ff for negative ints)
       must not change the decoding. *)
    let h'' = if Z.Compare.(z < Z.zero) then "ff" ^ h else "00" ^ h in
    let z'' = to_zint @@ int_of_bytes_be (Hex.to_bytes_exn (`Hex h'')) in
    if Z.Compare.(z <> z'') then
      failwith (Format.asprintf "%a <> %a <= %s" Z.pp_print z Z.pp_print z' h)
  in
  test "0" "" ;
  test "1" "01" ;
  test "-1" "ff" ;
  test "127" "7f" ;
  test "-128" "80" ;
  test "128" "0080" ;
  test "-129" "ff7f" ;
  test "0x8000" "008000" ;
  test "-33024" "ff7f00" ;
  test "0x010000000000000000" "010000000000000000" ;
  test "-0x010000000000000000" "ff0000000000000000" ;
  test "0xcd9e7dbee9425ffc" "00cd9e7dbee9425ffc" (* once failed due to a bug *)

(* Tests of int => bytes *)
let test_bytes_of_int_random () =
  let gen =
    let open QCheck2.Gen in
    let* z = gen_z in
    let+ leading_bytes = small_nat in
    (z, leading_bytes)
  in
  QCheck_alcotest.to_alcotest
  @@ QCheck2.Test.make ~name:"bytes_of_int" gen
  @@ fun (z, leading_bytes) ->
  (* [bytes_of_int] must encode any [int] to [bytes]. *)
  let bytes = bytes_of_int_be @@ of_zint z in
  (* [bytes_of_int] must return the shortest encoding: at most 1 char of zero
     or '\255's at the head. *)
  (if Bytes.length bytes >= 2 then
   match (Bytes.get bytes 0, Bytes.get bytes 1) with
   | '\000', '\000' | '\255', '\255' -> assert false
   | _ -> ()) ;
  (* [int_of_bytes @@ bytes_of_int z = z] *)
  (let z' = to_zint @@ int_of_bytes_be bytes in
   Z.Compare.(z = z'))
  (* [int_of_bytes] must ignore the leading zeros for 0 and positive ints
       and '\255's for negatives *)
  &&
  let leading_bytes =
    Bytes.make leading_bytes (if Z.Compare.(z < Z.zero) then '\255' else '\000')
  in
  Z.Compare.(to_zint @@ int_of_bytes_be (Bytes.cat leading_bytes bytes) = z)

(* Tests of bytes => int *)
let test_int_of_bytes_random () =
  QCheck_alcotest.to_alcotest
  @@ QCheck2.Test.make ~name:"int_of_bytes" gen_bytes
  @@ fun b ->
  (* [int_of_bytes] decodes any [bytes] to a [int] *)
  let i = int_of_bytes_be b in
  (* [bytes_of_int] must encode the [int] back to the original [bytes]
       but without its leading zeros for 0 and positive [int]s and '\255'
       for negatives. *)
  let b' = bytes_of_int_be i in
  let diff = Bytes.length b - Bytes.length b' in
  let leading_bytes =
    Bytes.make diff (if Z.Compare.(to_zint i < Z.zero) then '\255' else '\000')
  in
  b = Bytes.cat leading_bytes b'

let tests =
  [
    ( "bytes_nat_conv",
      [
        ("unit", `Quick, test_bytes_nat_conversion_unit);
        test_bytes_of_nat_random ();
        test_nat_of_bytes_random ();
      ] );
    ( "bytes_int_conv",
      [
        ("unit", `Quick, test_bytes_int_conversion_unit);
        test_bytes_of_int_random ();
        test_int_of_bytes_random ();
      ] );
  ]

let () =
  Alcotest.run
    ~__FILE__
    (Protocol.name ^ ": bytes and int/nat conversion ")
    tests
