(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
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
open Utils

let () = Random.self_init ()

(** The test vectors are generated using
    https://github.com/dannywillems/ocaml-ff *)
let test_vectors =
  [
    "5241434266765085153989819426158356963249585137477420674959011812945457865191";
    "10839440052692226066497714164180551800338639216929046788248680350103009908352";
    "45771516566988367809715142190959127910391288669516577059039340716912455457131";
    "12909915968096385929046240252673624834885730199746273136167032454235900707423";
    "9906806778085203695146840231942453635945512651510460213691437498308396392030";
    "20451006147593515828371694915490427948041026610654337997907355913265840025855";
    "22753274685202779061111872324861161292260930710591061598808549358079414450472";
    "12823588949385074189879212809942339506958509313775057573450243545256259992541";
    "3453";
    "323580923485092809298430986453";
    "984305293863456098093285";
    "235234634090909863456";
    "24352346534563452436524356";
    "3836944629596737352";
    "65363576374567456780984059630856836098740965874094860978";
    "546574608450909809809809824360345639808560937";
  ]

let random_z () =
  let size = 1 + Random.int Bls12_381.Fr.size_in_bytes in
  let r = generate_random_bytes size in
  Z.erem (Z.of_bits (Bytes.to_string r)) Bls12_381.Fr.order

module Tests = Ff_pbt.MakeAll (Bls12_381.Fr)

module Memory = struct
  let test_copy () =
    let x = Bls12_381.Fr.random () in
    let y = Bls12_381.Fr.copy x in
    assert (Bls12_381.Fr.eq x y)

  let test_size_in_memory () = assert (Bls12_381.Fr.size_in_memory = 48)

  let get_tests () =
    let txt = "Memory" in
    let open Alcotest in
    ( txt,
      [
        test_case "copy" `Quick (Utils.repeat 100 test_copy);
        test_case "size in memory" `Quick test_size_in_memory;
      ] )
end

module InplaceOperations = struct
  let test_add_inplace () =
    let x = Bls12_381.Fr.random () in
    let y = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.add x y in
    let res2 = Bls12_381.Fr.(copy one) in
    Bls12_381.Fr.add_inplace res2 x y ;
    assert (Bls12_381.Fr.eq res res2)

  let test_double_inplace () =
    let x = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.double x in
    let res2 = Bls12_381.Fr.(copy one) in
    Bls12_381.Fr.double_inplace res2 x ;
    assert (Bls12_381.Fr.eq res res2)

  let test_square_inplace () =
    let x = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.square x in
    let res2 = Bls12_381.Fr.(copy one) in
    Bls12_381.Fr.square_inplace res2 x ;
    assert (Bls12_381.Fr.eq res res2)

  let test_negate_inplace () =
    let x = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.negate x in
    let res2 = Bls12_381.Fr.(copy one) in
    Bls12_381.Fr.negate_inplace res2 x ;
    assert (Bls12_381.Fr.eq res res2)

  let test_inverse_inplace () =
    let x = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.inverse_exn x in
    let res2 = Bls12_381.Fr.(copy one) in
    Bls12_381.Fr.inverse_exn_inplace res2 x ;
    assert (Bls12_381.Fr.eq res res2)

  let test_add_inplace_with_same_value () =
    let x = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.add x x in
    let res2 = Bls12_381.Fr.(copy one) in
    Bls12_381.Fr.add_inplace res2 x x ;
    assert (Bls12_381.Fr.eq res res2)

  let test_sub_inplace () =
    let x = Bls12_381.Fr.random () in
    let y = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.sub x y in
    let res2 = Bls12_381.Fr.(copy one) in
    Bls12_381.Fr.sub_inplace res2 x y ;
    assert (Bls12_381.Fr.eq res res2)

  let test_sub_inplace_with_same_value () =
    let x = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.sub x x in
    let res2 = Bls12_381.Fr.(copy one) in
    Bls12_381.Fr.sub_inplace res2 x x ;
    assert (Bls12_381.Fr.eq res res2)

  let test_sub_inplace_with_same_value_as_output () =
    let x = Bls12_381.Fr.random () in
    let y = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.sub x y in
    Bls12_381.Fr.sub_inplace x x y ;
    assert (Bls12_381.Fr.eq x res)

  let test_mul_inplace () =
    let x = Bls12_381.Fr.random () in
    let y = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.mul x y in
    let res2 = Bls12_381.Fr.(copy one) in
    Bls12_381.Fr.mul_inplace res2 x y ;
    assert (Bls12_381.Fr.eq res res2)

  let test_mul_inplace_with_same_value () =
    let x = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.mul x x in
    let res2 = Bls12_381.Fr.(copy one) in
    Bls12_381.Fr.mul_inplace res2 x x ;
    assert (Bls12_381.Fr.eq res res2)

  let test_add_inplace_with_same_value_as_output () =
    let x = Bls12_381.Fr.random () in
    let y = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.add x y in
    Bls12_381.Fr.add_inplace x x y ;
    assert (Bls12_381.Fr.eq x res)

  let test_mul_inplace_with_same_value_as_output () =
    let x = Bls12_381.Fr.random () in
    let y = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.mul x y in
    Bls12_381.Fr.mul_inplace x x y ;
    assert (Bls12_381.Fr.eq x res)

  let test_inverse_inplace_with_same_value_as_output () =
    let x = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.inverse_exn x in
    Bls12_381.Fr.inverse_exn_inplace x x ;
    assert (Bls12_381.Fr.eq x res)

  let test_negate_inplace_with_same_value_as_output () =
    let x = Bls12_381.Fr.random () in
    let res = Bls12_381.Fr.negate x in
    Bls12_381.Fr.negate_inplace x x ;
    assert (Bls12_381.Fr.eq x res)

  let get_tests () =
    let txt = "Inplace operations" in
    let open Alcotest in
    ( txt,
      [
        test_case "add" `Quick (Utils.repeat 100 test_add_inplace);
        test_case
          "add with same value"
          `Quick
          (Utils.repeat 100 test_add_inplace_with_same_value);
        test_case "square" `Quick (Utils.repeat 100 test_square_inplace);
        test_case "negate" `Quick (Utils.repeat 100 test_negate_inplace);
        test_case "double" `Quick (Utils.repeat 100 test_double_inplace);
        test_case "inverse" `Quick (Utils.repeat 100 test_inverse_inplace);
        test_case
          "negate with same value as output"
          `Quick
          (Utils.repeat 100 test_negate_inplace_with_same_value_as_output);
        test_case
          "inverse_exn with same value as output"
          `Quick
          (Utils.repeat 100 test_inverse_inplace_with_same_value_as_output);
        test_case
          "sub with same value"
          `Quick
          (Utils.repeat 100 test_sub_inplace_with_same_value);
        test_case
          "sub with same value as output"
          `Quick
          (Utils.repeat 100 test_sub_inplace_with_same_value_as_output);
        test_case
          "mul with same value"
          `Quick
          (Utils.repeat 100 test_mul_inplace_with_same_value);
        test_case
          "add with same value as output"
          `Quick
          (Utils.repeat 100 test_add_inplace_with_same_value_as_output);
        test_case
          "mul with same value as output"
          `Quick
          (Utils.repeat 100 test_mul_inplace_with_same_value_as_output);
        test_case "sub" `Quick (Utils.repeat 100 test_sub_inplace);
        test_case "mul" `Quick (Utils.repeat 100 test_mul_inplace);
      ] )
end

module StringRepresentation = struct
  let test_to_string_one () =
    assert (String.equal "1" (Bls12_381.Fr.to_string Bls12_381.Fr.one))

  let test_to_string_zero () =
    assert (String.equal "0" (Bls12_381.Fr.to_string Bls12_381.Fr.zero))

  let test_of_string_with_of_z () =
    List.iter
      (fun x ->
        assert (
          Bls12_381.Fr.eq
            (Bls12_381.Fr.of_string x)
            (Bls12_381.Fr.of_z (Z.of_string x))))
      test_vectors

  let test_of_string_to_string_consistency () =
    List.iter
      (fun x ->
        assert (
          String.equal (Bls12_381.Fr.to_string (Bls12_381.Fr.of_string x)) x))
      test_vectors

  let test_of_string_higher_than_the_modulus () =
    let x = random_z () in
    let x_str = Z.to_string x in
    let x_plus_order = Z.(add x Bls12_381.Fr.order) in
    let x_plus_order_str = Z.to_string x_plus_order in
    assert (Bls12_381.Fr.(eq (of_string x_str) (of_string x_plus_order_str)))

  let get_tests () =
    let open Alcotest in
    ( "String representation",
      [
        test_case "one" `Quick test_to_string_one;
        test_case
          "consistency of_string with of_z with test vectors"
          `Quick
          test_of_string_with_of_z;
        test_case
          "of_string accepts elements higher than the modulus"
          `Quick
          test_of_string_higher_than_the_modulus;
        test_case
          "consistency of_string to_string with test vectors"
          `Quick
          test_of_string_to_string_consistency;
        test_case "zero" `Quick test_to_string_zero;
      ] )
end

module ZRepresentation = struct
  let test_of_z_zero () =
    assert (Bls12_381.Fr.eq Bls12_381.Fr.zero (Bls12_381.Fr.of_z Z.zero))

  let test_of_z_one () =
    assert (
      Bls12_381.Fr.eq Bls12_381.Fr.one (Bls12_381.Fr.of_z (Z.of_string "1")))

  let test_random_of_z_and_to_z () =
    let x = Bls12_381.Fr.random () in
    assert (Bls12_381.Fr.eq x (Bls12_381.Fr.of_z (Bls12_381.Fr.to_z x)))

  let test_random_to_z_and_of_z () =
    let x = random_z () in
    assert (Z.equal (Bls12_381.Fr.to_z (Bls12_381.Fr.of_z x)) x)

  let test_random_of_z_higher_than_modulo () =
    (* Verify of_z uses the modulo of the parameter (and therefore accepts value
       higher than the order) *)
    let x = random_z () in
    let x_plus_order = Z.(add x Bls12_381.Fr.order) in
    assert (Bls12_381.Fr.(eq (of_z x) (of_z x_plus_order)))

  let test_vectors_to_z_and_of_z () =
    let test_vectors = List.map Z.of_string test_vectors in
    List.iter
      (fun x -> assert (Z.equal (Bls12_381.Fr.to_z (Bls12_381.Fr.of_z x)) x))
      test_vectors

  let get_tests () =
    let open Alcotest in
    ( "Z representation",
      [
        test_case "one" `Quick test_of_z_one;
        test_case "zero" `Quick test_of_z_zero;
        test_case
          "of z and to z with random small numbers"
          `Quick
          (Utils.repeat 100 test_random_of_z_and_to_z);
        test_case
          "to z and of z with test vectors"
          `Quick
          test_vectors_to_z_and_of_z;
        test_case
          "of z accepts value greater than the modulo"
          `Quick
          (Utils.repeat 100 test_random_of_z_higher_than_modulo);
        test_case
          "to z and of z with random small numbers"
          `Quick
          (Utils.repeat 100 test_random_to_z_and_of_z);
      ] )
end

module BytesRepresentation = struct
  let test_bytes_repr_is_zarith_encoding_using_to_bits () =
    (* Pad zarith repr *)
    let r_z = random_z () in
    let bytes_z = Bytes.of_string (Z.to_bits r_z) in
    let bytes = Bytes.make Bls12_381.Fr.size_in_bytes '\000' in
    Bytes.blit bytes_z 0 bytes 0 (Bytes.length bytes_z) ;
    assert (
      Bls12_381.Fr.eq
        (Bls12_381.Fr.of_bytes_exn bytes)
        (Bls12_381.Fr.of_string (Z.to_string r_z))) ;
    let r = Bls12_381.Fr.random () in
    (* Use Fr repr *)
    let bytes_r = Bls12_381.Fr.to_bytes r in
    (* Use the Fr repr to convert in a Z element *)
    let z_r = Z.of_bits (Bytes.to_string bytes_r) in
    (* We should get the same value, using both ways *)
    assert (Z.equal z_r (Bls12_381.Fr.to_z r)) ;
    assert (Bls12_381.Fr.(eq (of_z z_r) r))

  let test_padding_is_done_automatically_with_of_bytes () =
    let z = Z.of_string "32343543534" in
    let z_bytes = Bytes.of_string (Z.to_bits z) in
    (* Checking we are in the case requiring a padding *)
    assert (Bytes.length z_bytes < Bls12_381.Fr.size_in_bytes) ;
    (* Should not raise an exception *)
    let e = Bls12_381.Fr.of_bytes_exn z_bytes in
    (* Should not be an option *)
    assert (Option.is_some (Bls12_381.Fr.of_bytes_opt z_bytes)) ;
    (* Equality in Fr should be fine (require to check to verify the internal
       representation is the same). In the current implementation, we verify the
       internal representation is the padded version. *)
    assert (Bls12_381.Fr.(eq (of_z z) e)) ;
    (* And as zarith elements, we also have the equality *)
    assert (Z.equal (Bls12_381.Fr.to_z e) z)

  let test_of_bytes_exn_and_opt_do_not_accept_elements_higher_than_the_modulus
      () =
    (* last byte of Bls12_381.Fr.order is 115 *)
    let r =
      Bytes.init 32 (fun i ->
          char_of_int
          @@ if i = 31 then 116 + Random.int (256 - 116) else Random.int 256)
    in
    assert (Option.is_none (Bls12_381.Fr.of_bytes_opt r)) ;
    try
      ignore @@ Bls12_381.Fr.of_bytes_exn r ;
      assert false
    with Bls12_381.Fr.Not_in_field _ -> ()

  let get_tests () =
    let open Alcotest in
    ( "Bytes representation",
      [
        test_case
          "bytes representation is the same than zarith using Z.to_bits"
          `Quick
          (Utils.repeat 10 test_bytes_repr_is_zarith_encoding_using_to_bits);
        test_case
          "of_bytes_[exn/opt] do not accept elements higher than the modulus"
          `Quick
          (Utils.repeat
             10
             test_of_bytes_exn_and_opt_do_not_accept_elements_higher_than_the_modulus);
        test_case
          "Padding is done automatically with of_bytes"
          `Quick
          test_padding_is_done_automatically_with_of_bytes;
      ] )
end

module TestVector = struct
  let test_inverse () =
    let test_vectors =
      [
        ( "5241434266765085153989819426158356963249585137477420674959011812945457865191",
          "10839440052692226066497714164180551800338639216929046788248680350103009908352"
        );
        ( "45771516566988367809715142190959127910391288669516577059039340716912455457131",
          "45609475631078884634858595528211458305369692448866344559573507066772305338186"
        );
        ( "12909915968096385929046240252673624834885730199746273136167032454235900707423",
          "11000310335493461593980032382804784919007817741315871286620011674413549793814"
        );
        ( "9906806778085203695146840231942453635945512651510460213691437498308396392030",
          "14376170892131209521313997949250266279614396523892055155196474364730307649110"
        );
        ( "20451006147593515828371694915490427948041026610654337997907355913265840025855",
          "9251674366848220983783993301665718813823734287374642487691950418950023775049"
        );
        ( "22753274685202779061111872324861161292260930710591061598808549358079414450472",
          "5879182491359474138365930955028927605587956455972550635628359324770111549635"
        );
        ( "12823588949385074189879212809942339506958509313775057573450243545256259992541",
          "37176703988340956294235799427206509384158992510189606907136259793202107500314"
        );
      ]
    in
    List.iter
      (fun (e, i) ->
        assert (
          Bls12_381.Fr.eq
            (Bls12_381.Fr.inverse_exn (Bls12_381.Fr.of_string e))
            (Bls12_381.Fr.of_string i)))
      test_vectors ;
    List.iter
      (fun (e, i) ->
        assert (
          Bls12_381.Fr.eq
            (Bls12_381.Fr.inverse_exn (Bls12_381.Fr.of_string i))
            (Bls12_381.Fr.of_string e)))
      test_vectors

  let test_add_bulk () =
    let n = 10 + Random.int 1_000 in
    let xs = List.init n (fun _ -> Bls12_381.Fr.random ()) in
    assert (
      Bls12_381.Fr.(
        eq
          (List.fold_left Bls12_381.Fr.add Bls12_381.Fr.zero xs)
          (Bls12_381.Fr.add_bulk xs)))

  let test_mul_bulk () =
    let n = 10 + Random.int 1_000 in
    let xs = List.init n (fun _ -> Bls12_381.Fr.random ()) in
    let left = List.fold_left Bls12_381.Fr.mul Bls12_381.Fr.one xs in
    let right = Bls12_381.Fr.mul_bulk xs in
    if not @@ Bls12_381.Fr.(eq left right) then
      Alcotest.failf
        "Expected result %s, computed %s\n"
        (Bls12_381.Fr.to_string left)
        (Bls12_381.Fr.to_string right)

  let test_add () =
    let test_vectors =
      [
        ( "52078196679215712148218322720576334474579224383898730538745959257577939031988",
          "14304697501712570926435354702070278490052573047716755203338045808050772484669",
          "13947019005802092595205936914460647126941244931087847919480346365690130332144"
        );
        ( "19157304358764478240694328289471146271697961435094141547920922715555209453450",
          "11728945318991987128312512931314113966598035268029910445432277435051890961717",
          "30886249677756465369006841220785260238295996703124051993353200150607100415167"
        );
        ( "31296266781120594533063853258918717262467469319142606380721992558348378328397",
          "5820131821230508181650789592096633040648713066445785718497340531185653967933",
          "37116398602351102714714642851015350303116182385588392099219333089534032296330"
        );
        ( "39560938173284521169378001220360644956845338274621437250191508195058982219820",
          "38064607903920408690614292538356509340138834185257338707027916971694121463660",
          "25189670902078739380544553250531188459293619959351138134615766466814522498967"
        );
      ]
    in
    List.iter
      (fun (e1, e2, expected_result) ->
        assert (
          Bls12_381.Fr.eq
            (Bls12_381.Fr.add
               (Bls12_381.Fr.of_string e1)
               (Bls12_381.Fr.of_string e2))
            (Bls12_381.Fr.of_string expected_result)))
      test_vectors ;
    List.iter
      (fun (e1, e2, expected_result) ->
        assert (
          Bls12_381.Fr.eq
            (Bls12_381.Fr.add
               (Bls12_381.Fr.of_string e2)
               (Bls12_381.Fr.of_string e1))
            (Bls12_381.Fr.of_string expected_result)))
      test_vectors

  let test_mul () =
    let test_vectors =
      [
        ( "38060637728987323531851344110399976342797446962849502240683562298774992708830",
          "5512470721848092388961431210636327528269807331564913139270778763494220846493",
          "37668727721438606074520892100332665478321086205735021165111387339937557071514"
        );
        ( "8920353329234094921489611026184774357268414518382488349470656930013415883424",
          "49136653454012368208567167956110520759637791556856057105423947118262807325779",
          "15885623306930744461021285813204059242301068985087295733128928505332635787610"
        );
        ( "27505619973888738863986068934484781011766945824263356612923712981356457561202",
          "50243072596783212750626991643373709632302860135434554488507947926966036993873",
          "41343614115054986651575849604178072836351973556978705402848027675783507031010"
        );
        ( "22595773174612669619067973477148714090185633332320792125410903789347752011910",
          "52328732251934881978597625733405265672319639896554870653166667703616699256860",
          "40257812317025926695523520096471471069294532648049850170792668232075952784083"
        );
      ]
    in
    List.iter
      (fun (e1, e2, expected_result) ->
        assert (
          Bls12_381.Fr.eq
            (Bls12_381.Fr.mul
               (Bls12_381.Fr.of_string e1)
               (Bls12_381.Fr.of_string e2))
            (Bls12_381.Fr.of_string expected_result)))
      test_vectors ;
    List.iter
      (fun (e1, e2, expected_result) ->
        assert (
          Bls12_381.Fr.eq
            (Bls12_381.Fr.mul
               (Bls12_381.Fr.of_string e2)
               (Bls12_381.Fr.of_string e1))
            (Bls12_381.Fr.of_string expected_result)))
      test_vectors

  let test_opposite () =
    let test_vectors =
      [
        ( "41115813042790628185693779037818020465346656435243125143422155873970076434871",
          "11320062132335562293753961470367945372343896065284512679181502825968504749642"
        );
        ( "42018322502149629012634568822875196842144777572867508162082880801617895571737",
          "10417552672976561466813171685310768995545774927660129660520777898320685612776"
        );
        ( "34539139262525805815749017833342205015904514998269280061826808173178967747220",
          "17896735912600384663698722674843760821786037502258357760776850526759613437293"
        );
        ( "48147683698672565222275497827671970468018938121714425045755179114542522684737",
          "4288191476453625257172242680513995369671614378813212776848479585396058499776"
        );
      ]
    in
    List.iter
      (fun (e1, expected_result) ->
        assert (
          Bls12_381.Fr.eq
            (Bls12_381.Fr.negate (Bls12_381.Fr.of_string e1))
            (Bls12_381.Fr.of_string expected_result)))
      test_vectors ;
    List.iter
      (fun (e1, expected_result) ->
        assert (
          Bls12_381.Fr.eq
            (Bls12_381.Fr.negate (Bls12_381.Fr.of_string expected_result))
            (Bls12_381.Fr.of_string e1)))
      test_vectors

  let test_pow () =
    let test_vectors =
      [
        ( "19382565044794829105685946147333667407406947769919002500736830762980080217116",
          "48159949448997187908979844521309454081051202554580566653703924472697903187543",
          "51805065919052658973952545206023802114592698824188349145165662267033488307015"
        );
        ( "38434293760957543250833416278928537431247174199351417891430036507051711516795",
          "19350167110479287515066444930433610752856061045118438172892254847951537570134",
          "5638414748000331847846282606999064802458819295656595143203518899742396580213"
        );
        ( "49664271363539622878107770584406780589976347771473156015482691689195652813880",
          "19379581748332915194987329063856477906332155141792491408304078230104564222030",
          "30921874175813683797322233883008640815321607592610957475928976635504264297632"
        );
        ( "51734967732893479663302261399661867713222970046133566655959761380034878973281",
          "37560370265646062523028551976728263929547556442627149817510607017268305870511",
          "49814797937772261149726667662726741057831444313882786994092918399718266462922"
        );
      ]
    in
    List.iter
      (fun (x, e, expected_result) ->
        assert (
          Bls12_381.Fr.eq
            (Bls12_381.Fr.pow (Bls12_381.Fr.of_string x) (Z.of_string e))
            (Bls12_381.Fr.of_string expected_result)))
      test_vectors

  let get_tests () =
    let open Alcotest in
    ( "Test vectors",
      [
        test_case "inverse" `Quick test_inverse;
        test_case "add" `Quick test_add;
        test_case "add bulk" `Quick test_add_bulk;
        test_case "mul bulk" `Quick test_mul_bulk;
        test_case "opposite" `Quick test_opposite;
        test_case "pow" `Quick test_pow;
        test_case "multiplication" `Quick test_mul;
      ] )
end

module OCamlComparisonOperators = struct
  let test_fr_equal_with_same_random_element () =
    let x = Bls12_381.Fr.random () in
    if not (x = x) then
      Alcotest.failf
        "(=) Expected comparison on the same random element must be true, got \
         false"

  let test_fr_equal_with_zero () =
    if not (Bls12_381.Fr.zero = Bls12_381.Fr.zero) then
      Alcotest.failf "(=) Expected comparison on zero must be true, got false"

  let test_fr_equal_with_one () =
    if not (Bls12_381.Fr.one = Bls12_381.Fr.one) then
      Alcotest.failf "(=) Expected comparison on one must be true, got false"

  let test_fr_equality_failing_test_with_random () =
    let x = Bls12_381.Fr.random () in
    let y = Bls12_381.Fr.(x + one) in
    if x = y then
      Alcotest.failf
        "(=) Expected comparison on a random element and its successor must be \
         false, got true"

  let test_fr_different_failing_test_with_same_random_element () =
    let x = Bls12_381.Fr.random () in
    if x != x then
      Alcotest.failf
        "(!=) Expected comparison on a random element must be false, got true"

  let test_fr_zero_is_smaller_than_one () =
    if not (Bls12_381.Fr.zero < Bls12_381.Fr.one) then
      Alcotest.failf "(<) zero is expected to be smaller than one"

  let test_fr_zero_is_not_greater_than_one () =
    if Bls12_381.Fr.zero > Bls12_381.Fr.one then
      Alcotest.failf "(>) zero is not expected to be greater than one"

  let test_fr_one_is_greater_than_zero () =
    if not (Bls12_381.Fr.one > Bls12_381.Fr.zero) then
      Alcotest.failf "(>) one is expected to be greater than zero"

  let test_fr_one_is_not_smaller_than_zero () =
    if Bls12_381.Fr.one < Bls12_381.Fr.zero then
      Alcotest.failf "(<) one is not expected to be smaller than zero"

  let test_fr_successor_is_greater () =
    let x = Bls12_381.Fr.random () in
    if not (Bls12_381.Fr.(x + one) > x) then
      Alcotest.failf "(>) the successor of an element is expected to be greater"

  let test_fr_random_element_is_smaller_than_its_successor () =
    let x = Bls12_381.Fr.random () in
    if not (x < Bls12_381.Fr.(x + one)) then
      Alcotest.failf
        "(<) a random element (when smaller than order - 1) is smaller than \
         its succesor"

  let get_tests () =
    let open Alcotest in
    ( "Test comparison operators",
      [
        test_case
          "(=) operator on random element"
          `Quick
          (Utils.repeat 100 test_fr_equal_with_same_random_element);
        test_case
          "(=) operator on random element: failing test"
          `Quick
          (Utils.repeat 100 test_fr_equality_failing_test_with_random);
        test_case
          "(!=) operator on random element: failing test"
          `Quick
          (Utils.repeat
             100
             test_fr_different_failing_test_with_same_random_element);
        test_case "(=) operator on zero" `Quick test_fr_equal_with_zero;
        test_case "(=) operator on one" `Quick test_fr_equal_with_one;
        test_case "(<) 0 < 1" `Quick test_fr_zero_is_smaller_than_one;
        test_case
          "(>) 0 > 1: failing test"
          `Quick
          test_fr_zero_is_not_greater_than_one;
        test_case "(>) successor is greater" `Quick test_fr_successor_is_greater;
        test_case "(>) 1 > 0" `Quick test_fr_one_is_greater_than_zero;
        test_case
          "(<) 1 < 0: failing test"
          `Quick
          test_fr_one_is_not_smaller_than_zero;
        test_case
          "(<) x < x + 1"
          `Quick
          test_fr_random_element_is_smaller_than_its_successor;
      ] )
end

module InnerProduct = struct
  let test_random_elements () =
    let n = 1 + Random.int 1000 in
    let a = Array.init n (fun _ -> Bls12_381.Fr.random ()) in
    let b = Array.init n (fun _ -> Bls12_381.Fr.random ()) in
    let exp_res =
      Array.fold_left
        Bls12_381.Fr.add
        Bls12_381.Fr.zero
        (Array.map2 Bls12_381.Fr.mul a b)
    in
    let res_exn = Bls12_381.Fr.inner_product_exn a b in
    let res_opt = Bls12_381.Fr.inner_product_opt a b in
    assert (Option.is_some res_opt) ;
    assert (Bls12_381.Fr.eq exp_res res_exn) ;
    assert (Bls12_381.Fr.eq exp_res (Option.get res_opt))

  let get_tests () =
    let open Alcotest in
    ( "Inner product",
      [
        test_case
          "with random elements"
          `Quick
          (Utils.repeat 100 test_random_elements);
      ] )
end

module AdditionalConstructors = struct
  let test_positive_values_as_documented () =
    let n = Random.int 1_000_000 in
    let n_fr = Bls12_381.Fr.of_int n in
    assert (Bls12_381.Fr.(eq (of_z (Z.of_int n)) n_fr))

  let test_positive_values_use_decimal_representation () =
    let n = Random.int 1_000_000 in
    let n_fr = Bls12_381.Fr.of_int n in
    assert (String.equal (Bls12_381.Fr.to_string n_fr) (string_of_int n))

  let test_negative_values_as_documented () =
    let n = -Random.int 1_000_000 in
    let n_fr = Bls12_381.Fr.of_int n in
    assert (Bls12_381.Fr.(eq (of_z (Z.of_int n)) n_fr))

  let test_negative_values_use_decimal_representation () =
    let n = -Random.int 1_000_000 in
    let n_fr = Bls12_381.Fr.of_int n in
    let res = Bls12_381.Fr.to_string n_fr in
    let exp_res_z = Z.(add Bls12_381.Fr.order (of_int n)) in
    let exp_res = Bls12_381.Fr.(to_string (of_z exp_res_z)) in
    assert (String.equal res exp_res)

  let get_tests () =
    let open Alcotest in
    ( "Additional Constructors",
      [
        test_case
          "with positive values as documented"
          `Quick
          (Utils.repeat 100 test_positive_values_as_documented);
        test_case
          "with positive values use decimal represntation"
          `Quick
          (Utils.repeat 100 test_positive_values_use_decimal_representation);
        test_case
          "with negative values as documented"
          `Quick
          (Utils.repeat 100 test_negative_values_as_documented);
        test_case
          "with negeative values use decimal represntation"
          `Quick
          (Utils.repeat 100 test_negative_values_use_decimal_representation);
      ] )
end

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Fr"
    (TestVector.get_tests ()
    :: ZRepresentation.get_tests ()
    :: Memory.get_tests ()
    :: AdditionalConstructors.get_tests ()
    :: InplaceOperations.get_tests ()
    :: BytesRepresentation.get_tests ()
    :: OCamlComparisonOperators.get_tests ()
    :: InnerProduct.get_tests ()
    :: StringRepresentation.get_tests ()
    :: Tests.get_tests ())
