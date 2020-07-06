open Bls12_381

let rec repeat n f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat (n - 1) f )

module Properties = struct
  let with_zero_as_first_component () =
    assert (
      Fq12.eq
        (Pairing.pairing G1.Uncompressed.zero (G2.Uncompressed.random ()))
        Fq12.one )

  let with_zero_as_second_component () =
    assert (
      Fq12.eq
        (Pairing.pairing (G1.Uncompressed.random ()) G2.Uncompressed.zero)
        Fq12.one )

  let linearity_commutativity_scalar () =
    (* pairing(a * g_{1}, b * g_{2}) = pairing(b * g_{1}, a * g_{2})*)
    let a = Fr.random () in
    let b = Fr.random () in
    let g1 = G1.Uncompressed.random () in
    let g2 = G2.Uncompressed.random () in
    assert (
      Fq12.eq
        (Pairing.pairing (G1.Uncompressed.mul g1 a) (G2.Uncompressed.mul g2 b))
        (Pairing.pairing (G1.Uncompressed.mul g1 b) (G2.Uncompressed.mul g2 a))
    )

  let linearity_commutativity_scalar_with_only_one_scalar () =
    (* pairing(a * g_{1}, g_{2}) = pairing(a * g_{1}, g_{2})*)
    let a = Fr.random () in
    let g1 = G1.Uncompressed.random () in
    let g2 = G2.Uncompressed.random () in
    assert (
      Fq12.eq
        (Pairing.pairing g1 (G2.Uncompressed.mul g2 a))
        (Pairing.pairing (G1.Uncompressed.mul g1 a) g2) )

  let linearity_scalar_in_scalar_with_only_one_scalar () =
    (* pairing(a * g_{1}, g_{2}) = pairing(g_{1}, g_{2}) ^ a*)
    let a = Fr.random () in
    let g1 = G1.Uncompressed.random () in
    let g2 = G2.Uncompressed.random () in
    assert (
      Fq12.eq
        (Pairing.pairing g1 (G2.Uncompressed.mul g2 a))
        (Fq12.pow (Pairing.pairing g1 g2) (Fr.to_z a)) )

  let full_linearity () =
    let a = Fr.random () in
    let b = Fr.random () in
    let g1 = G1.Uncompressed.random () in
    let g2 = G2.Uncompressed.random () in
    assert (
      Fq12.eq
        (Pairing.pairing (G1.Uncompressed.mul g1 a) (G2.Uncompressed.mul g2 b))
        (Fq12.pow (Pairing.pairing g1 g2) (Z.mul (Fr.to_z a) (Fr.to_z b))) ) ;
    assert (
      Fq12.eq
        (Pairing.pairing (G1.Uncompressed.mul g1 a) (G2.Uncompressed.mul g2 b))
        (Fq12.pow (Pairing.pairing g1 g2) (Fr.to_z (Fr.mul a b))) )

  let result_pairing_with_miller_loop_followed_by_final_exponentiation () =
    let a = Fr.random () in
    let b = Fr.random () in
    let g1 = G1.Uncompressed.random () in
    let g2 = G2.Uncompressed.random () in
    assert (
      Fq12.eq
        (Pairing.pairing (G1.Uncompressed.mul g1 a) (G2.Uncompressed.mul g2 b))
        (Pairing.unsafe_final_exponentiation
           (Pairing.miller_loop_simple
              (G1.Uncompressed.mul g1 a)
              (G2.Uncompressed.mul g2 b))) )
end

let test_vectors_one_one () =
  let expected_result =
    Fq12.of_string
      "2819105605953691245277803056322684086884703000473961065716485506033588504203831029066448642358042597501014294104502"
      "1323968232986996742571315206151405965104242542339680722164220900812303524334628370163366153839984196298685227734799"
      "2987335049721312504428602988447616328830341722376962214011674875969052835043875658579425548512925634040144704192135"
      "3879723582452552452538684314479081967502111497413076598816163759028842927668327542875108457755966417881797966271311"
      "261508182517997003171385743374653339186059518494239543139839025878870012614975302676296704930880982238308326681253"
      "231488992246460459663813598342448669854473942105054381511346786719005883340876032043606739070883099647773793170614"
      "3993582095516422658773669068931361134188738159766715576187490305611759126554796569868053818105850661142222948198557"
      "1074773511698422344502264006159859710502164045911412750831641680783012525555872467108249271286757399121183508900634"
      "2727588299083545686739024317998512740561167011046940249988557419323068809019137624943703910267790601287073339193943"
      "493643299814437640914745677854369670041080344349607504656543355799077485536288866009245028091988146107059514546594"
      "734401332196641441839439105942623141234148957972407782257355060229193854324927417865401895596108124443575283868655"
      "2348330098288556420918672502923664952620152483128593484301759394583320358354186482723629999370241674973832318248497"
  in
  assert (
    Fq12.eq
      (Pairing.pairing G1.Uncompressed.one G2.Uncompressed.one)
      expected_result ) ;
  assert (
    Fq12.eq
      (Pairing.unsafe_final_exponentiation
         (Pairing.miller_loop_simple G1.Uncompressed.one G2.Uncompressed.one))
      expected_result )

let test_vectors_one_one_multiple_miller_loop () =
  let expected_result =
    Fq12.of_string
      "2819105605953691245277803056322684086884703000473961065716485506033588504203831029066448642358042597501014294104502"
      "1323968232986996742571315206151405965104242542339680722164220900812303524334628370163366153839984196298685227734799"
      "2987335049721312504428602988447616328830341722376962214011674875969052835043875658579425548512925634040144704192135"
      "3879723582452552452538684314479081967502111497413076598816163759028842927668327542875108457755966417881797966271311"
      "261508182517997003171385743374653339186059518494239543139839025878870012614975302676296704930880982238308326681253"
      "231488992246460459663813598342448669854473942105054381511346786719005883340876032043606739070883099647773793170614"
      "3993582095516422658773669068931361134188738159766715576187490305611759126554796569868053818105850661142222948198557"
      "1074773511698422344502264006159859710502164045911412750831641680783012525555872467108249271286757399121183508900634"
      "2727588299083545686739024317998512740561167011046940249988557419323068809019137624943703910267790601287073339193943"
      "493643299814437640914745677854369670041080344349607504656543355799077485536288866009245028091988146107059514546594"
      "734401332196641441839439105942623141234148957972407782257355060229193854324927417865401895596108124443575283868655"
      "2348330098288556420918672502923664952620152483128593484301759394583320358354186482723629999370241674973832318248497"
  in
  let expected_result = Fq12.mul expected_result expected_result in
  assert (
    Fq12.eq
      (Pairing.miller_loop
         [ (G1.Uncompressed.one, G2.Uncompressed.one);
           (G1.Uncompressed.one, G2.Uncompressed.one) ])
      expected_result )

let () =
  let open Alcotest in
  run
    "Pairing"
    [ ( "Properties",
        [ test_case
            "with zero as first component"
            `Quick
            (repeat 100 Properties.with_zero_as_first_component);
          test_case
            "with zero as second component"
            `Quick
            (repeat 100 Properties.with_zero_as_second_component);
          test_case
            "linearity commutative scalar with only one scalar"
            `Quick
            (repeat
               100
               Properties.linearity_commutativity_scalar_with_only_one_scalar);
          test_case
            "linearity scalar in scalar with only one scalar"
            `Quick
            (repeat
               100
               Properties.linearity_scalar_in_scalar_with_only_one_scalar);
          test_case
            "full linearity"
            `Quick
            (repeat 100 Properties.full_linearity);
          test_case
            "test vectors pairing of one and one"
            `Quick
            (repeat 10 test_vectors_one_one);
          test_case
            "test result pairing with miller loop simple followed by final exponentiation"
            `Quick
            (repeat
               10
               Properties
               .result_pairing_with_miller_loop_followed_by_final_exponentiation);
          test_case
            "linearity commutativity scalar"
            `Quick
            (repeat 100 Properties.linearity_commutativity_scalar) ] ) ]
