module Parameters : Mec.Permutation.Marvellous.PARAMETERS = struct
  let width = 3

  let rounds = 14

  let round_constants = Ark_pobls.v

  let mds_matrix = Mds_pobls.v

  (* This is the first alpha such that pgc(alpha, p - 1) = 1 *)
  let alpha = Z.of_string "5"

  let alphainv =
    Z.of_string
      "20974350070050476191779096203274386335076221000211055129041463479975432473805"
end

module Scalar = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)

module Permutation = Mec.Permutation.Marvellous.Make (Parameters) (Scalar)

let test_vectors_from_reference_implementation () =
  let values =
    [
      ( [|"42"; "42"; "42"|],
        [|
          "1700964629023230081688449128938267741281135202287971876415319604264351638730";
          "30351877146302309485006750598668855676603663881859469782701435098299236408108";
          "1266974358383892903376729581212148792188884064230596723344843464220594616291";
        |] );
    ]
  in
  List.iter
    (fun (input, exp_output) ->
      let input = Array.map Scalar.of_string input in
      let exp_output = Array.map Scalar.of_string exp_output in
      let ctxt = Permutation.init input in
      Permutation.apply ctxt ;
      let res = Permutation.get ctxt in
      Array.iter2
        (fun a b ->
          if not (Scalar.eq a b) then
            Alcotest.failf
              "Expected output is %s but computed %s"
              (Scalar.to_string a)
              (Scalar.to_string b))
        exp_output
        res)
    values

let test_regression () =
  let test_vectors =
    [
      ( [|
          "0";
          "19540886853600136773806888540031779652697522926951761090609474934921975120659";
          "27368034540955591518185075247638312229509481411752400387472688330662143761856";
        |],
        [|
          "44673107086290821414141179286216522882469521449559636868455247005114020114445";
          "36631684854749147804429266824538272650916679166988362643455728142484809981118";
          "6299192490564547343493433393738313689693119470079691921424273466838440224855";
        |] );
      ( [|
          "0";
          "27079498335589470388559429012071885383086029562052523482197446658383111072774";
          "42808036164195249275280963312025828986508786508614910971333518929197538998773";
        |],
        [|
          "33984868849407543137734814872861866351541889998060667955612713996984910605397";
          "4641627185242898456867942643033909448689919023961721348259024082238188176596";
          "713519522119335567984877165818659706904142877478060072511766145209379073275";
        |] );
      ( [|
          "0";
          "2280773603130921897588628948561054337050485605305725986334019017364524534576";
          "49742584797721038216392695876666332237687899609664289853866233942594829409245";
        |],
        [|
          "20077542126429611153881019033799341995311428088869746915924850362129927371289";
          "38547305939988350836972846971113863192807010866171540518921476662284151835220";
          "8478277515116959072250029506006378039116363843044265570742187828912632979438";
        |] );
      ( [|
          "0";
          "47345255170739768354940339244069904962490289137071838723660628082786560244227";
          "12508134994244913485311518548620293355291296251148491744230743543606532994206";
        |],
        [|
          "50927739926931280620422144191479516292814380533564245832950972175783227490582";
          "47903072246991027484665412890156631636069614146568039741296638969334550407630";
          "47491331892735386854365197646017331478438648030708643559209673528669254013444";
        |] );
      ( [|
          "0";
          "22266149478348354835731057366322705807112053199389518651299197937563769914341";
          "46391019025260596444368872653036215340017923491388854958773789052412558961328";
        |],
        [|
          "37498820915193954318343590451968185938009803654297685764064124822117204060828";
          "16239130390180195445652573959561373168694718306164480810197800625237334891866";
          "21507804981211574030250333588537037287147041282903807447161895309722506987102";
        |] );
    ]
  in
  List.iter
    (fun (inputs, expected_output) ->
      let inputs = Array.map Scalar.of_string inputs in
      let expected_output = Array.map Scalar.of_string expected_output in
      let ctxt = Permutation.init inputs in
      let () = Permutation.apply ctxt in
      let output = Permutation.get ctxt in
      Array.iter2
        (fun a b ->
          if not (Scalar.eq a b) then
            Alcotest.failf
              "Expected output is %s, computed %s\n"
              (Scalar.to_string a)
              (Scalar.to_string b))
        expected_output
        output)
    test_vectors

let () =
  Alcotest.run
    ~__FILE__
    "Marvellous permutation"
    [
      ( "Built on scalar field of BLS12-381",
        [
          Alcotest.test_case
            "Test vectors from reference implementation from KUL"
            `Quick
            test_vectors_from_reference_implementation;
          Alcotest.test_case "Regression tests" `Quick test_regression;
        ] );
    ]
