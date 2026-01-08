(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(* Testing
   -------
   Component:    Lib Bls12_381_hash
   Invocation:   dune exec src/lib_bls12_381_hash/test/main.exe \
                  -- --file test_anemoi.ml
   Subject:      Test Bls12_381_hash
*)

(* Generated using
   https://github.com/vesselinux/anemoi-hash/commit/9d9fc2a52e31c5e9379be2856414233e4e780f58
   with:
       test_jive(
           n_tests=10,
           q=52435875175126190479447740508185965837690552500527637822603658699938581184513,
           alpha=5,
           n_rounds=19,
           n_cols=1,
           b=2,
           security_level=128)
*)

let test_vectors_anemoi128_1 () =
  let vectors =
    [
      ( ( "35845669682800269995209049467553751861599775089532198376674489477778357560569",
          "28310433413288793552092391706285055875131165883761155923425672020460794794380"
        ),
        "17700999234906509130159287769741962009709982288860491609792036961305502635098"
      );
      ( ( "13880477308466057091493501926554391163288910083198657398974047674796581779693",
          "28373522553669637689090633983852091038187643792080489433228018701827926002768"
        ),
        "12743339877423634016102198261148085947154915493891480470390198918335851344292"
      );
      ( ( "38216845839357573472239023564373200263918534132389374505946789120458119648667",
          "39487400661842934211797098190376905462917596530484823725006452014098066670694"
        ),
        "20572002340893814224552312721780888755123139461240945499350006876769919946773"
      );
      ( ( "31131499311453094268713009639057190648340672035944963596113874024605484784791",
          "43680468547604090714590688733926978605538499294774749079842255618079617105051"
        ),
        "20569340066597303483907565303221880873948750088343130268109858923675035314078"
      );
      ( ( "10924450358499789254279737793602938412226831772739559590509902641736698048741",
          "18748800755846695314043868352493820136560550891447333332696158632417495012819"
        ),
        "26749108456690136523602417383379636205348260403407066884482315588658233098940"
      );
      ( ( "46462878845927326448614772124701161417983809741866268465266076023457688715870",
          "38375603405434266869135117417290149314037130980580543700755400748014512343979"
        ),
        "2673364003345693113273421176038865068057650274048787980068125230806361632850"
      );
      ( ( "39170732922513369978795230271067076371378632409257775938940964299534787717175",
          "2408517610992178001486942104041564424468943886715588829396035415376300986469"
        ),
        "13310229620063667311431399083462393119055434191991960510670894258662703203975"
      );
    ]
  in
  List.iter
    (fun ((x1_s, x2_s), exp_res_s) ->
      let x1 = Bls12_381.Fr.of_string x1_s in
      let x2 = Bls12_381.Fr.of_string x2_s in
      let exp_res = Bls12_381.Fr.of_string exp_res_s in
      let res = Bls12_381_hash.Permutation.Anemoi.jive128_1 x1 x2 in
      if not (Bls12_381.Fr.eq res exp_res) then
        Alcotest.failf
          "Expected result = %s, computed result = %s, input = (%s, %s)"
          exp_res_s
          (Bls12_381.Fr.to_string res)
          x1_s
          x2_s)
    vectors

let test_vectors_anemoi128_2 () =
  let vectors =
    [
      ( ("0", "0", "0", "0"),
        ( "23506137766702864106501714498337645198425779982503345533323642665774237530743",
          "34447732706544914382845425456929395985238811287211171548953979792382042373740",
          "38305963246199256223939132687221377996807495295070124114622442403207046030062",
          "52085062252809860097055957733590114937197370490476211931083350477393806117621"
        ) );
      ( ( "27839080182556610705887939966818701340864145322792715287810357208440914854281",
          "41481150764080331637382663225726959872938887013456865948198868284552242708519",
          "38189883379737770191491270513853770942621403161576272820058113505808823630900",
          "6529435419442382849730797362349015216552820358285062739653681825023447217068"
        ),
        ( "14473897802199918953583228273316340143396476374899351470678632558263678699877",
          "24587633048074155346278630990352810495608304969367523541545168873834143089902",
          "47132715740050880437392890948798686584816384888633241850565214056840729624505",
          "6606493401694644772471916676579866387417408500033435451518189487740033865747"
        ) );
      ( ( "14716987267992449080941609095284865759356382937671116288486258350642247071627",
          "13760197033165110695402422952291893734496635277111384162422733902296757710874",
          "33294609204786805322854530266028003773787556848644980529869834547261241614105",
          "11644531352096195394853594279658462131209985215175335801365205433425048456350"
        ),
        ( "47737136264857950730454747976524405656326494304978652844311111261281852476501",
          "13716019695701301324018899979909121742372906083132069510290774177616830373506",
          "19515279450582255760487675881545023135485186764178376441852639052497885578212",
          "45847068138243304073707520982719523479147863491193499975734034993479294600685"
        ) );
      ( ( "42824693565443175259806687802757821151773600948730449896541161263256961486645",
          "31633466755115991202264118192133521021227289162761994770025404887100313415161",
          "43223036947018592502360048383150256123208669504194939523196078411285214762463",
          "7192257416851768692824129030440632757292330835871601049965097914082460916711"
        ),
        ( "38660702903297582610177132699877133632374538437943056324961513888350888194086",
          "31630694424006989989717484628399682914962426161626133340250479990037519265561",
          "13466503268904584668912085977052425437912732123809722793173012874164204852136",
          "4471437305484391530147684290538853431954907851628258053432383018741801157611"
        ) );
    ]
  in

  List.iter
    (fun ( (x1_s, x2_s, y1_s, y2_s),
           (exp_res_x1_s, exp_res_x2_s, exp_res_y1_s, exp_res_y2_s) )
       ->
      let x1 = Bls12_381.Fr.of_string x1_s in
      let x2 = Bls12_381.Fr.of_string x2_s in
      let y1 = Bls12_381.Fr.of_string y1_s in
      let y2 = Bls12_381.Fr.of_string y2_s in
      let exp_res_x1 = Bls12_381.Fr.of_string exp_res_x1_s in
      let exp_res_x2 = Bls12_381.Fr.of_string exp_res_x2_s in
      let exp_res_y1 = Bls12_381.Fr.of_string exp_res_y1_s in
      let exp_res_y2 = Bls12_381.Fr.of_string exp_res_y2_s in
      let state = [|x1; x2; y1; y2|] in
      let ctxt =
        Bls12_381_hash.Permutation.Anemoi.allocate_ctxt
          Bls12_381_hash.Permutation.Anemoi.Parameters.security_128_state_size_4
      in
      let () = Bls12_381_hash.Permutation.Anemoi.set_state ctxt state in
      let () = Bls12_381_hash.Permutation.Anemoi.apply_permutation ctxt in
      let output = Bls12_381_hash.Permutation.Anemoi.get_state ctxt in
      let res_x1, res_x2, res_y1, res_y2 =
        (output.(0), output.(1), output.(2), output.(3))
      in
      let res_x1_s = Bls12_381.Fr.to_string res_x1 in
      let res_x2_s = Bls12_381.Fr.to_string res_x2 in
      let res_y1_s = Bls12_381.Fr.to_string res_y1 in
      let res_y2_s = Bls12_381.Fr.to_string res_y2 in
      let is_eq =
        Bls12_381.Fr.eq res_x1 exp_res_x1
        && Bls12_381.Fr.eq res_x2 exp_res_x2
        && Bls12_381.Fr.eq res_y1 exp_res_y1
        && Bls12_381.Fr.eq res_y2 exp_res_y2
      in
      if not is_eq then
        Alcotest.failf
          "Expected result = (%s, %s, %s, %s), computed result = (%s, %s, %s, \
           %s), input = (%s, %s, %s, %s)"
          exp_res_x1_s
          exp_res_x2_s
          exp_res_y1_s
          exp_res_y2_s
          res_x1_s
          res_x2_s
          res_y1_s
          res_y2_s
          x1_s
          x2_s
          y1_s
          y2_s)
    vectors

let test_vectors_anemoi128_3 () =
  let vectors =
    [
      ( ("0", "0", "0", "0", "0", "0"),
        ( "33478314834223416808169927398566653899403429754255462666159894772887073320365",
          "47176243459522521751687145816065715088321551004033205074244614792181543416010",
          "23618281931389771856815846815269641563251679885040985573831869306241272644113",
          "31724951276938006062858814692792550537217527277501666853791904466055832323690",
          "42647860407630674344335892435289166728294144362723196232677285611394540961425",
          "19471638696219074945013770512807270758314299984285699758112364258991389044512"
        ) );
      ( ("1", "1", "1", "1", "1", "1"),
        ( "35479415329613071429429550675265174289054891063407106141043395424648010771320",
          "40306996468472921766576498685266538561364696876140265870598730755251029512891",
          "20690705352280971494112435038111871800589215895863022560441462778525416320807",
          "46706916210868941572390746709219268688104096455025207646228390693939029609098",
          "36997439342487000681405280778882655995987145098763687399724629582578794692794",
          "48039916176227764586762930360199537139847085765157498900275563135023139617488"
        ) );
      ( ("0", "0", "0", "1", "1", "1"),
        ( "37933281578278578226664982565092449003672438365411607042195625975115660158828",
          "44753139152867038775378832584199388585565237869691759485112778145882682277556",
          "32230208920419839266320424365836626532619521980144779932965733449069428147791",
          "49800001876826046352042671365996929335824355383400972887543145110732946379057",
          "5180942914719476310821602386486420533986710078522585795533936269418555321952",
          "2679906742199435085043898910663175140308975212645374494483508379075336654721"
        ) );
    ]
  in

  List.iter
    (fun ( (x1_s, x2_s, x3_s, y1_s, y2_s, y3_s),
           ( exp_res_x1_s,
             exp_res_x2_s,
             exp_res_x3_s,
             exp_res_y1_s,
             exp_res_y2_s,
             exp_res_y3_s ) )
       ->
      let x1 = Bls12_381.Fr.of_string x1_s in
      let x2 = Bls12_381.Fr.of_string x2_s in
      let x3 = Bls12_381.Fr.of_string x3_s in
      let y1 = Bls12_381.Fr.of_string y1_s in
      let y2 = Bls12_381.Fr.of_string y2_s in
      let y3 = Bls12_381.Fr.of_string y3_s in
      let exp_res_x1 = Bls12_381.Fr.of_string exp_res_x1_s in
      let exp_res_x2 = Bls12_381.Fr.of_string exp_res_x2_s in
      let exp_res_x3 = Bls12_381.Fr.of_string exp_res_x3_s in
      let exp_res_y1 = Bls12_381.Fr.of_string exp_res_y1_s in
      let exp_res_y2 = Bls12_381.Fr.of_string exp_res_y2_s in
      let exp_res_y3 = Bls12_381.Fr.of_string exp_res_y3_s in
      let state = [|x1; x2; x3; y1; y2; y3|] in
      let ctxt =
        Bls12_381_hash.Permutation.Anemoi.allocate_ctxt
          Bls12_381_hash.Permutation.Anemoi.Parameters.security_128_state_size_6
      in
      let () = Bls12_381_hash.Permutation.Anemoi.set_state ctxt state in
      let () = Bls12_381_hash.Permutation.Anemoi.apply_permutation ctxt in
      let output = Bls12_381_hash.Permutation.Anemoi.get_state ctxt in
      let res_x1, res_x2, res_x3, res_y1, res_y2, res_y3 =
        (output.(0), output.(1), output.(2), output.(3), output.(4), output.(5))
      in
      let res_x1_s = Bls12_381.Fr.to_string res_x1 in
      let res_x2_s = Bls12_381.Fr.to_string res_x2 in
      let res_x3_s = Bls12_381.Fr.to_string res_x3 in
      let res_y1_s = Bls12_381.Fr.to_string res_y1 in
      let res_y2_s = Bls12_381.Fr.to_string res_y2 in
      let res_y3_s = Bls12_381.Fr.to_string res_y3 in
      let is_eq =
        Bls12_381.Fr.eq res_x1 exp_res_x1
        && Bls12_381.Fr.eq res_x2 exp_res_x2
        && Bls12_381.Fr.eq res_x3 exp_res_x3
        && Bls12_381.Fr.eq res_y1 exp_res_y1
        && Bls12_381.Fr.eq res_y2 exp_res_y2
        && Bls12_381.Fr.eq res_y3 exp_res_y3
      in
      if not is_eq then
        Alcotest.failf
          "Expected result = (%s, %s, %s, %s, %s, %s), computed result = (%s, \
           %s, %s, %s, %s, %s), input = (%s, %s, %s, %s, %s, %s)"
          exp_res_x1_s
          exp_res_x2_s
          exp_res_x3_s
          exp_res_y1_s
          exp_res_y2_s
          exp_res_y3_s
          res_x1_s
          res_x2_s
          res_x3_s
          res_y1_s
          res_y2_s
          res_y3_s
          x1_s
          x2_s
          x3_s
          y1_s
          y2_s
          y3_s)
    vectors

let test_vectors_anemoi128_4 () =
  let vectors =
    [
      ( ("0", "0", "0", "0", "0", "0", "0", "0"),
        ( "7706460525141057798581274838395425127394178058031335456383676808670282739597",
          "7765006946327915662125438952942444007224155128922309040617707265091525101650",
          "36678898577444650135514357278510022052240370283820393434444670594528685414542",
          "38112460417474206233727356444574986419489443411513333432061688919529220935208",
          "32886919269921708859920915140938102703808094739820106011065065407661206427905",
          "31957519853632408523793272895865433064889015146894845079565739509711556279655",
          "48507819850717925613830013169811675061814439063072799726071757655412038748460",
          "12054807835900330617081688019415644583219549287154039455011128765870547430639"
        ) );
    ]
  in

  List.iter
    (fun ( (x1_s, x2_s, x3_s, x4_s, y1_s, y2_s, y3_s, y4_s),
           ( exp_res_x1_s,
             exp_res_x2_s,
             exp_res_x3_s,
             exp_res_x4_s,
             exp_res_y1_s,
             exp_res_y2_s,
             exp_res_y3_s,
             exp_res_y4_s ) )
       ->
      let x1 = Bls12_381.Fr.of_string x1_s in
      let x2 = Bls12_381.Fr.of_string x2_s in
      let x3 = Bls12_381.Fr.of_string x3_s in
      let x4 = Bls12_381.Fr.of_string x4_s in
      let y1 = Bls12_381.Fr.of_string y1_s in
      let y2 = Bls12_381.Fr.of_string y2_s in
      let y3 = Bls12_381.Fr.of_string y3_s in
      let y4 = Bls12_381.Fr.of_string y4_s in
      let exp_res_x1 = Bls12_381.Fr.of_string exp_res_x1_s in
      let exp_res_x2 = Bls12_381.Fr.of_string exp_res_x2_s in
      let exp_res_x3 = Bls12_381.Fr.of_string exp_res_x3_s in
      let exp_res_x4 = Bls12_381.Fr.of_string exp_res_x4_s in
      let exp_res_y1 = Bls12_381.Fr.of_string exp_res_y1_s in
      let exp_res_y2 = Bls12_381.Fr.of_string exp_res_y2_s in
      let exp_res_y3 = Bls12_381.Fr.of_string exp_res_y3_s in
      let exp_res_y4 = Bls12_381.Fr.of_string exp_res_y4_s in
      let state = [|x1; x2; x3; x4; y1; y2; y3; y4|] in
      let ctxt =
        Bls12_381_hash.Permutation.Anemoi.(
          allocate_ctxt Parameters.security_128_state_size_8)
      in
      let () = Bls12_381_hash.Permutation.Anemoi.set_state ctxt state in
      let () = Bls12_381_hash.Permutation.Anemoi.apply_permutation ctxt in
      let output = Bls12_381_hash.Permutation.Anemoi.get_state ctxt in
      let res_x1, res_x2, res_x3, res_x4, res_y1, res_y2, res_y3, res_y4 =
        ( output.(0),
          output.(1),
          output.(2),
          output.(3),
          output.(4),
          output.(5),
          output.(6),
          output.(7) )
      in
      let res_x1_s = Bls12_381.Fr.to_string res_x1 in
      let res_x2_s = Bls12_381.Fr.to_string res_x2 in
      let res_x3_s = Bls12_381.Fr.to_string res_x3 in
      let res_x4_s = Bls12_381.Fr.to_string res_x4 in
      let res_y1_s = Bls12_381.Fr.to_string res_y1 in
      let res_y2_s = Bls12_381.Fr.to_string res_y2 in
      let res_y3_s = Bls12_381.Fr.to_string res_y3 in
      let res_y4_s = Bls12_381.Fr.to_string res_y4 in
      let is_eq =
        Bls12_381.Fr.eq res_x1 exp_res_x1
        && Bls12_381.Fr.eq res_x2 exp_res_x2
        && Bls12_381.Fr.eq res_x3 exp_res_x3
        && Bls12_381.Fr.eq res_x4 exp_res_x4
        && Bls12_381.Fr.eq res_y1 exp_res_y1
        && Bls12_381.Fr.eq res_y2 exp_res_y2
        && Bls12_381.Fr.eq res_y3 exp_res_y3
        && Bls12_381.Fr.eq res_y4 exp_res_y4
      in
      if not is_eq then
        Alcotest.failf
          "Expected result = (%s, %s, %s, %s, %s, %s, %s, %s), computed result \
           = (%s, %s, %s, %s, %s, %s, %s, %s), input = (%s, %s, %s, %s, %s, \
           %s, %s, %s)"
          exp_res_x1_s
          exp_res_x2_s
          exp_res_x3_s
          exp_res_x4_s
          exp_res_y1_s
          exp_res_y2_s
          exp_res_y3_s
          exp_res_y4_s
          res_x1_s
          res_x2_s
          res_x3_s
          res_x4_s
          res_y1_s
          res_y2_s
          res_y3_s
          res_y4_s
          x1_s
          x2_s
          x3_s
          x4_s
          y1_s
          y2_s
          y3_s
          y4_s)
    vectors

let test_state_functions () =
  let l = 5 + Random.int 10 in
  let mds =
    Array.init l (fun _ -> Array.init l (fun _ -> Bls12_381.Fr.random ()))
  in
  let state_size = 2 * l in
  let state = Array.init state_size (fun _ -> Bls12_381.Fr.random ()) in
  let parameters =
    (Bls12_381_hash.Permutation.Anemoi.Parameters.create
       128
       state_size
       mds [@warning "-3"])
  in
  let ctxt = Bls12_381_hash.Permutation.Anemoi.allocate_ctxt parameters in
  let () = Bls12_381_hash.Permutation.Anemoi.set_state ctxt state in
  let output = Bls12_381_hash.Permutation.Anemoi.get_state ctxt in
  if not (Array.for_all2 Bls12_381.Fr.eq state output) then
    Alcotest.failf
      "Exp: [%s], computed: [%s]"
      (String.concat
         "; "
         (List.map Bls12_381.Fr.to_string (Array.to_list state)))
      (String.concat
         "; "
         (List.map Bls12_381.Fr.to_string (Array.to_list output)))

let test_anemoi_generic_with_l_one_is_anemoi_jive128_1 () =
  let state_size = 2 in
  let state = Array.init state_size (fun _ -> Bls12_381.Fr.random ()) in
  let ctxt =
    Bls12_381_hash.Permutation.Anemoi.(
      allocate_ctxt Parameters.security_128_state_size_2)
  in
  let () = Bls12_381_hash.Permutation.Anemoi.set_state ctxt state in
  let () = Bls12_381_hash.Permutation.Anemoi.apply_permutation ctxt in
  let output = Bls12_381_hash.Permutation.Anemoi.get_state ctxt in
  assert (
    Bls12_381.Fr.eq
      (Bls12_381_hash.Permutation.Anemoi.jive128_1 state.(0) state.(1))
      Bls12_381.Fr.(state.(0) + state.(1) + output.(0) + output.(1)))

let test_compute_number_of_rounds () =
  assert (
    Bls12_381_hash.Permutation.Anemoi.Parameters.compute_number_of_rounds 2 128
    = 19) ;
  assert (
    Bls12_381_hash.Permutation.Anemoi.Parameters.compute_number_of_rounds 4 128
    = 12) ;
  assert (
    Bls12_381_hash.Permutation.Anemoi.Parameters.compute_number_of_rounds 6 128
    = 10) ;
  assert (
    Bls12_381_hash.Permutation.Anemoi.Parameters.compute_number_of_rounds 8 128
    = 10) ;
  assert (
    Bls12_381_hash.Permutation.Anemoi.Parameters.compute_number_of_rounds 2 256
    = 35) ;
  assert (
    Bls12_381_hash.Permutation.Anemoi.Parameters.compute_number_of_rounds 4 256
    = 20) ;
  assert (
    Bls12_381_hash.Permutation.Anemoi.Parameters.compute_number_of_rounds 6 256
    = 15) ;
  assert (
    Bls12_381_hash.Permutation.Anemoi.Parameters.compute_number_of_rounds 8 256
    = 14)

let test_anemoi_generate_constants () =
  let l = 1 in
  let nb_rounds = 19 in
  let exp_res =
    Bls12_381.Fr.
      [|
        of_string "39";
        of_string
          "41362478282768062297187132445775312675360473883834860695283235286481594490621";
        of_string
          "9548818195234740988996233204400874453525674173109474205108603996010297049928";
        of_string
          "25365440569177822667580105183435418073995888230868180942004497015015045856900";
        of_string
          "34023498397393406644117994167986720327178154686105264833093891093045919619309";
        of_string
          "38816051319719761886041858113129205506758421478656182868737326994635468402951";
        of_string
          "35167418087531820804128377095512663922179887277669504047069913414630376083753";
        of_string
          "25885868839756469722325652387535232478219821850603640827385444642154834700231";
        of_string
          "8867588811641202981080659274007552529205713737251862066053445622305818871963";
        of_string
          "36439756010140137556111047750162544185710881404522379792044818039722752946048";
        of_string
          "7788624504122357216765350546787885309160020166693449889975992574536033007374";
        of_string
          "3134147137704626983201116226440762775442116005053282329971088789984415999550";
        of_string
          "50252287380741824818995733304361249016282047978221591906573165442023106203143";
        of_string
          "48434698978712278012409706205559577163572452744833134361195687109159129985373";
        of_string
          "32960510617530186159512413633821386297955642598241661044178889571655571939473";
        of_string
          "12850897859166761094422335671106280470381427571695744605265713866647560628356";
        of_string
          "14578036872634298798382048587794204613583128573535557156943783762854124345644";
        of_string
          "21588109842058901916690548710649523388049643745013696896704903154857389904594";
        of_string
          "35731638686520516424752846654442973203189295883541072759390882351699754104989";
        of_string
          "14981678621464625851270783002338847382197300714436467949315331057125308909900";
        of_string
          "28253420209785428420233456008091632509255652343634529984400816700490470131093";
        of_string
          "51511939407083344002778208487678590135577660247075600880835916725469990319313";
        of_string
          "46291121544435738125248657675097664742296276807186696922340332893747842754587";
        of_string
          "3650460179273129580093806058710273018999560093475503119057680216309578390988";
        of_string
          "45802223370746268123059159806400152299867771061127345631244786118574025749328";
        of_string
          "11798621276624967315721748990709309216351696098813162382053396097866233042733";
        of_string
          "42372918959432199162670834641599336326433006968669415662488070504036922966492";
        of_string
          "52181371244193189669553521955614617990714056725501643636576377752669773323445";
        of_string
          "23791984554824031672195249524658580601428376029501889159059009332107176394097";
        of_string
          "33342520831620303764059548442834699069640109058400548818586964467754352720368";
        of_string
          "16791548253207744974576845515705461794133799104808996134617754018912057476556";
        of_string
          "11087343419860825311828133337767238110556416596687749174422888171911517001265";
        of_string
          "11931207770538477937808955037363240956790374856666237106403111503668796872571";
        of_string
          "3296943608590459582451043049934874894049468383833500962645016062634514172805";
        of_string
          "7080580976521357573320018355401935489220216583936865937104131954142364033647";
        of_string
          "25990144965911478244481527888046366474489820502460615136523859419965697796405";
        of_string
          "33907313384235729375566529911940467295099705980234607934575786561097199483218";
        of_string
          "25996950265608465541351207283024962044374873682152889814392533334239395044136";
      |]
  in
  let res =
    Bls12_381_hash.Permutation.Anemoi.Parameters.generate_constants nb_rounds l
  in
  assert (Array.for_all2 Bls12_381.Fr.eq exp_res res)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "The permutation Anemoi and the mode of operation Jive"
    [
      ( "From reference implementation",
        [
          test_case
            "Tests vectors from reference implementation"
            `Quick
            test_vectors_anemoi128_1;
        ] );
      ( "Generic instantiations",
        [
          test_case
            "l = 1 <==> jive128_1"
            `Quick
            test_anemoi_generic_with_l_one_is_anemoi_jive128_1;
          test_case
            "l = 2 -> tests vectors from reference implementation"
            `Quick
            test_vectors_anemoi128_2;
          test_case
            "l = 4 -> tests vectors from reference implementation"
            `Quick
            test_vectors_anemoi128_4;
          test_case
            "l = 3 -> tests vectors from reference implementation"
            `Quick
            test_vectors_anemoi128_3;
        ] );
      ( "Additional functions",
        [
          test_case
            "State initialisation and get state"
            `Quick
            test_state_functions;
          test_case "Constant generation" `Quick test_anemoi_generate_constants;
          test_case
            "Compute number of rounds"
            `Quick
            test_compute_number_of_rounds;
        ] );
    ]
