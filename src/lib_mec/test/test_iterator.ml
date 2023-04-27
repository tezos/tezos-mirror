open Mec.Utils

let test_one_byte () =
  (* Single byte *)
  let test_vectors =
    [
      ("\x00", [0; 0; 0; 0; 0; 0; 0; 0]);
      ("\x0f", [1; 1; 1; 1; 0; 0; 0; 0]);
      ("\x10", [0; 0; 0; 0; 1; 0; 0; 0]);
    ]
  in
  List.iter
    (fun (b, expected_l) ->
      let iterator = Iterator.Bit.of_bytes_le (Bytes.of_string b) in
      List.iter
        (fun exp_b -> assert (Iterator.Bit.next iterator = Some exp_b))
        expected_l ;
      assert (Iterator.Bit.next iterator = None))
    test_vectors

let test_from_bool_list () =
  (* Single byte *)
  let test_vectors =
    [
      ([true; true; true], [1; 1; 1]);
      ([false; true; true], [0; 1; 1]);
      ([true; true; false; true; false], [1; 1; 0; 1; 0]);
    ]
  in
  List.iter
    (fun (b, expected_l) ->
      let iterator = Iterator.Bit.of_bool_list b in
      List.iter
        (fun exp_b -> assert (Iterator.Bit.next iterator = Some exp_b))
        expected_l ;
      assert (Iterator.Bit.next iterator = None))
    test_vectors

let test_multiple_bytes () =
  (* The individual bytes are given in big endian and are concatenated in the
     order of appearance in the string
     \x10\x00 represents 0x0010 in big endian, i.e. 16.
  *)
  let test_vectors =
    [
      ("\x00\x00", [0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]);
      ("\x0f\x00", [1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]);
      ("\x10\x00", [0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0]);
      ( "\x10\x00\x10",
        [0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0; 0]
      );
      ( "\x10\x00\x10\x2f",
        [
          (* 0x10 *)
          0;
          0;
          0;
          0;
          1;
          0;
          0;
          0;
          (* 0x00 *)
          0;
          0;
          0;
          0;
          0;
          0;
          0;
          0;
          (* 0x10 *)
          0;
          0;
          0;
          0;
          1;
          0;
          0;
          0;
          (* 0x2f *)
          1;
          1;
          1;
          1;
          0;
          1;
          0;
          0;
        ] );
    ]
  in
  List.iter
    (fun (b, expected_l) ->
      let iterator = Iterator.Bit.of_bytes_le (Bytes.of_string b) in
      List.iter
        (fun exp_b -> assert (Iterator.Bit.next iterator = Some exp_b))
        expected_l ;
      assert (Iterator.Bit.next iterator = None))
    test_vectors

let test_is_processed () =
  let test_vectors =
    [
      ([], [(0, true); (1, true); (Random.int 1000, true)]);
      ([true], [(0, false); (1, true); (2, true); (2 + Random.int 1000, true)]);
      ([true; false; true], [(Random.int 3, false); (3 + Random.int 1000, true)]);
      (let n = Random.int 1000 in
       ( List.init n (fun _ -> Random.bool ()),
         [(Random.int n, false); (n, true); (n + 1 + Random.int 1000, true)] ));
      ([true; false; true], [(Random.int 3, false); (3 + Random.int 1000, true)]);
    ]
  in
  List.iter
    (fun (v, iterations) ->
      List.iter
        (fun (n, expected_value) ->
          let iterator = Iterator.Bit.of_bool_list v in
          for _ = 0 to n - 1 do
            ignore @@ Iterator.Bit.next iterator
          done ;
          assert (Iterator.Bit.is_processed iterator = expected_value))
        iterations)
    test_vectors

let test_get_chunk () =
  let test_vectors =
    [
      ([], 5, [List.init 5 (fun _ -> 0)]);
      ([true; false], 1, [[1]; [0]]);
      ([true; false], 2, [[1; 0]; [0; 0]]);
      ([true; false; true], 2, [[1; 0]; [1; 0]]);
      ([true; false; true; true], 2, [[1; 0]; [1; 1]]);
      ([true; false; true; true], 3, [[1; 0; 1]; [1; 0; 0]]);
    ]
  in
  List.iter
    (fun (l, chunk_size, exp_values) ->
      let iterator = Iterator.Bit.of_bool_list l in
      List.iter
        (fun exp_value ->
          let chunk = Iterator.Bit.get_chunk iterator chunk_size in
          assert (chunk = exp_value))
        exp_values)
    test_vectors

let () =
  Alcotest.run
    ~__FILE__
    "Iterator"
    [
      ( "Bytes",
        [
          Alcotest.test_case "One byte" `Quick test_one_byte;
          Alcotest.test_case "Multiple bytes" `Quick test_multiple_bytes;
        ] );
      ( "Bool list",
        [Alcotest.test_case "Test vectors" `Quick test_from_bool_list] );
      ( "Utils functions",
        [Alcotest.test_case "is processed" `Quick test_is_processed] );
      ("get chunk", [Alcotest.test_case "get chnk" `Quick test_get_chunk]);
    ]
