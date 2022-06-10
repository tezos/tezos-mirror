open QCheck_alcotest
open QCheck2
open Chunked_byte_vector

let create_works =
  Test.make ~name:"create works" Gen.ui64 (fun len ->
      let vector = create len in
      length vector = len)

let store_load_byte_works =
  Test.make ~name:"store_byte and load_byte work" Gen.string (fun str ->
      let bytes = Bytes.of_string str in
      let len = Int64.of_int (Bytes.length bytes) in
      let vector = create len in
      let mapping =
        List.init (Bytes.length bytes) (fun i ->
            let index = Int64.of_int i in
            let byte = Bytes.get_uint8 bytes i in
            store_byte vector index byte ;
            (index, byte))
      in
      List.for_all (fun (i, c) -> load_byte vector i = c) mapping)

let grow_works =
  Test.make
    ~name:"grow works"
    Gen.(pair string ui64)
    (fun (init_str, grow_len) ->
      let vector = of_string init_str in
      let check_contents () =
        List.init (String.length init_str) (fun i ->
            let index = Int64.of_int i in
            load_byte vector index = Char.code (String.get init_str i))
        |> List.for_all Fun.id
      in
      let check1 = check_contents () in
      grow vector grow_len ;
      let check2 = check_contents () in
      let check3 =
        Int64.(length vector = add grow_len (of_int (String.length init_str)))
      in
      check1 && check2 && check3)

let internal_num_pages_edge_case =
  let test () =
    let open Alcotest in
    check int64 "exact value" 0L (Chunk.num_needed 0L) ;
    check int64 "exact value" 1L (Chunk.num_needed Chunk.size) ;
    check int64 "exact value" 1L (Chunk.num_needed (Int64.pred Chunk.size)) ;
    check int64 "exact value" 2L (Chunk.num_needed (Int64.succ Chunk.size))
  in
  ("internal: num_pages edge case", `Quick, test)

let tests =
  [
    to_alcotest create_works;
    to_alcotest store_load_byte_works;
    to_alcotest grow_works;
    internal_num_pages_edge_case;
  ]
