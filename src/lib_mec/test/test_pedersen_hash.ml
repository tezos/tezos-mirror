open Mec.Curve
open Mec.Hash
open Mec.Utils

let test_vector_from_zcash_primitives () =
  let open Test_vector_pedersen_hash in
  let vectors =
    List.map
      (fun (b, (u, v)) ->
        ( b,
          Jubjub.AffineEdwards.from_coordinates_exn
            ~u:(Jubjub.AffineEdwards.Base.of_string u)
            ~v:(Jubjub.AffineEdwards.Base.of_string v) ))
      vectors
  in
  List.iter
    (fun (input, expected_output) ->
      let iterator = Iterator.Bit.of_bool_list input in
      let output = PedersenHash.Zcash.hash iterator in
      if not (Jubjub.AffineEdwards.eq output expected_output) then
        Alcotest.failf
          "On input [%s] (length = %d), expected output (u=%s, v=%s), computed \
           output (u=%s, v=%s)"
          (String.concat
             ", "
             (List.map (fun b -> if b then "1" else "0") input))
          (List.length input)
          (Jubjub.AffineEdwards.Base.to_string
          @@ Jubjub.AffineEdwards.get_u_coordinate expected_output)
          (Jubjub.AffineEdwards.Base.to_string
          @@ Jubjub.AffineEdwards.get_v_coordinate expected_output)
          (Jubjub.AffineEdwards.Base.to_string
          @@ Jubjub.AffineEdwards.get_u_coordinate output)
          (Jubjub.AffineEdwards.Base.to_string
          @@ Jubjub.AffineEdwards.get_v_coordinate output))
    vectors

let test_zcash_bitstring_too_long () =
  let n = Random.int 1_000_000 in
  let max_bitstring_zcash = 6 * 3 * 63 in
  let bitstring = List.init (n + max_bitstring_zcash + 1) (fun _ -> false) in
  let iterator = Iterator.Bit.of_bool_list bitstring in
  try
    ignore @@ PedersenHash.Zcash.hash iterator ;
    assert false
  with
  | Invalid_argument _ -> ()
  | _ -> assert false

let () =
  Alcotest.run
    ~verbose:true
    "Pedersen Hash Zcash"
    [
      ( "Vectors",
        [
          Alcotest.test_case
            "Test vector from zcash primitives"
            `Quick
            test_vector_from_zcash_primitives;
        ] );
      ( "Too long bitstrings",
        [
          Alcotest.test_case
            "Too long bitstrings, not enough generators"
            `Quick
            test_zcash_bitstring_too_long;
        ] );
    ]
