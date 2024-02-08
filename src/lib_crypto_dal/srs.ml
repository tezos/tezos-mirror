(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Error_monad
open Kzg.Bls
open Zcash_srs

type srs_verifier = {shards : G2.t; pages : G2.t; commitment : G2.t}

let max_verifier_srs_size = Srs_g1.size srs_g1

let get_verifier_srs2 max_srs_size get_srs2 ~max_polynomial_length
    ~page_length_domain ~shard_length =
  let shards = get_srs2 shard_length in
  let pages = get_srs2 page_length_domain in
  let commitment = get_srs2 (max_srs_size - max_polynomial_length) in
  {shards; pages; commitment}

module Internal_for_tests = struct
  let max_srs_size = 1 lsl 16

  let fake_srs_seed =
    Scalar.of_string
      "20812168509434597367146703229805575690060615791308155437936410982393987532344"

  let fake_srs ?(size = max_srs_size) () =
    Srs_g1.generate_insecure size fake_srs_seed

  let get_srs2 i = G2.mul G2.one (Scalar.pow fake_srs_seed (Z.of_int i))

  let get_verifier_srs2 = get_verifier_srs2 max_srs_size get_srs2

  let get_verifier_srs1 = fake_srs ~size:max_verifier_srs_size

  let is_in_srs2 _ = true
end

let max_srs_size = max_srs_g1_size

let get_srs2 i = List.assoc i srs_g2

let get_verifier_srs1 () = srs_g1

let get_verifier_srs2 = get_verifier_srs2 max_srs_size get_srs2

let is_in_srs2 i = List.mem_assoc i srs_g2

let ensure_srs_validity ~test ~mode ~slot_size ~page_size ~redundancy_factor
    ~number_of_shards =
  let open Result_syntax in
  let assert_result condition error_message =
    if not condition then fail (`Fail (error_message ())) else return_unit
  in
  let max_polynomial_length, _erasure_encoded_polynomial_length, shard_length =
    Parameters_check.compute_lengths
      ~redundancy_factor
      ~slot_size
      ~page_size
      ~number_of_shards
  in
  let min_g1, srs_g1_length =
    match mode with
    | `Prover when test ->
        (max_polynomial_length, Internal_for_tests.max_srs_size)
    | `Prover -> (max_polynomial_length, max_srs_size)
    | `Verifier -> (shard_length, max_verifier_srs_size)
  in
  let* () =
    assert_result
      (min_g1 <= srs_g1_length)
      (* The committed polynomials have degree t.max_polynomial_length - 1 at most,
         so t.max_polynomial_length coefficients. *)
      (fun () ->
        Format.asprintf
          "SRS on G1 size is too small. Expected more than %d. Got %d. Hint: \
           you can reduce the size of a slot."
          min_g1
          srs_g1_length)
  in
  let page_length_domain = Parameters_check.domain_length ~size:page_size in
  let max_srs_size, is_in_srs2 =
    if test then Internal_for_tests.(max_srs_size, is_in_srs2)
    else (max_srs_size, is_in_srs2)
  in
  let offset_monomial_degree = max_srs_size - max_polynomial_length in
  assert_result
    (is_in_srs2 shard_length
    && is_in_srs2 page_length_domain
    && is_in_srs2 offset_monomial_degree)
    (fun () ->
      Format.asprintf
        "SRS on should contain points of indices shard_length = %d, \
         page_length_domain = %d & offset_monomial_degree = %d."
        shard_length
        page_length_domain
        offset_monomial_degree)

exception Failed_to_load_trusted_setup of string

let read_srs ?len ~path srs_of_bigstring =
  let open Lwt_syntax in
  let to_bigstring ~path =
    let* fd = Lwt_unix.openfile path [Unix.O_RDONLY] 0o440 in
    Lwt.finalize
      (fun () ->
        match
          Lwt_bytes.map_file ~fd:(Lwt_unix.unix_file_descr fd) ~shared:false ()
        with
        | exception Unix.Unix_error (error_code, function_name, _) ->
            raise
              (Failed_to_load_trusted_setup
                 (Format.sprintf
                    "%s: Unix.Unix_error: %s"
                    function_name
                    (Unix.error_message error_code)))
        | exception e ->
            raise (Failed_to_load_trusted_setup (Printexc.to_string e))
        | res -> Lwt.return res)
      (fun () -> Lwt_unix.close fd)
  in
  let* srs_g1_bigstring = to_bigstring ~path in
  Lwt.return @@ srs_of_bigstring ?len srs_g1_bigstring

let read_srs_g1 ?len ~path () = read_srs ?len ~path Srs_g1.of_bigstring

let read_srs_g2 ?len ~path () = read_srs ?len ~path Srs_g2.of_bigstring

module Print = struct
  (* Bounds (in log₂)
     1 <= redundancy<= 4
     7 <= page size + (redundancy + 1) <= slot size <= 20
     5 <= page size <= slot size - (redundancy + 1) <= 18 - 5 = 13
     2 <= redundancy + 1 <= nb shards <= slot size - page size <= 15
  *)
  type range = {
    redundancy : int list;
    slot : int list;
    page : int list;
    shards : int list;
  }

  let _mainnet_params =
    {
      redundancy = [1; 2; 3; 4];
      slot = [15; 16; 17; 18; 19; 20];
      page = [12];
      shards = [11; 12];
    }

  let _additionnal_params =
    {redundancy = [1; 2; 3; 4]; slot = [15]; page = [12]; shards = [6]}

  let concat_map4 {slot; redundancy; page; shards} func =
    (* Ensure validity before computing actual value *)
    let f ~slot ~redundancy ~page ~shards =
      Parameters_check.ensure_validity_without_srs
        ~slot_size:(1 lsl slot)
        ~page_size:(1 lsl page)
        ~redundancy_factor:(1 lsl redundancy)
        ~number_of_shards:(1 lsl shards)
      |> function
      | Ok () -> func ~slot ~redundancy ~page ~shards
      | _ -> 0
    in
    List.concat_map
      (fun slot ->
        List.concat_map
          (fun redundancy ->
            List.concat_map
              (fun page ->
                List.map
                  (fun shards -> f ~slot ~redundancy ~page ~shards)
                  shards)
              page)
          redundancy)
      slot

  let generate_poly_lengths ~max_srs_size p =
    let page_srs =
      let values =
        List.map
          (fun page -> Parameters_check.domain_length ~size:(1 lsl page))
          p.page
      in
      values
    in
    let commitment_srs =
      concat_map4 p (fun ~slot ~redundancy:_ ~page ~shards:_ ->
          max_srs_size
          - Parameters_check.slot_as_polynomial_length
              ~page_size:(1 lsl page)
              ~slot_size:(1 lsl slot))
    in
    let shard_srs =
      concat_map4 p (fun ~slot ~redundancy ~page ~shards ->
          let max_polynomial_length =
            Parameters_check.slot_as_polynomial_length
              ~page_size:(1 lsl page)
              ~slot_size:(1 lsl slot)
          in
          let erasure_encoded_polynomial_length =
            (1 lsl redundancy) * max_polynomial_length
          in
          erasure_encoded_polynomial_length / (1 lsl shards))
    in
    let page_shards =
      List.sort_uniq (fun x y -> Int.compare y x) (page_srs @ shard_srs)
    in
    let max_srs1_needed = List.hd page_shards in
    (max_srs1_needed, List.sort_uniq Int.compare (page_shards @ commitment_srs))

  let _generate_all_poly_lengths ~max_srs_size =
    List.fold_left
      (fun (acc_size, acc_lengths) p ->
        let size, lengths = generate_poly_lengths ~max_srs_size p in
        (max acc_size size, List.sort_uniq Int.compare (acc_lengths @ lengths)))
      (0, [])

  (* run with
     let _ = Lwt_main.run
       @@ Srs.Print.(print_verifier_srs_from_file ~zcash_g1_path
            ~zcash_g2_path mainnet_params) in
  *)
  let _print_verifier_srs_from_file ?(max_srs_size = max_srs_size)
      ~zcash_g1_path ~zcash_g2_path params =
    let open Lwt_result_syntax in
    let srs_g1_size, lengths = generate_poly_lengths ~max_srs_size params in
    let* srs_g1 = read_srs_g1 ~len:srs_g1_size ~path:zcash_g1_path () in
    let* srs_g2 = read_srs_g2 ~path:zcash_g2_path () in
    let srs2 =
      List.map
        (fun i ->
          let g2 =
            Srs_g2.get srs_g2 i |> G2.to_compressed_bytes |> Hex.of_bytes
            |> Hex.show
          in
          Printf.sprintf "(%d, \"%s\")" i g2)
        lengths
    in
    let srs1 =
      List.init srs_g1_size (fun i ->
          Printf.sprintf
            "\"%s\""
            (Srs_g1.get srs_g1 i |> G1.to_compressed_bytes |> Hex.of_bytes
           |> Hex.show))
    in
    Printf.printf
      "\n\nlet srs_g1 = [|\n  %s\n|] |> read_srs_g1"
      (String.concat " ;\n  " @@ srs1) ;
    Printf.printf
      "\n\nlet srs_g2 = [\n  %s\n] |> read_srs_g2"
      (String.concat " ;\n  " @@ srs2) ;
    return_unit
end
