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

exception Failed_to_load_trusted_setup of string

exception Failed_to_write_uncompressed_srs of string

(* Magic string for marking serialized uncompressed srs files.
 * The U means uncompressed.
 * The version is string encoded to avoid endianness issues *)
let magic = "TZ_SRS_U01"

let read_uncompressed_srs ?len ~srsu_g1_path ~srsu_g2_path () =
  let open Lwt_syntax in
  let header_sz = String.length magic in
  let do_read ~path ty =
    Lwt_utils_unix.with_open_in path @@ fun fd ->
    (* Setup a header ourselves with magic/version, because [Repr] does not
     * offer guarantees concerning the backward compatibility of serialization. *)
    let header = Bytes.create header_sz in
    let* nr =
      Lwt.catch
        (fun () -> Lwt_unix.read fd header 0 header_sz)
        (function
          | Unix.Unix_error (error_code, function_name, _) ->
              Lwt.reraise
                (Failed_to_load_trusted_setup
                   (Format.sprintf
                      "%s: Unix.Unix_error: %s"
                      function_name
                      (Unix.error_message error_code)))
          | e ->
              Lwt.reraise (Failed_to_load_trusted_setup (Printexc.to_string e)))
    in
    let () =
      if nr <> header_sz then
        raise
          (Failed_to_load_trusted_setup
             (Format.asprintf "Missing header: %s" path))
    in
    (* Check the input file is in the expected format
     * by comparing the magic string present at the beginning of the file *)
    let () =
      if String.equal (Bytes.to_string header) magic then ()
      else
        raise
          (Failed_to_load_trusted_setup
             (Format.asprintf "Not an uncompressed srs: %s" path))
    in
    (* Map file into memory to access it efficiently *)
    match
      Lwt_bytes.map_file
        ~pos:(Int64.of_int header_sz)
        ~fd:(Lwt_unix.unix_file_descr fd)
        ~shared:false
        ()
    with
    | exception Unix.Unix_error (error_code, function_name, _) ->
        raise
          (Failed_to_load_trusted_setup
             (Format.sprintf
                "%s: Unix.Unix_error for %s: %s"
                function_name
                path
                (Unix.error_message error_code)))
    | exception e -> raise (Failed_to_load_trusted_setup (Printexc.to_string e))
    | res -> (
        let of_bin_string = Repr.unstage (Repr.of_bin_string ty) in
        (* Unfortunate copy, we should be able to serialize bigstringaf
         * directly instead *)
        let res = Lwt_bytes.to_string res in
        match of_bin_string res with
        | Ok srs -> return srs
        | Error (`Msg msg) ->
            raise
              (Failed_to_load_trusted_setup
                 (Format.sprintf "%s: Failed to deserialize: %s" path msg)))
  in
  let open Lwt_result_syntax in
  let* srs_g1 = do_read ~path:srsu_g1_path Srs_g1.t in
  let* srs_g2 = do_read ~path:srsu_g2_path Srs_g2.t in
  (* Returns a copy of the SRS with only the first [len] points *)
  let truncate of_array to_array fn_len srs len =
    (* Get length of the SRS *)
    let g_len = fn_len srs in
    if g_len < len then
      raise
        (Failed_to_load_trusted_setup
           (Format.sprintf "Unexpected srs size: %d" g_len))
    else if g_len > len then
      (* Truncates the srs if len smaller than file *)
      (* Note: this also makes some memory copies. *)
      let arr = to_array srs in
      let arr = Array.sub arr 0 len in
      of_array arr
    else srs
  in
  (* Checks the SRS have a sufficient number of points according to [len]
     and drop points not needed if the serialized SRS contains more than [len] *)
  let srs_g1, srs_g2 =
    Option.fold
      ~none:(srs_g1, srs_g2)
      ~some:(fun len ->
        let srs_g1 =
          truncate Srs_g1.of_array Srs_g1.to_array Srs_g1.size srs_g1 len
        in
        let srs_g2 =
          truncate Srs_g2.of_array Srs_g2.to_array Srs_g2.size srs_g2 len
        in
        (srs_g1, srs_g2))
      len
  in
  return (srs_g1, srs_g2)

let read_srs ?len ~srs_g1_path ~srs_g2_path () =
  let open Lwt_result_syntax in
  let to_bigstring ~path =
    let*! fd = Lwt_unix.openfile path [Unix.O_RDONLY] 0o440 in
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
  let*! srs_g1_bigstring = to_bigstring ~path:srs_g1_path in
  let*! srs_g2_bigstring = to_bigstring ~path:srs_g2_path in
  let*? srs_g1 = Srs_g1.of_bigstring srs_g1_bigstring ?len in
  let*? srs_g2 = Srs_g2.of_bigstring srs_g2_bigstring ?len in
  return (srs_g1, srs_g2)

type srs_verifier = {shards : G2.t; pages : G2.t; commitment : G2.t}

let max_verifier_srs_size = Srs_g1.size srs_g1

let get_verifier_srs2_aux max_srs_size get_srs2 ~max_polynomial_length
    ~page_length_domain ~shard_length =
  let shards = get_srs2 shard_length in
  let pages = get_srs2 page_length_domain in
  let commitment = get_srs2 (max_srs_size - max_polynomial_length) in
  {shards; pages; commitment}

let max_srs_size = max_srs_g1_size

let get_srs2 i = List.assoc i srs_g2

let get_verifier_srs1 () = srs_g1

let get_verifier_srs2 = get_verifier_srs2_aux max_srs_size get_srs2

let is_in_srs2 i = List.mem_assoc i srs_g2

module Internal_for_tests = struct
  let max_srs_size = 1 lsl 9

  let fake_srs_seed =
    Scalar.of_string
      "20812168509434597367146703229805575690060615791308155437936410982393987532344"

  let compute_fake_srs ?(size = max_srs_size) gen () = gen size fake_srs_seed

  let get_srs2 i = G2.mul G2.one (Scalar.pow fake_srs_seed (Z.of_int i))

  let get_verifier_srs2 = get_verifier_srs2_aux max_srs_size get_srs2

  let get_verifier_srs1 =
    compute_fake_srs ~size:max_verifier_srs_size Srs_g1.generate_insecure

  let is_in_srs2 _ = true

  let fake_srs1 = Lazy.from_fun (compute_fake_srs Srs_g1.generate_insecure)

  let fake_srs2 = Lazy.from_fun (compute_fake_srs Srs_g2.generate_insecure)

  (* Writes a uncompressed SRS given SRS₁ and SRS₂.
   * The serialisation format is based on Repr, but is prepended by a header
   * containing [magic], ie 'TZ_SRS_U01', for tezos srs uncompressed version 01.
   *)
  let write_uncompressed_srs ~srsu_g1_path ~srsu_g2_path (srs_g1, srs_g2) =
    let open Lwt_result_syntax in
    let write ~path srs ty =
      let open Tezos_stdlib_unix.Lwt_utils_unix in
      with_open_out path @@ fun fd ->
      let*! () =
        Lwt.catch
          (fun () ->
            let chan = Lwt_io.of_fd ~mode:Output fd in
            let*! () = Lwt_io.write chan magic in
            let to_bin_string = Repr.(unstage (to_bin_string ty)) in
            let bin = to_bin_string srs in
            let*! () = Lwt_io.write chan bin in
            let*! () = Lwt_io.flush chan in
            Lwt.return_unit)
          (function
            | Unix.Unix_error (error_code, function_name, _) ->
                Lwt.reraise
                  (Failed_to_load_trusted_setup
                     (Format.sprintf
                        "%s: Unix.Unix_error: %s"
                        function_name
                        (Unix.error_message error_code)))
            | e ->
                Lwt.reraise
                  (Failed_to_write_uncompressed_srs (Printexc.to_string e)))
      in
      Lwt.return_unit
    in
    let* () = write ~path:srsu_g1_path srs_g1 Srs_g1.t in
    let* () = write ~path:srsu_g2_path srs_g2 Srs_g2.t in
    return_unit

  (* Generates SRS uncompressed from zcash srs
   * Serialization format contains a magic, see [write_uncompressed_srs].
   * This also tests the reading function in order to check it is equal once
   * deserialized. *)
  let _generate_srsu_uncompressed_serialized_from_zcash_file ~srs_g1_path
      ~srs_g2_path ~srsu_g1_path ~srsu_g2_path =
    let open Lwt_result_syntax in
    let*! r = read_srs ~srs_g1_path ~srs_g2_path () in
    let* srs_g1, srs_g2 =
      match r with
      | Ok (srs_g1, srs_g2) -> Lwt.return_ok (srs_g1, srs_g2)
      | Error (`End_of_file msg) ->
          Lwt.return_error
            (error_of_exn
               (Failed_to_load_trusted_setup
                  (Format.sprintf "End_of_file: %s" msg)))
      | Error (`Invalid_point i) ->
          Lwt.return_error
            (error_of_exn
               (Failed_to_load_trusted_setup
                  (Format.sprintf "Invalid point: %d" i)))
    in
    let srs_size = Srs_g1.size srs_g1 in
    let*! result =
      write_uncompressed_srs ~srsu_g1_path ~srsu_g2_path (srs_g1, srs_g2)
    in
    let*? () =
      Result.map_error (fun err -> Lwt_utils_unix.Io_error err) result
    in
    let t1 = Unix.gettimeofday () in
    let*! r =
      read_uncompressed_srs ~len:srs_size ~srsu_g1_path ~srsu_g2_path ()
    in
    let*? srs_g1', srs_g2' =
      Result.map_error (fun err -> Lwt_utils_unix.Io_error err) r
    in
    let t2 = Unix.gettimeofday () in
    let g_equal f_size f_get f_eq g g' =
      if f_size g <> f_size g' then false
      else
        let rec loop i =
          if i < f_size g then
            let point1 = f_get g i in
            let point2 = f_get g' i in
            let res = f_eq point1 point2 in
            if res then loop (i + 1) else false
          else true
        in
        loop 0
    in
    assert (g_equal Srs_g1.size Srs_g1.get G1.eq srs_g1 srs_g1') ;
    assert (g_equal Srs_g2.size Srs_g2.get G2.eq srs_g2 srs_g2') ;
    Format.eprintf "Timing reading uncompressed_srs: %f\n%!" (t2 -. t1) ;
    return_unit

  module Print = struct
    (* Bounds (following inequalities are given for log₂ for simplicity)
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

    let concat_map4 {slot; redundancy; page; shards} func =
      (* Ensure validity before computing actual value *)
      let f ~slot ~redundancy ~page ~shards =
        Parameters_check.ensure_validity_without_srs
          ~slot_size:slot
          ~page_size:page
          ~redundancy_factor:redundancy
          ~number_of_shards:shards
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
            (fun page -> Parameters_check.domain_length ~size:page)
            p.page
        in
        values
      in
      let commitment_srs =
        concat_map4 p (fun ~slot ~redundancy:_ ~page ~shards:_ ->
            max_srs_size
            - Parameters_check.slot_as_polynomial_length
                ~page_size:page
                ~slot_size:slot)
      in
      let shard_srs =
        concat_map4 p (fun ~slot ~redundancy ~page ~shards ->
            let max_polynomial_length =
              Parameters_check.slot_as_polynomial_length
                ~page_size:page
                ~slot_size:slot
            in
            let erasure_encoded_polynomial_length =
              redundancy * max_polynomial_length
            in
            erasure_encoded_polynomial_length / shards)
      in
      let page_shards =
        List.sort_uniq (fun x y -> Int.compare y x) (page_srs @ shard_srs)
      in
      let max_srs1_needed = List.hd page_shards in
      ( max_srs1_needed,
        List.sort_uniq Int.compare (page_shards @ commitment_srs)
        |> List.filter (fun i -> i > 0) )

    let generate_all_poly_lengths ~max_srs_size =
      List.fold_left
        (fun (acc_size, acc_lengths) p ->
          let size, lengths = generate_poly_lengths ~max_srs_size p in
          (max acc_size size, List.sort_uniq Int.compare (acc_lengths @ lengths)))
        (0, [])
  end

  let print_verifier_srs_from_file ?(max_srs_size = Zcash_srs.max_srs_g1_size)
      ~srs_g1_path ~srs_g2_path ~dest_path () =
    (* Some "historical" parameters that may be used in tests *)
    let base_params =
      Print.
        {
          redundancy = [1; 2; 3; 4] |> List.map (Int.shift_left 1);
          slot = [15; 16; 17; 18; 19; 20] |> List.map (Int.shift_left 1);
          page = [12] |> List.map (Int.shift_left 1);
          shards = [11; 12] |> List.map (Int.shift_left 1);
        }
    in
    (* (current and future) Mainnet parameters *)
    let default_params =
      Print.
        {
          slot = [126_944; 380_832];
          page = [3967];
          redundancy = [8];
          shards = [512];
        }
    in
    (* Some additionnal parameters used in tests *)
    let test_params =
      Print.
        {
          slot = [63_472; 126_944; 190_416; 253_888; 380_832];
          page = [3967];
          redundancy = [2; 4; 8];
          shards = [256];
        }
    in
    (* Some additionnal parameters that are powers of 2 used in tests *)
    let test_params_pow_2 =
      Print.
        {
          redundancy = [3] |> List.map (Int.shift_left 1);
          slot = [11; 15] |> List.map (Int.shift_left 1);
          page = [7; 8] |> List.map (Int.shift_left 1);
          shards = [6; 8] |> List.map (Int.shift_left 1);
        }
    in
    let open Lwt_result_syntax in
    let srs_g1_size, lengths =
      Print.generate_all_poly_lengths
        ~max_srs_size
        [base_params; default_params; test_params; test_params_pow_2]
    in
    let* srs_g1, srs_g2 = read_srs ~srs_g1_path ~srs_g2_path () in
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
    let oc = open_out dest_path in
    Printf.fprintf
      oc
      "\n\nlet srs_g1 = [|\n  %s\n|] |> read_srs_g1"
      (String.concat " ;\n  " @@ srs1) ;
    Printf.fprintf
      oc
      "\n\nlet srs_g2 = [\n  %s\n] |> read_srs_g2"
      (String.concat " ;\n  " @@ srs2) ;
    close_out oc ;
    return_unit
end

let ensure_srs_validity ~is_fake ~mode ~slot_size ~page_size ~redundancy_factor
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
    | `Prover when is_fake ->
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
          "The size of the SRS on G1 is too small. Expected more than %d. Got \
           %d. Hint: you can reduce the size of a slot."
          min_g1
          srs_g1_length)
  in
  let page_length_domain = Parameters_check.domain_length ~size:page_size in
  let max_srs_size, is_in_srs2 =
    if is_fake then Internal_for_tests.(max_srs_size, is_in_srs2)
    else (max_srs_size, is_in_srs2)
  in
  let offset_monomial_degree = max_srs_size - max_polynomial_length in
  assert_result
    (is_in_srs2 shard_length
    && is_in_srs2 page_length_domain
    && is_in_srs2 offset_monomial_degree)
    (fun () ->
      Format.asprintf
        "The verifier SRS on G2 should contain points for indices shard_length \
         = %d, page_length_domain = %d & offset_monomial_degree = %d. Hint: \
         you can add new points to the SRS (to do that, use the function \
         Srs.Internal_for_tests.Print.print_verifier_srs_from_file)."
        shard_length
        page_length_domain
        offset_monomial_degree)
