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

  let get_srs1 i = G1.mul G1.one (Scalar.pow fake_srs_seed (Z.of_int i))

  let get_srs2 i = G2.mul G2.one (Scalar.pow fake_srs_seed (Z.of_int i))

  let get_verifier_srs2 = get_verifier_srs2 max_srs_size get_srs2

  let get_verifier_srs1 = fake_srs ~size:max_verifier_srs_size

  let is_in_srs2 _ = true
end

let max_srs_size = max_srs_g1_size

let get_srs1 = Srs_g1.get srs_g1

let get_srs2 i = List.assoc i srs_g2

let get_verifier_srs1 () = srs_g1

let get_verifier_srs2 = get_verifier_srs2 max_srs_size get_srs2

let is_in_srs2 i = List.mem_assoc i srs_g2

module type S = sig
  val max_srs_size : int

  val is_in_srs2 : int -> bool

  val get_verifier_srs1 : unit -> Srs_g1.t

  val get_verifier_srs2 :
    max_polynomial_length:int ->
    page_length_domain:int ->
    shard_length:int ->
    srs_verifier

  val get_srs1 : int -> G1.t
end

module Print = struct
  (* This code is duplicated in cryptobox.ml *)
  module Read = struct
    type error += Failed_to_load_trusted_setup of string

    let () =
      register_error_kind
        `Permanent
        ~id:"dal.node.SRS_loading_failed"
        ~title:"Trusted setup loading failed"
        ~description:"Trusted setup failed to load"
        ~pp:(fun ppf msg ->
          Format.fprintf ppf "Trusted setup failed to load: %s" msg)
        Data_encoding.(obj1 (req "msg" string))
        (function
          | Failed_to_load_trusted_setup parameter -> Some parameter | _ -> None)
        (fun parameter -> Failed_to_load_trusted_setup parameter)
      [@@coverage off]

    let read_srs ?len ~srs_path srs_of_bigstring =
      let open Lwt_result_syntax in
      let to_bigstring ~path =
        let open Lwt_syntax in
        let* fd = Lwt_unix.openfile path [Unix.O_RDONLY] 0o440 in
        Lwt.finalize
          (fun () ->
            return
              (match
                 Lwt_bytes.map_file
                   ~fd:(Lwt_unix.unix_file_descr fd)
                   ~shared:false
                   ()
               with
              | exception Unix.Unix_error (error_code, function_name, _) ->
                  Error
                    [
                      Failed_to_load_trusted_setup
                        (Format.sprintf
                           "%s: Unix.Unix_error: %s"
                           function_name
                           (Unix.error_message error_code));
                    ]
              | exception e ->
                  Error [Failed_to_load_trusted_setup (Printexc.to_string e)]
              | res -> Ok res))
          (fun () -> Lwt_unix.close fd)
      in
      let* srs_bigstring = to_bigstring ~path:srs_path in
      match srs_of_bigstring ?len srs_bigstring with
      | Error (`End_of_file s) ->
          tzfail (Failed_to_load_trusted_setup ("EOF: " ^ s))
      | Error (`Invalid_point p) ->
          tzfail
            (Failed_to_load_trusted_setup (Printf.sprintf "Invalid point %i" p))
      | Ok srs -> return srs

    (* FIXME https://gitlab.com/tezos/tezos/-/issues/3400

       An integrity check is run to ensure the validity of the files. *)
    let initialisation_parameters_from_file ?srs_size_log2 ~zcash_g1_path () =
      let len = Option.map (fun i -> 1 lsl i) srs_size_log2 in
      read_srs ?len ~srs_path:zcash_g1_path Srs_g1.of_bigstring

    let read_srs_g1 ?len ~zcash_g1_path () =
      read_srs ?len ~srs_path:zcash_g1_path Srs_g1.of_bigstring

    let read_srs_g2 ?len ~zcash_g2_path () =
      read_srs ?len ~srs_path:zcash_g2_path Srs_g2.of_bigstring
  end

  (* Bounds (in log₂)
     1 <= redundancy<= 4
     7 <= page size + (redundancy + 1) <= slot size <= 20
     5 <= page size <= slot size - (redundancy + 1) <= 18 - 5 = 13
     2 <= redundancy + 1 <= nb shards <= slot size - page size <= 15
     we call range the number of logs to go through
     we call offset the index to start (included)
  *)
  type range = {
    redundancy_range : int;
    redundancy_offset : int;
    slot_range : int;
    slot_offset : int;
    page_range : int;
    page_offset : int;
    shard_range : int;
    shard_offset : int;
  }

  let small_params_for_tests =
    {
      redundancy_range = 4;
      redundancy_offset = 1;
      slot_range = 7;
      slot_offset = 8;
      page_range = 4;
      page_offset = 5;
      shard_range = 15;
      shard_offset = 1;
    }

  let mainnet_params =
    {
      redundancy_range = 4;
      redundancy_offset = 1;
      slot_range = 6;
      slot_offset = 15;
      page_range = 1;
      page_offset = 12;
      shard_range = 2;
      shard_offset = 11;
    }

  let generate_poly_lengths ~max_srs_size p =
    let page_srs =
      let values =
        List.init p.page_range (fun i ->
            Parameters_check.domain_length ~size:(1 lsl (i + p.page_offset)))
      in
      values
    in
    let commitment_srs =
      List.init p.slot_range (fun slot_size ->
          let slot_size = slot_size + p.slot_offset in
          List.init p.redundancy_range (fun redundancy ->
              let redundancy = redundancy + p.redundancy_offset in
              List.init p.page_range (fun page_size ->
                  let page_size = page_size + p.page_offset in
                  let res =
                    max_srs_size
                    - Parameters_check.slot_as_polynomial_length
                        ~page_size:(1 lsl page_size)
                        ~slot_size:(1 lsl slot_size)
                  in
                  Printf.printf
                    "\nslot : %d    page : %d    R : %d    res : %d"
                    slot_size
                    page_size
                    redundancy
                    res ;
                  res)))
      |> List.concat |> List.concat
    in
    let shard_srs =
      List.init p.slot_range (fun slot_size ->
          let slot_size = slot_size + p.slot_offset in
          List.init p.redundancy_range (fun redundancy ->
              let redundancy = redundancy + p.redundancy_offset in
              List.init p.page_range (fun page_size ->
                  let page_size = page_size + p.page_offset in
                  let shard_offset = p.shard_offset in
                  List.init p.shard_range (fun nb_shards ->
                      let nb_shards = nb_shards + shard_offset in
                      redundancy
                      * Parameters_check.slot_as_polynomial_length
                          ~page_size:(1 lsl page_size)
                          ~slot_size:(1 lsl slot_size)
                      / (1 lsl nb_shards)))))
      |> List.concat |> List.concat |> List.concat
    in
    let page_shards =
      List.sort_uniq (fun x y -> Int.compare y x) (page_srs @ shard_srs)
    in
    let max_srs1_needed = List.hd page_shards in
    (max_srs1_needed, List.sort_uniq Int.compare (page_shards @ commitment_srs))

  (* run with
     let _ = Lwt_main.run
       @@ Srs_verifier.Print.(print_verifier_srs_from_file ~zcash_g1_path
            ~zcash_g2_path mainnet_params) in
  *)
  let print_verifier_srs_from_file ?(max_srs_size = max_srs_size) ~zcash_g1_path
      ~zcash_g2_path params =
    let open Lwt_result_syntax in
    let srs_g1_size, lengths = generate_poly_lengths ~max_srs_size params in
    let* srs_g1 = Read.read_srs_g1 ~len:srs_g1_size ~zcash_g1_path () in
    let* srs_g2 = Read.read_srs_g2 ~zcash_g2_path () in
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
