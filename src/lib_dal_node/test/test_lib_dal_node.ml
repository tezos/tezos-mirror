(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_dal_node_lib

let parameters =
  Cryptobox.
    {
      number_of_shards = 256;
      slot_size = 1 lsl 16;
      redundancy_factor = 4;
      page_size = 4096;
    }

let initialisation_parameters =
  Cryptobox.Internal_for_tests.initialisation_parameters_from_slot_size
    ~slot_size:parameters.slot_size

let () = Cryptobox.Internal_for_tests.load_parameters initialisation_parameters

let dal, parameters =
  match Cryptobox.make parameters with
  | Ok cryptobox -> (cryptobox, parameters)
  | Error (`Fail str) -> Stdlib.failwith ("DAL INIT ERROR: " ^ str)

let shards_from_bytes dal b =
  let polynomial =
    match Cryptobox.polynomial_from_slot dal b with
    | Ok v -> v
    | Error _ -> Stdlib.failwith "DAL ERROR: polynomial_from_slot failed"
  in
  let commitment = Cryptobox.commit dal polynomial in
  let shards = Cryptobox.shards_from_polynomial dal polynomial in
  (commitment, shards)

let with_store f =
  let open Lwt_result_syntax in
  Lwt_utils_unix.with_tempdir "dal_shard_store_dir" @@ fun base_dir ->
  let* store =
    Shard_store.init ~max_mutexes:50 (Filename.concat base_dir "shard_store")
  in
  f dal parameters store

let test_read_fail () =
  with_store @@ fun dal parameters store ->
  let open Lwt_result_syntax in
  let share_size = Cryptobox.encoded_share_size dal in
  let commitment, _ =
    shards_from_bytes dal (Bytes.make parameters.slot_size 'a')
  in
  let*! failed_shards = Shard_store.read_shards ~share_size store commitment in
  let () =
    match failed_shards with
    | Error [Shard_store.Resource_not_found s]
      when s = Cryptobox.Commitment.to_b58check commitment ->
        ()
    | _ -> assert false
  in
  let*! failed_shard = Shard_store.read_shard ~share_size store commitment 0 in
  let () =
    match failed_shard with
    | Error [Shard_store.Resource_not_found s]
      when s = Filename.concat (Cryptobox.Commitment.to_b58check commitment) "0"
      ->
        ()
    | _ -> assert false
  in
  return_unit

let test_wrong_shard_size () =
  with_store @@ fun dal parameters store ->
  let open Lwt_result_syntax in
  let commitment, shards =
    shards_from_bytes dal (Bytes.make parameters.slot_size 'a')
  in
  let* () = Shard_store.write_shards store commitment shards in
  let*! failed_shards =
    Shard_store.read_shards ~share_size:22 store commitment
  in
  let () =
    match failed_shards with
    | Error [Tezos_base.Data_encoding_wrapper.Decoding_error _s] -> ()
    | _ -> assert false
  in
  return_unit

module IntMap = Map.Make (Int)

let test_rw () =
  with_store @@ fun dal parameters store ->
  let open Lwt_result_syntax in
  let share_size = Cryptobox.encoded_share_size dal in
  let commitment, shards =
    shards_from_bytes dal (Bytes.make parameters.slot_size 'a')
  in
  let* () = Shard_store.write_shards store commitment shards in
  let* shards_extracted =
    Shard_store.read_shards ~share_size store commitment
  in
  print_endline
  @@ Format.sprintf "%d %d" (Seq.length shards) (Seq.length shards_extracted) ;
  let map_shards =
    Seq.map (fun ({index; share} : Cryptobox.shard) -> (index, share))
  in
  assert (
    IntMap.(
      equal
        ( = )
        (of_seq (map_shards shards_extracted))
        (of_seq (map_shards shards)))) ;
  return_unit

let tests_store =
  ( "Test Store",
    [
      Tztest.tztest "Read/write" `Quick test_rw;
      Tztest.tztest "Read unknown shard/commitement" `Quick test_read_fail;
      Tztest.tztest "Read wrong share size" `Quick test_wrong_shard_size;
    ] )

let () = Alcotest_lwt.run "Shard_store" [tests_store] |> Lwt_main.run
