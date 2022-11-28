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

type t = {data_dir : string}

let bytes_of_share s =
  Data_encoding.(Binary.to_bytes Cryptobox.share_encoding s)
  |> Result.map_error (fun e ->
         [Tezos_base.Data_encoding_wrapper.Encoding_error e])

let share_of_bytes b =
  Data_encoding.(Binary.of_bytes Cryptobox.share_encoding b)
  |> Result.map_error (fun e ->
         [Tezos_base.Data_encoding_wrapper.Decoding_error e])

let mkdir_if_not_exists dirpath =
  Lwt.catch
    (fun () -> Lwt_unix.mkdir dirpath 0o755)
    (function
      | Unix.Unix_error (Unix.EEXIST, "mkdir", _) -> Lwt.return_unit
      | e -> raise e)

let init data_dir =
  let open Lwt_syntax in
  let* () = mkdir_if_not_exists data_dir in
  Lwt.return {data_dir}

let commitment_dir store commitment =
  let commitment = Cryptobox.Commitment.to_b58check commitment in
  Filename.concat store.data_dir commitment

let write_shards store commitment shards =
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4289
     check if slot and shards already exists *)
  let open Lwt_result_syntax in
  let commitment_dir = commitment_dir store commitment in
  let*! () = mkdir_if_not_exists commitment_dir in
  shards |> Cryptobox.IntMap.to_seq
  |> Seq.iter_es (fun (index, share) ->
         let filepath = Filename.concat commitment_dir (string_of_int index) in
         let*? content = bytes_of_share share in
         let*! r =
           Lwt_utils_unix.with_atomic_open_out filepath (fun fd ->
               Lwt_utils_unix.write_bytes fd content)
         in
         Lwt.return @@ Result.bind_error r Lwt_utils_unix.tzfail_of_io_error)

let read_share_from_disk ~share_size filepath =
  let open Lwt_result_syntax in
  let*! share =
    Lwt_utils_unix.with_open_in filepath (fun fd ->
        let share = Bytes.create share_size in
        let*! () = Lwt_utils_unix.read_bytes fd share in
        Lwt.return share)
  in
  let*? share = Result.bind_error share Lwt_utils_unix.tzfail_of_io_error in
  Lwt.return @@ share_of_bytes share

let read_shards ~share_size store commitment =
  let open Lwt_result_syntax in
  let dir = commitment_dir store commitment in
  let file_stream = Lwt_unix.files_of_directory dir in
  let rec read acc =
    let*! elt = Lwt_stream.get file_stream in
    match elt with
    | None -> return acc
    | Some "." | Some ".." -> read acc
    | Some shard_file ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4289
           handle error cases for [int_of_string] *)
        let shard_index = int_of_string shard_file in
        let filepath = Filename.concat dir shard_file in
        let* share = read_share_from_disk ~share_size filepath in
        read (Cryptobox.IntMap.add shard_index share acc)
  in
  read Cryptobox.IntMap.empty

let read_shards_subset ~share_size store commitment shards =
  let open Lwt_result_syntax in
  let dir = commitment_dir store commitment in
  List.map_es
    (fun index ->
      let filepath = Filename.concat dir (string_of_int index) in
      let* share = read_share_from_disk ~share_size filepath in
      return Cryptobox.{index; share})
    shards

let read_shard ~share_size store commitment shard_index =
  let open Lwt_result_syntax in
  let dir = commitment_dir store commitment in
  let filepath = Filename.concat dir (string_of_int shard_index) in
  let* share = read_share_from_disk ~share_size filepath in
  return Cryptobox.{index = shard_index; share}
