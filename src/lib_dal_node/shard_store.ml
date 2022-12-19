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

type error += Resource_not_found of string

let () =
  register_error_kind
    `Permanent
    ~id:"dal.node.resource_not_found"
    ~title:"Resource not found"
    ~description:"Resource not found at the given path"
    ~pp:(fun ppf s ->
      Format.fprintf ppf "Resource not found at the given path: %s" s)
    Data_encoding.(obj1 (req "path" string))
    (function Resource_not_found s -> Some s | _ -> None)
    (fun s -> Resource_not_found s)

module Mutexes = struct
  type mutex = Lwt_idle_waiter.t

  include
    Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong)
      (struct
        type t = string

        let equal = String.equal

        let hash = Hashtbl.hash
      end)

  let mem t k = Option.is_some @@ find_opt t k

  let add t k =
    let mutex = Lwt_idle_waiter.create () in
    replace t k mutex ;
    mutex
end

type t = {data_dir : string; mutexes : Mutexes.mutex Mutexes.t}

let get_data_dir t = t.data_dir

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

let init ~max_mutexes data_dir =
  let open Lwt_result_syntax in
  let*! () = mkdir_if_not_exists data_dir in
  let mutexes = Mutexes.create max_mutexes in
  return {data_dir; mutexes}

let commitment_dir store commitment =
  let commitment = Cryptobox.Commitment.to_b58check commitment in
  Filename.concat store.data_dir commitment

let with_mutex store filepath f =
  let open Lwt_result_syntax in
  let mutex =
    match Mutexes.find_opt store.mutexes filepath with
    | None -> Mutexes.add store.mutexes filepath
    | Some mutex -> mutex
  in
  let*! v = f mutex in
  let () =
    match v with Ok _ -> () | Error _ -> Mutexes.remove store.mutexes filepath
  in
  Lwt.return v

let write_shards store commitment shards =
  let open Lwt_result_syntax in
  let commitment_dir = commitment_dir store commitment in
  let*! () = mkdir_if_not_exists commitment_dir in
  shards |> Cryptobox.IntMap.to_seq
  |> Seq.iter_es (fun (index, share) ->
         let filepath = Filename.concat commitment_dir (string_of_int index) in
         if Mutexes.mem store.mutexes filepath then return_unit
         else
           with_mutex store filepath @@ fun mutex ->
           Lwt_idle_waiter.force_idle mutex @@ fun () ->
           let*? content = bytes_of_share share in
           (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4289
              check if slot and shards already exists *)
           let*! r =
             Lwt_utils_unix.with_atomic_open_out filepath (fun fd ->
                 Lwt_utils_unix.write_bytes fd content)
           in
           Lwt.return @@ Result.bind_error r Lwt_utils_unix.tzfail_of_io_error)

let read_share_from_disk ~commitment ~name store ~share_size filepath =
  let open Lwt_result_syntax in
  let*! share =
    with_mutex store filepath @@ fun mutex ->
    catch_es @@ fun () ->
    let*! share =
      Lwt_idle_waiter.task mutex @@ fun () ->
      Lwt_utils_unix.with_open_in filepath @@ fun fd ->
      let share = Bytes.create share_size in
      let*! () = Lwt_utils_unix.read_bytes fd share in
      Lwt.return share
    in
    Lwt.return @@ Result.bind_error share Lwt_utils_unix.tzfail_of_io_error
  in
  match share with
  | Ok share -> Lwt.return @@ share_of_bytes share
  | Error [Lwt_utils_unix.Io_error e] when e.action = `Open ->
      fail
        [
          Resource_not_found
            (Filename.concat (Cryptobox.Commitment.to_b58check commitment) name);
        ]
  | Error e -> fail e

let read_shards ~share_size store commitment =
  let open Lwt_result_syntax in
  let dir = commitment_dir store commitment in
  let file_stream = Lwt_unix.files_of_directory dir in
  let rec read acc =
    let*! elt = catch_s @@ fun () -> Lwt_stream.get file_stream in
    let* elt =
      match elt with
      | Ok r -> return r
      | Error [Exn (Unix.Unix_error (_, "opendir", _))] ->
          fail
            [Resource_not_found (Cryptobox.Commitment.to_b58check commitment)]
      | Error e -> fail e
    in
    match elt with
    | None -> return acc
    | Some "." | Some ".." -> read acc
    | Some shard_file ->
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/4289
           handle error cases for [int_of_string] *)
        let shard_index = int_of_string shard_file in
        let filepath = Filename.concat dir shard_file in
        let* share =
          read_share_from_disk
            ~commitment
            ~name:shard_file
            store
            ~share_size
            filepath
        in
        read (Cryptobox.IntMap.add shard_index share acc)
  in
  read Cryptobox.IntMap.empty

let read_shards_subset ~share_size store commitment shards =
  let open Lwt_result_syntax in
  let dir = commitment_dir store commitment in
  List.map_es
    (fun index ->
      let name = string_of_int index in
      let filepath = Filename.concat dir name in
      let* share =
        read_share_from_disk store ~commitment ~name ~share_size filepath
      in
      return Cryptobox.{index; share})
    shards

let read_shard ~share_size store commitment shard_index =
  let open Lwt_result_syntax in
  let dir = commitment_dir store commitment in
  let name = string_of_int shard_index in
  let filepath = Filename.concat dir name in
  let* share =
    read_share_from_disk ~commitment ~name store ~share_size filepath
  in
  return Cryptobox.{index = shard_index; share}
