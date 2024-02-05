(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(*

  This implementation of the Blueprints Store saves the blueprint in a simple
  file system hierarchy (to avoid over-populating one directory in particular).

  The [AAAABBBBCCCC]th blueprint will be stored under the path
  [data_dir/blueprints/AAAA/BBBB/CCCC] in a binary format.

 *)

open Ethereum_types
open Filename.Infix

type t = string

let make ~data_dir = data_dir // "blueprint"

let blueprint_relative_path number =
  let to_string i = Format.sprintf "%04i" i in
  let share = 10_000 in
  let base = number / share in
  let file = number mod share |> to_string in
  let sub_directory = base / share |> to_string in
  let sub_sub_directory = base mod share |> to_string in
  (sub_directory // sub_sub_directory, file)

type error += Cannot_store_blueprint

let store store_path blueprint (Qty number) =
  let open Lwt_syntax in
  let number = Z.to_int number in
  let str =
    Data_encoding.Binary.to_string_exn
      Blueprint_types.payload_encoding
      blueprint
  in

  let blueprint_directory, file = blueprint_relative_path number in

  let* _ = Lwt_utils_unix.create_dir (store_path // blueprint_directory) in

  let* res =
    Lwt_utils_unix.(
      with_open_out
        (store_path // blueprint_directory // file)
        (fun fd -> write_string fd str))
  in

  return (Result.map_error (fun _ -> [Cannot_store_blueprint]) res)

let find store_path (Qty number) =
  let open Lwt_syntax in
  let number = Z.to_int number in

  let blueprint_directory, file = blueprint_relative_path number in

  Lwt.catch
    (fun () ->
      let* str =
        Lwt_utils_unix.read_file (store_path // blueprint_directory // file)
      in

      return
      @@ Some
           (Data_encoding.Binary.of_string_exn
              Blueprint_types.payload_encoding
              str))
    (fun _exn -> return_none)
