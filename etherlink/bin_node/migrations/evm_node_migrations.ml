(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Sqlite.Request
open Caqti_type.Std

let re_t = Re.(compile @@ Posix.re "[0-9]{3}_([a-z0-9_]+).sql")

module type S = sig
  val name : string

  val up : (unit, unit, [`Zero]) Sqlite.Request.t list
end

type migration = (module S)

let make_migration path =
  let read_exn path =
    match Migrations.read path with
    | Some content -> content
    | None -> raise (Invalid_argument "read_exn")
  in

  let migration_name =
    let open Re in
    let (group : Group.t) = exec re_t path in
    Group.get group 1
  in

  let migration_step = ( @@ ) (unit ->. unit) in
  let make_migration_requests str =
    String.split ';' str
    |> List.filter_map (fun i ->
           match String.trim i with "" -> None | x -> Some (migration_step x))
  in

  (module struct
    let name = migration_name

    let up = make_migration_requests (read_exn path)
  end : S)

let migrations version =
  List.take_n (version + 1) Migrations.file_list |> List.map make_migration
