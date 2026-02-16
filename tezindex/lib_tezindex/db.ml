(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let maybe_create_tables db_pool =
  Caqti_lwt_unix.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
        (fun req ->
          Db.exec (Caqti_request.Infix.(Caqti_type.(unit ->. unit)) req) ())
        Sql_requests.create_tables)
    db_pool

let init_pool logger (config : Config.config) =
  let open Lwt_result_syntax in
  let ext = Filename.extension config.db_name in
  let uri =
    match ext with
    | ".sqlite" ->
        Uri.of_string
          ("sqlite3:" ^ Filename.concat config.basedir config.db_name)
    | s ->
        let () =
          Log.info logger (fun () ->
              Format.sprintf "Extension %s not supported" s)
        in
        assert false
  in
  let res = Caqti_lwt_unix.connect_pool ~env:Sql_requests.env uri in
  let show_error_and_exit e =
    let*! () = Lwt_io.eprintl (Caqti_error.show e) in
    assert false
  in
  match res with
  | Error e -> show_error_and_exit e
  | Ok pool ->
      let* () = maybe_create_tables pool in
      return pool
