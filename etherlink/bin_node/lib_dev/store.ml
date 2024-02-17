(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Filename.Infix

type t = {db_uri : Uri.t}

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std
  open Ethereum_types

  let level =
    custom
      ~encode:(fun (Qty x) -> Ok Z.(to_int x))
      ~decode:(fun x -> Ok (Qty Z.(of_int x)))
      int

  let payload =
    custom
      ~encode:(fun payload ->
        Ok
          (Data_encoding.Binary.to_string_exn
             Blueprint_types.payload_encoding
             payload))
      ~decode:(fun bytes ->
        Option.to_result ~none:"Not a valid blueprint payload"
        @@ Data_encoding.Binary.of_string_opt
             Blueprint_types.payload_encoding
             bytes)
      string

  let context_hash =
    custom
      ~encode:(fun hash -> Ok (Context_hash.to_b58check hash))
      ~decode:(fun bytes ->
        Option.to_result ~none:"Not a valid b58check encoded hash"
        @@ Context_hash.of_b58check_opt bytes)
      string

  module Executable_blueprints = struct
    let create_table =
      (unit ->. unit)
      @@ {eos|
      CREATE TABLE executable_blueprints (
        id SERIAL PRIMARY KEY,
        payload BLOB NOT NULL
      )
    |eos}

    let insert =
      (t2 level payload ->. unit)
      @@ {eos|INSERT INTO executable_blueprints (id, payload) VALUES (?, ?)|eos}

    let select =
      (level ->? payload)
      @@ {eos|SELECT (payload) FROM executable_blueprints WHERE id = ?|eos}
  end

  module Publishable_blueprints = struct
    let create_table =
      (unit ->. unit)
      @@ {eos|
      CREATE TABLE publishable_blueprints (
        id SERIAL PRIMARY KEY,
        payload BLOB NOT NULL
      )
    |eos}

    let insert =
      (t2 level payload ->. unit)
      @@ {eos|INSERT INTO publishable_blueprints (id, payload) VALUES (?, ?)|eos}

    let select =
      (level ->? payload)
      @@ {eos|SELECT (payload) FROM publishable_blueprints WHERE id = ?|eos}
  end

  module Context_hashes = struct
    let create_table =
      (unit ->. unit)
      @@ {eos|
      CREATE TABLE context_hashes (
        id SERIAL PRIMARY KEY,
        context_hash VARCHAR(52) NOT NULL
      )
    |eos}

    let insert =
      (t2 level context_hash ->. unit)
      @@ {eos|REPLACE INTO context_hashes (id, context_hash) VALUES (?, ?)|eos}

    let select =
      (level ->? context_hash)
      @@ {eos|SELECT (context_hash) FROM context_hashes WHERE id = ?|eos}

    let get_latest =
      (unit ->? t2 level context_hash)
      @@ {eos|SELECT id, context_hash FROM context_hashes ORDER BY id DESC LIMIT 1|eos}
  end
end

let with_caqti_error p =
  let open Lwt_syntax in
  let* p in
  match p with
  | Ok p -> return_ok p
  | Error err -> failwith "Caqti: %a" Caqti_error.pp err

let init ~data_dir =
  let open Lwt_result_syntax in
  with_caqti_error
  @@
  let path = data_dir // "store.sqlite" in
  let*! exists = Lwt_unix.file_exists path in
  let uri = Uri.of_string Format.(sprintf "sqlite3:%s" path) in
  let* () =
    when_ (not exists) @@ fun () ->
    Caqti_lwt_unix.with_connection
      uri
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        let* () = Db.exec Q.Publishable_blueprints.create_table () in
        let* () = Db.exec Q.Executable_blueprints.create_table () in
        let* () = Db.exec Q.Context_hashes.create_table () in
        return_unit)
  in
  return {db_uri = uri}

module Executable_blueprints = struct
  let store {db_uri} number blueprint =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.exec Q.Executable_blueprints.insert (number, blueprint)

  let find {db_uri} number =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find_opt Q.Executable_blueprints.select number
end

module Publishable_blueprints = struct
  let store {db_uri} number blueprint =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.exec Q.Publishable_blueprints.insert (number, blueprint)

  let find {db_uri} number =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find_opt Q.Publishable_blueprints.select number
end

module Context_hashes = struct
  let store {db_uri} number hash =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.exec Q.Context_hashes.insert (number, hash)

  let find {db_uri} number =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find_opt Q.Context_hashes.select number

  let find_latest {db_uri} =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.find_opt Q.Context_hashes.get_latest ()
end
