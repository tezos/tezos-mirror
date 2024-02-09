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
      (t2 int string ->. unit)
      @@ {eos|INSERT INTO executable_blueprints (id, payload) VALUES (?, ?)|eos}

    let select =
      (int ->? string)
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
      (t2 int string ->. unit)
      @@ {eos|INSERT INTO publishable_blueprints (id, payload) VALUES (?, ?)|eos}

    let select =
      (int ->? string)
      @@ {eos|SELECT (payload) FROM publishable_blueprints WHERE id = ?|eos}
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
        return_unit)
  in
  return {db_uri = uri}

module Executable_blueprints = struct
  open Ethereum_types

  let store {db_uri} (Qty number) blueprint =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.exec
      Q.Executable_blueprints.insert
      ( Z.to_int number,
        Data_encoding.Binary.to_string_exn
          Blueprint_types.payload_encoding
          blueprint )

  let find {db_uri} (Qty number) =
    let open Lwt_result_syntax in
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    let* blob = Db.find_opt Q.Executable_blueprints.select (Z.to_int number) in
    match blob with
    | Some blob ->
        let payload =
          Data_encoding.Binary.of_string_exn
            Blueprint_types.payload_encoding
            blob
        in
        return_some payload
    | None -> return_none
end

module Publishable_blueprints = struct
  open Ethereum_types

  let store {db_uri} (Qty number) blueprint =
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    Db.exec
      Q.Publishable_blueprints.insert
      ( Z.to_int number,
        Data_encoding.Binary.to_string_exn
          Blueprint_types.payload_encoding
          blueprint )

  let find {db_uri} (Qty number) =
    let open Lwt_result_syntax in
    with_caqti_error
    @@ Caqti_lwt_unix.with_connection db_uri
    @@ fun (module Db : Caqti_lwt.CONNECTION) ->
    let* blob = Db.find_opt Q.Publishable_blueprints.select (Z.to_int number) in
    match blob with
    | Some blob ->
        let payload =
          Data_encoding.Binary.of_string_exn
            Blueprint_types.payload_encoding
            blob
        in
        return_some payload
    | None -> return_none
end
