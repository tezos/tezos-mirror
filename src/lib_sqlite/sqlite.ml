(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type error += Caqti_error of string | File_already_exists of string

(* Error registration *)
let () =
  register_error_kind
    `Permanent
    ~id:"caqti_error"
    ~title:"Error raised by Caqti"
    ~description:"Caqti raised an error while processing a SQL statement"
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "Caqti raised an error while processing a SQL statement: %s"
        msg)
    Data_encoding.(obj1 (req "caqti_error" string))
    (function Caqti_error err -> Some err | _ -> None)
    (fun err -> Caqti_error err)

let () =
  register_error_kind
    `Permanent
    ~id:"caqti_file_already_exists"
    ~title:"File already exists"
    ~description:"Raise an error when the file already exists"
    ~pp:(fun ppf path -> Format.fprintf ppf "The file %s already exists" path)
    Data_encoding.(obj1 (req "file_already_exists" string))
    (function File_already_exists path -> Some path | _ -> None)
    (fun path -> File_already_exists path)

type pool =
  ( Caqti_lwt.connection,
    [Caqti_error.connect | `K_error of tztrace] )
  Caqti_lwt_unix.Pool.t

type sqlite_journal_mode = Wal | Other

module Db = struct
  let wrap_caqti_lwt_result (p : ('a, Caqti_error.t) result Lwt.t) :
      'a tzresult Lwt.t =
    let open Lwt_result_syntax in
    let*! p in
    match p with
    | Ok p -> return p
    | Error err -> fail [Caqti_error (Caqti_error.show err)]

  let wrap_caqti_result (p : ('a, Caqti_error.t) result) : 'a tzresult Lwt.t =
    let open Lwt_result_syntax in
    match p with
    | Ok p -> return p
    | Error err -> fail [Caqti_error (Caqti_error.show err)]

  let use_pool (pool : pool) (k : Caqti_lwt.connection -> 'a tzresult Lwt.t) =
    let open Lwt_result_syntax in
    let*! res =
      Caqti_lwt_unix.Pool.use
        (fun conn ->
          let*! res = k conn in
          match res with
          | Ok res -> return res
          | Error err -> fail (`K_error err))
        pool
    in
    match res with
    | Ok err -> return err
    | Error (`K_error err) -> fail err
    | Error (#Caqti_error.connect as err) ->
        fail [Caqti_error (Caqti_error.show err)]

  let start (module Db : Caqti_lwt.CONNECTION) =
    wrap_caqti_lwt_result @@ Db.start ()

  let commit (module Db : Caqti_lwt.CONNECTION) =
    wrap_caqti_lwt_result @@ Db.commit ()

  let rollback (module Db : Caqti_lwt.CONNECTION) =
    wrap_caqti_lwt_result @@ Db.rollback ()

  let exec (module Db : Caqti_lwt.CONNECTION) req arg =
    wrap_caqti_lwt_result @@ Db.exec req arg

  let find (module Db : Caqti_lwt.CONNECTION) req arg =
    wrap_caqti_lwt_result @@ Db.find req arg

  let find_opt (module Db : Caqti_lwt.CONNECTION) req arg =
    wrap_caqti_lwt_result @@ Db.find_opt req arg

  let collect_list (module Db : Caqti_lwt.CONNECTION) req arg =
    wrap_caqti_lwt_result @@ Db.collect_list req arg

  let rev_collect_list (module Db : Caqti_lwt.CONNECTION) req arg =
    wrap_caqti_lwt_result @@ Db.rev_collect_list req arg

  let fold (module Db : Caqti_lwt.CONNECTION) req f x acc =
    wrap_caqti_lwt_result @@ Db.fold req f x acc

  let fold_s (module Db : Caqti_lwt.CONNECTION) req f x acc =
    wrap_caqti_lwt_result @@ Db.fold_s req f x acc

  let iter_s (module Db : Caqti_lwt.CONNECTION) req f x =
    wrap_caqti_lwt_result @@ Db.iter_s req f x
end

type t = Pool : {db_pool : pool} -> t

type conn =
  | Raw_connection of (module Caqti_lwt.CONNECTION)
  | Ongoing_transaction of (module Caqti_lwt.CONNECTION)

let assert_in_transaction conn =
  match conn with
  | Raw_connection _ -> assert false
  | Ongoing_transaction _ -> ()

let with_connection conn k =
  match conn with
  | Ongoing_transaction conn -> k conn
  | Raw_connection conn -> k conn

let with_transaction conn k =
  let open Lwt_result_syntax in
  match conn with
  | Raw_connection conn -> (
      let* () = Db.start conn in
      let*! res =
        Lwt.catch
          (fun () -> k (Ongoing_transaction conn))
          (fun exn -> fail_with_exn exn)
      in
      match res with
      | Ok x ->
          let* () = Db.commit conn in
          return x
      | Error err ->
          let* () = Db.rollback conn in
          fail err)
  | Ongoing_transaction _ ->
      failwith "Internal error: attempting to perform a nested transaction"

let use (Pool {db_pool}) k =
  Db.use_pool db_pool @@ fun conn -> k (Raw_connection conn)

(* Internal queries *)
module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let journal_mode =
    custom
      ~encode:(function Wal -> Ok "wal" | Other -> Ok "delete")
      ~decode:(function "wal" -> Ok Wal | _ -> Ok Other)
      string

  let vacuum_self = (unit ->. unit) @@ {|VACUUM main|}

  let vacuum_request = (string ->. unit) @@ {|VACUUM main INTO ?|}

  module Journal_mode = struct
    let get = (unit ->! journal_mode) @@ {|PRAGMA journal_mode|}

    (* It does not appear possible to write a request {|PRAGMA journal_mode=?|}
       accepted by caqti, sadly. *)

    let set_wal = (unit ->! journal_mode) @@ {|PRAGMA journal_mode=wal|}
  end
end

let uri path perm =
  let write_perm =
    match perm with `Read_only -> false | `Read_write -> true
  in
  Uri.of_string Format.(sprintf "sqlite3:%s?write=%b" path write_perm)

let set_wal_journal_mode store =
  let open Lwt_result_syntax in
  with_connection store @@ fun conn ->
  let* current_mode = Db.find conn Q.Journal_mode.get () in
  when_ (current_mode <> Wal) @@ fun () ->
  let* _wal = Db.find conn Q.Journal_mode.set_wal () in
  return_unit

let vacuum ~conn ~output_db_file =
  let open Lwt_result_syntax in
  let*! exists = Lwt_unix.file_exists output_db_file in
  let*? () = error_when exists (File_already_exists output_db_file) in
  let* () =
    with_connection conn @@ fun conn ->
    Db.exec conn Q.vacuum_request output_db_file
  in
  let* db_pool =
    Db.wrap_caqti_result
    @@ Caqti_lwt_unix.connect_pool (uri output_db_file `Read_write)
  in
  let* () = use (Pool {db_pool}) set_wal_journal_mode in
  return_unit

let vacuum_self ~conn =
  with_connection conn @@ fun conn -> Db.exec conn Q.vacuum_self ()

let init ~path ~perm migration_code =
  let open Lwt_result_syntax in
  let* db_pool =
    Db.wrap_caqti_result @@ Caqti_lwt_unix.connect_pool (uri path perm)
  in
  let store = Pool {db_pool} in
  use store @@ fun conn ->
  let* () = set_wal_journal_mode conn in
  let* () = with_transaction conn migration_code in
  return store

let close (Pool {db_pool}) = Caqti_lwt_unix.Pool.drain db_pool
