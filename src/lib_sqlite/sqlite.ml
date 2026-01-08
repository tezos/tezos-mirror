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

exception Connection_error of Caqti_error.load_or_connect

module Pool = struct
  type pool_conn = {conn : Caqti_lwt.connection; mutable use_count : int}

  type t = pool_conn Lwt_pool.t

  let validate ~max_use_count
      ({conn = (module Db : Caqti_lwt.CONNECTION); _} as p) =
    let open Lwt_syntax in
    p.use_count <- p.use_count + 1 ;
    match max_use_count with
    | Some max when p.use_count > max -> return_false
    | _ -> Db.validate ()

  let check {conn = (module Db : Caqti_lwt.CONNECTION); _} is_ok =
    Db.check is_ok

  let dispose {conn = (module Db : Caqti_lwt.CONNECTION); _} = Db.disconnect ()

  let connect ?register uri =
    let open Lwt_syntax in
    let* res = Caqti_lwt_unix.connect uri in
    match res with
    | Ok conn ->
        let (module Conn) = conn in
        (match (register, Conn.driver_connection) with
        | None, _ -> ()
        | Some register, Some (Caqti_driver_sqlite3.Driver_connection db) ->
            register db
        | _ -> assert false) ;
        return {conn; use_count = 0}
    | Error e -> Lwt.fail (Connection_error e)

  let create ?max_use_count ?register size uri =
    Lwt_pool.create
      size
      ~validate:(validate ~max_use_count)
      ~check
      ~dispose
      (fun () -> connect ?register uri)
end

type sqlite_journal_mode = Wal | Other

type t = {
  db_pool : Pool.t;
  add_attrs :
    'a.
    ('a -> Opentelemetry.key_value list) ->
    'a ->
    Opentelemetry.Scope.t option ->
    unit;
  trace :
    'a.
    ?trace_id:Opentelemetry.Trace_id.t ->
    ?parent:Opentelemetry.Span_id.t ->
    ?scope:Opentelemetry.Scope.t ->
    string ->
    (Opentelemetry.Scope.t option -> 'a Lwt.t) ->
    'a Lwt.t;
}

module Request = struct
  open Caqti_request.Infix

  type ('a, 'b, +'m) t = {
    req : ('a, 'b, 'm) Caqti_request.t;
    name : string option;
    op_name : string option;
    table : string option;
    query : string;
    attrs : 'a -> Opentelemetry.Span.key_value list;
  }

  let op_name_of_query q =
    match Tezos_stdlib.TzString.split ' ' ~limit:1 (String.trim q) with
    | op :: _ -> Some op
    | _ -> None

  let shorten_name s =
    if String.starts_with ~prefix:"Octez" s then
      match Tezos_stdlib.TzString.split '.' ~limit:1 s |> List.rev with
      | f :: _ -> f
      | [] -> s
    else s

  let ( ->. ) t u ?name ?table ?(attrs = Fun.const []) ?oneshot query =
    let req = ( ->. ) ?oneshot t u query in
    let name = Option.map shorten_name name in
    {req; query; name; op_name = op_name_of_query query; table; attrs}

  let ( ->! ) t u ?name ?table ?(attrs = Fun.const []) ?oneshot query =
    let req = ( ->! ) ?oneshot t u query in
    let name = Option.map shorten_name name in
    {req; query; name; op_name = op_name_of_query query; table; attrs}

  let ( ->? ) t u ?name ?table ?(attrs = Fun.const []) ?oneshot query =
    let req = ( ->? ) ?oneshot t u query in
    let name = Option.map shorten_name name in
    {req; query; name; op_name = op_name_of_query query; table; attrs}

  let ( ->* ) t u ?name ?table ?(attrs = Fun.const []) ?oneshot query =
    let req = ( ->* ) ?oneshot t u query in
    let name = Option.map shorten_name name in
    {req; query; name; op_name = op_name_of_query query; table; attrs}
end

module Db = struct
  open Request

  type conn = {
    conn : (module Caqti_lwt.CONNECTION);
    add_attrs :
      'a.
      ('a -> Opentelemetry.key_value list) ->
      'a ->
      Opentelemetry.Scope.t option ->
      unit;
    trace :
      'a.
      ?trace_id:Opentelemetry.Trace_id.t ->
      ?parent:Opentelemetry.Span_id.t ->
      ?scope:Opentelemetry.Scope.t ->
      string ->
      (Opentelemetry.Scope.t option -> 'a Lwt.t) ->
      'a Lwt.t;
  }

  let wrap_caqti_lwt_result (p : ('a, Caqti_error.t) result Lwt.t) :
      'a tzresult Lwt.t =
    let open Lwt_result_syntax in
    let*! p in
    match p with
    | Ok p -> return p
    | Error err -> fail [Caqti_error (Caqti_error.show err)]

  let use_pool (pool : Pool.t) (k : Caqti_lwt.connection -> 'a tzresult Lwt.t) =
    let open Lwt_result_syntax in
    Lwt.catch
      (fun () -> Lwt_pool.use pool (fun p -> k p.Pool.conn))
      (function
        | Connection_error err -> tzfail (Caqti_error (Caqti_error.show err))
        | e -> fail_with_exn e)

  let trace_with trace ~op_name name f =
    trace ?trace_id:None ?parent:None ?scope:None name @@ fun scope ->
    match scope with
    | None -> f None
    | Some scope ->
        Opentelemetry.Scope.add_attrs scope (fun () ->
            [("db.operation.name", `String op_name)]) ;
        f (Some scope)

  let trace_req trace ?scope ?op_name fallback_name req f =
    let name = Option.value req.name ~default:fallback_name in
    trace ?trace_id:None ?parent:None ?scope name @@ fun scope ->
    match scope with
    | None -> f None
    | Some scope ->
        let attrs () =
          let op_name = Option.either req.op_name op_name in
          let attr1 =
            Option.map (fun s -> ("db.operation.name", `String s)) op_name
            |> Option.to_list
          in
          let attr2 =
            Option.map (fun s -> ("db.collection.name", `String s)) req.table
            |> Option.to_list
          in
          let attr3 = [("db.query.text", `String req.query)] in
          attr1 @ attr2 @ attr3
        in
        Opentelemetry.Scope.add_attrs scope attrs ;
        f (Some scope)

  let start {conn = (module Db); trace; _} =
    trace_with trace ~op_name:"start" "Sqlite.start" @@ fun _ ->
    wrap_caqti_lwt_result @@ Db.start ()

  let commit {conn = (module Db); trace; _} =
    trace_with trace ~op_name:"COMMIT" "Sqlite.commit" @@ fun _ ->
    wrap_caqti_lwt_result @@ Db.commit ()

  let rollback {conn = (module Db); trace; _} =
    trace_with trace ~op_name:"ROLLBACK" "Sqlite.rollback" @@ fun _ ->
    wrap_caqti_lwt_result @@ Db.rollback ()

  let exec ?scope {conn = (module Db); trace; add_attrs} req arg =
    trace_req ?scope trace "Sqlite.Db.exec" req @@ fun scope ->
    add_attrs req.attrs arg scope ;
    wrap_caqti_lwt_result @@ Db.exec req.req arg

  let find ?scope {conn = (module Db); trace; add_attrs} req arg =
    trace_req ?scope trace "Sqlite.Db.find" ~op_name:"find" req @@ fun scope ->
    add_attrs req.attrs arg scope ;
    wrap_caqti_lwt_result @@ Db.find req.req arg

  let find_opt ?scope {conn = (module Db); trace; add_attrs} req arg =
    trace_req ?scope trace "Sqlite.Db.find_opt" ~op_name:"find" req
    @@ fun scope ->
    add_attrs req.attrs arg scope ;
    wrap_caqti_lwt_result @@ Db.find_opt req.req arg

  let collect_list ?scope {conn = (module Db); trace; add_attrs} req arg =
    trace_req ?scope trace "Sqlite.Db.collect_list" ~op_name:"collect" req
    @@ fun scope ->
    add_attrs req.attrs arg scope ;
    wrap_caqti_lwt_result @@ Db.collect_list req.req arg

  let rev_collect_list ?scope {conn = (module Db); trace; add_attrs} req arg =
    trace_req ?scope trace "Sqlite.Db.rev_collect_list" ~op_name:"collect" req
    @@ fun scope ->
    add_attrs req.attrs arg scope ;
    wrap_caqti_lwt_result @@ Db.rev_collect_list req.req arg

  let fold ?scope {conn = (module Db); trace; _} req f x acc =
    trace_req ?scope trace "Sqlite.Db.fold" ~op_name:"iter" req @@ fun _ ->
    wrap_caqti_lwt_result @@ Db.fold req.req f x acc

  let fold_s ?scope {conn = (module Db); trace; _} req f x acc =
    trace_req ?scope trace "Sqlite.Db.fold_s" ~op_name:"iter" req @@ fun _ ->
    wrap_caqti_lwt_result @@ Db.fold_s req.req f x acc

  let iter_s ?scope {conn = (module Db); trace; _} req f x =
    trace_req ?scope trace "Sqlite.Db.iter_s" ~op_name:"iter" req @@ fun _ ->
    wrap_caqti_lwt_result @@ Db.iter_s req.req f x
end

type conn = Raw_connection of Db.conn | Ongoing_transaction of Db.conn

let no_trace ?trace_id:_ ?parent:_ ?scope:_ _ f = f None

let no_add_attrs _ _ _ = ()

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

let use {db_pool; trace; add_attrs} k =
  Db.use_pool db_pool @@ fun conn -> k (Raw_connection {conn; trace; add_attrs})

(* Internal queries *)
module Q = struct
  open Request
  open Caqti_type.Std

  let journal_mode =
    custom
      ~encode:(function Wal -> Ok "wal" | Other -> Ok "delete")
      ~decode:(function "wal" -> Ok Wal | _ -> Ok Other)
      string

  let vacuum_self =
    (unit ->. unit) ~name:"Sqlite.vacuum_self" @@ {|VACUUM main|}

  let vacuum_request =
    (string ->. unit) ~name:"Sqlite.vacuum_request" @@ {|VACUUM main INTO ?|}

  module Journal_mode = struct
    let get =
      (unit ->! journal_mode) ~name:"Sqlite.Journal_mode.get"
      @@ {|PRAGMA journal_mode|}

    (* It does not appear possible to write a request {|PRAGMA journal_mode=?|}
       accepted by caqti, sadly. *)

    let set_wal =
      (unit ->! journal_mode) ~name:"Sqlite.Journal_mode.set_wal"
      @@ {|PRAGMA journal_mode=wal|}
  end
end

type perm = Read_only of {pool_size : int} | Read_write

let uri path perm =
  let write_perm =
    match perm with Read_only _ -> false | Read_write -> true
  in
  Uri.of_string Format.(sprintf "sqlite3:%s?write=%b" path write_perm)

let set_wal_journal_mode store =
  let open Lwt_result_syntax in
  with_connection store @@ fun conn ->
  let* current_mode = Db.find conn Q.Journal_mode.get () in
  when_ (current_mode <> Wal) @@ fun () ->
  let* _wal = Db.find conn Q.Journal_mode.set_wal () in
  return_unit

let close {db_pool; _} = Lwt_pool.clear db_pool

let vacuum ~conn ~output_db_file =
  let open Lwt_result_syntax in
  let*! exists = Lwt_unix.file_exists output_db_file in
  let*? () = error_when exists (File_already_exists output_db_file) in
  let* () =
    with_connection conn @@ fun conn ->
    Db.exec conn Q.vacuum_request output_db_file
  in
  let db =
    {
      db_pool = Pool.create 1 (uri output_db_file Read_write);
      trace = no_trace;
      add_attrs = no_add_attrs;
    }
  in
  let* () = use db set_wal_journal_mode in
  let*! () = close db in
  return_unit

let vacuum_self ~conn =
  with_connection conn @@ fun conn -> Db.exec conn Q.vacuum_self ()

let init ~path ~perm ?max_conn_reuse_count ?register migration_code =
  let open Lwt_result_syntax in
  let uri = uri path perm in
  let pool_size =
    match perm with
    | Read_only {pool_size} -> pool_size
    | Read_write ->
        (* When using [Read_write] mode, write operations will acquire an
           exclusive lock on the database file, preventing other processes from
           writing to it simultaneously. In this case the pool size is always
           1. *)
        1
  in
  let trace, add_attrs =
    if Opentelemetry.Collector.has_backend () then
      let attrs = [("db.system.name", `String "sqlite")] in
      ( (fun ?trace_id ?parent ?scope name f ->
          Opentelemetry_lwt.Trace.with_
            ?trace_id
            ?parent
            ?scope
            ~kind:Span_kind_client
            ~attrs
            ~service_name:"Sqlite"
            name
            (fun scope -> f (Some scope))),
        fun f x -> function
          | Some scope -> Opentelemetry.Scope.add_attrs scope (fun () -> f x)
          | None -> () )
    else (no_trace, no_add_attrs)
  in
  let db_pool =
    Pool.create pool_size ?max_use_count:max_conn_reuse_count ?register uri
  in
  let store = {db_pool; trace; add_attrs} in
  use store @@ fun conn ->
  let* () = set_wal_journal_mode conn in
  let* () = with_transaction conn migration_code in
  return store
