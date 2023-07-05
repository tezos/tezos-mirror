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

(*let mainnet_cycle_of_level level =*)
(*if level < 1_589_248 then level / 4096 else 388 + ((level - 1_589_248) / 8192)*)

let method_not_allowed_respond meths =
  let headers =
    Cohttp.Header.add_multi
      (Cohttp.Header.init ())
      "Allow"
      (List.map Cohttp.Code.string_of_method meths)
  in
  Cohttp_lwt_unix.Server.respond
    ~headers
    ~status:`Method_not_allowed
    ~body:Cohttp_lwt.Body.empty
    ()

let options_respond methods =
  let meths_str = List.map Cohttp.Code.string_of_method methods in
  let headers = Cohttp.Header.init_with "Access-Control-Allow-Origin" "*" in
  let headers =
    Cohttp.Header.add headers "Access-Control-Allow-Headers" "Content-Type"
  in
  let headers = Cohttp.Header.add_multi headers "Allow" meths_str in
  let headers =
    Cohttp.Header.add_multi headers "Access-Control-Allow-Methods" meths_str
  in
  let headers =
    Cohttp.Header.add headers "Access-Control-Request-Headers" "X-Custom-Header"
  in
  Cohttp_lwt_unix.Server.respond
    ~headers
    ~status:`OK
    ~body:Cohttp_lwt.Body.empty
    ()

let with_authentification wrong_login need_auth users header f =
  match Cohttp.Header.get_authorization header with
  | Some (`Basic (u, p)) ->
      if List.exists (fun (u', p') -> u = u' && Bcrypt.verify p p') users then
        f u
      else
        Cohttp_lwt_unix.Server.respond_string
          ~headers:
            (Cohttp.Header.init_with "content-type" "text/plain; charset=UTF-8")
          ~status:`Forbidden
          ~body:wrong_login
          ()
  | Some _ | None ->
      Cohttp_lwt_unix.Server.respond_need_auth ~auth:(`Basic need_auth) ()

(** [with_source_authentification users header f]
    For a given [users] allowed login list,
    checks that header contains authentification for an element of this list,
    and call [f] with the login used to login.
*)
let with_source_authentification =
  with_authentification "Unknown source/Wrong login" "Who are you?"

(** [with_admin_authentification users header f]
    Same as [with_source_authentification], but [users] is a list of admins instead of regular users.
*)
let with_admin_authentification =
  with_authentification "Not an admin/Wrong login" "Who are you?"

(** [post_only_endpoint] checks authorization and runs the callback passed as parameter if allowed,
    or returns an error responses.
    Also handles OPTIONS method.
    Other methods return an error response.
  *)
let post_only_endpoint users header meth f =
  let methods = [`POST; `OPTIONS] in
  match meth with
  | `POST -> with_source_authentification users header f
  | `OPTIONS -> options_respond methods
  | _ -> method_not_allowed_respond methods

(** [get_only_endpoint] runs the callback passed as parameter if method is GET.
    Also handles OPTIONS method.
    Other methods return an error response.
  *)
let get_only_endpoint meth f =
  let methods = [`GET; `OPTIONS] in
  match meth with
  | `GET -> f ()
  | `OPTIONS -> options_respond methods
  | _ -> method_not_allowed_respond methods

let with_json_body body f =
  Lwt.bind (Cohttp_lwt.Body.to_string body) (fun str ->
      match Ezjsonm.from_string_result str with
      | Error err ->
          Cohttp_lwt_unix.Server.respond_string
            ~headers:
              (Cohttp.Header.init_with
                 "content-type"
                 "text/plain; charset=UTF-8")
            ~status:`Bad_request
            ~body:(Ezjsonm.read_error_description err)
            ()
      | Ok json -> f json)

let with_data encoding body f =
  with_json_body body (fun json ->
      try f (Data_encoding.Json.destruct encoding json)
      with e ->
        Cohttp_lwt_unix.Server.respond_string
          ~headers:
            (Cohttp.Header.init_with "content-type" "text/plain; charset=UTF-8")
          ~status:`Bad_request
          ~body:(Printexc.to_string e)
          ())

let with_caqti_error x f =
  Lwt.bind x (function
      | Ok x -> f x
      | Error e ->
          let body = Caqti_error.show e in
          Cohttp_lwt_unix.Server.respond_error ~body ())

(** Returns [data] as JSON (encoded via [encoding]) to the client *)
let reply_json ?(status = `OK) ?headers:(headers_arg = []) encoding data =
  let body =
    Ezjsonm.value_to_string (Data_encoding.Json.construct encoding data)
  in
  let headers =
    Cohttp.Header.init_with "content-type" "application/json; charset=UTF-8"
  in
  let headers =
    List.fold_left
      (fun acc (k, v) -> Cohttp.Header.add acc k v)
      headers
      headers_arg
  in
  Cohttp_lwt_unix.Server.respond_string ~headers ~status ~body ()

(** Same as {!reply_json} with extra "Access-Control-Allow-Origin: *" header. *)
let reply_public_json encoding data =
  reply_json ~headers:[("Access-Control-Allow-Origin", "*")] encoding data

let get_stats db_pool =
  let query =
    Caqti_request.Infix.(
      Caqti_type.(unit ->? tup3 int32 int32 Sql_requests.Type.time_protocol))
      "SELECT level, round, timestamp FROM blocks ORDER BY level DESC, round \
       DESC LIMIT 1"
  in
  with_caqti_error
    (Caqti_lwt.Pool.use
       (fun (module Db : Caqti_lwt.CONNECTION) -> Db.find_opt query ())
       db_pool)
    (function
      | Some (level, round, timestamp) ->
          reply_public_json
            Data_encoding.(
              obj3
                (req "level" int32)
                (req "round" int32)
                (req "timestamp" string))
            (level, round, Tezos_base.Time.Protocol.to_notation timestamp)
      | None ->
          let body = "No registered block yet." in
          Cohttp_lwt_unix.Server.respond_error ~body ())

let get_head db_pool =
  let query =
    Caqti_request.Infix.(Caqti_type.(unit ->? int32))
      "SELECT MAX (level) FROM blocks"
  in
  with_caqti_error
    (Caqti_lwt.Pool.use
       (fun (module Db : Caqti_lwt.CONNECTION) -> Db.find_opt query ())
       db_pool)
    (fun head_level ->
      reply_public_json Data_encoding.(obj1 (opt "level" int32)) head_level)

(** Fetch allowed users' logins from db. *)
let get_users db_pool =
  let query =
    Caqti_request.Infix.(Caqti_type.(unit ->* string))
      "SELECT name FROM nodes WHERE password IS NOT NULL"
  in
  with_caqti_error
    (Caqti_lwt.Pool.use
       (fun (module Db : Caqti_lwt.CONNECTION) -> Db.collect_list query ())
       db_pool)
    (fun users -> reply_public_json Data_encoding.(list string) users)

(** Fetch the list of allowed logins from db and update the list ref given as parameter. *)
let refresh_users =
  let mutex = Lwt_mutex.create () in
  fun db_pool users ->
    Lwt_mutex.with_lock mutex (fun () ->
        let query =
          Caqti_request.Infix.(
            Caqti_type.(unit ->* tup2 string Sql_requests.Type.bcrypt_hash))
            "SELECT name, password FROM nodes WHERE password IS NOT NULL"
        in
        Lwt_result.map
          (fun users_from_db -> users := users_from_db)
          (Caqti_lwt.Pool.use
             (fun (module Db : Caqti_lwt.CONNECTION) ->
               Db.collect_list query ())
             db_pool))

(** Insert a new user (i.e. a teztale archiver) into the database.
    If the user already exists, password is updated.
  *)
let upsert_user db_pool login password =
  let query =
    Caqti_request.Infix.(
      Caqti_type.(tup2 string Sql_requests.Type.bcrypt_hash ->. unit))
      "INSERT INTO nodes(name, password) VALUES ($1, $2) ON CONFLICT (name) DO \
       UPDATE SET password = $2"
  in
  Caqti_lwt.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) -> Db.exec query (login, password))
    db_pool

(** Delete a user from db (i.e. removes feeder permissions) *)
let delete_user db_pool login =
  let query =
    Caqti_request.Infix.(Caqti_type.(string ->. unit))
      "UPDATE nodes SET password = NULL WHERE name = $1"
  in
  Caqti_lwt.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) -> Db.exec query login)
    db_pool

let maybe_create_tables db_pool =
  Caqti_lwt.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
        (fun req ->
          Db.exec (Caqti_request.Infix.(Caqti_type.(unit ->. unit)) req) ())
        Sql_requests.create_tables)
    db_pool

let maybe_alter_tables db_pool =
  Caqti_lwt.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      Lwt.map
        (fun () -> Ok ())
        (Lwt_list.iter_s
           (fun reqs ->
             Lwt.bind
               (Db.with_transaction (fun () ->
                    Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                      (fun req ->
                        Db.exec
                          (Caqti_request.Infix.(Caqti_type.(unit ->. unit)) req)
                          ())
                      reqs))
               (function
                 | Ok () -> Lwt.return_unit
                 | Error e ->
                     Lwt_io.eprintlf
                       "\"ALTER TABLE ADD COLUMN IF NOT EXISTS\" expression is \
                        not supported by sqlite, if the following error is \
                        because the column already exists ignore it:\n\
                        %s"
                       (Caqti_error.show e)))
           Sql_requests.alter_tables))
    db_pool

let maybe_alter_and_create_tables db_pool =
  Lwt.bind (maybe_alter_tables db_pool) (function
      | Error e -> Lwt.return_error e
      | Ok () -> maybe_create_tables db_pool)

let insert_operations_from_block (module Db : Caqti_lwt.CONNECTION) level
    block_hash operations =
  let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax in
  let* () =
    Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
      (fun op ->
        Db.exec
          Sql_requests.maybe_insert_operation
          Teztale_lib.Consensus_ops.
            ( (level, op.op.hash, op.op.kind = Endorsement, op.op.round),
              op.delegate ))
      operations
  in
  Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
    (fun op ->
      Db.exec
        Sql_requests.insert_included_operation
        ( Teztale_lib.Consensus_ops.
            (op.delegate, op.op.kind = Endorsement, op.op.round),
          (block_hash, level) ))
    operations

let endorsing_rights_callback db_pool g rights =
  let level = Int32.of_string (Re.Group.get g 1) in
  let out =
    let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax in
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun right ->
              Db.exec
                Sql_requests.maybe_insert_delegate
                right.Teztale_lib.Consensus_ops.address)
            rights
        in
        Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
          (fun Teztale_lib.Consensus_ops.{address; first_slot; power} ->
            Db.exec
              Sql_requests.maybe_insert_endorsing_right
              (level, first_slot, power, address))
          rights)
      db_pool
  in
  with_caqti_error out (fun () ->
      Cohttp_lwt_unix.Server.respond_string
        ~headers:
          (Cohttp.Header.init_with "content-type" "text/plain; charset=UTF-8")
        ~status:`OK
        ~body:"Endorsing_right noted"
        ())

let block_callback db_pool g source
    ( Teztale_lib.Data.Block.
        {delegate; timestamp; reception_times; round; hash; predecessor; _},
      (endorsements, preendorsements) ) =
  let out =
    let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax in
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        let* () = Db.exec Sql_requests.maybe_insert_source source in
        let* () = Db.exec Sql_requests.maybe_insert_delegate delegate in
        let level = Int32.of_string (Re.Group.get g 1) in
        let* () =
          Db.exec
            Sql_requests.maybe_insert_block
            ((level, timestamp, hash, round), (predecessor, delegate))
        in
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun r ->
              let open Teztale_lib.Data.Block in
              Db.exec
                Sql_requests.insert_received_block
                (r.application_time, r.validation_time, hash, source))
            reception_times
        in
        let* () =
          insert_operations_from_block
            (module Db)
            (Int32.pred level)
            hash
            endorsements
        in
        insert_operations_from_block (module Db) level hash preendorsements)
      db_pool
  in
  with_caqti_error out (fun () ->
      Cohttp_lwt_unix.Server.respond_string
        ~headers:
          (Cohttp.Header.init_with "content-type" "text/plain; charset=UTF-8")
        ~status:`OK
        ~body:"Block registered"
        ())

let operations_callback db_pool g source operations =
  let level = Int32.of_string (Re.Group.get g 1) in
  let out =
    let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax in
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        let* () = Db.exec Sql_requests.maybe_insert_source source in
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun (right, _) ->
              Db.exec
                Sql_requests.maybe_insert_delegate
                right.Teztale_lib.Consensus_ops.address)
            operations
        in
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun (right, ops) ->
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun (op : Teztale_lib.Consensus_ops.received_operation) ->
                  Db.exec
                    Sql_requests.maybe_insert_operation
                    Teztale_lib.Consensus_ops.
                      ( ( level,
                          op.op.hash,
                          op.op.kind = Endorsement,
                          op.op.round ),
                        right.Teztale_lib.Consensus_ops.address ))
                ops)
            operations
        in
        Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
          (fun (right, ops) ->
            Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
              (fun op ->
                Db.exec
                  Sql_requests.insert_received_operation
                  Teztale_lib.Consensus_ops.
                    ( ( op.reception_time,
                        op.errors,
                        right.Teztale_lib.Consensus_ops.address,
                        op.op.kind = Endorsement ),
                      (op.op.round, source, level) ))
              ops)
          operations)
      db_pool
  in
  with_caqti_error out (fun () ->
      Cohttp_lwt_unix.Server.respond_string
        ~headers:
          (Cohttp.Header.init_with "content-type" "text/plain; charset=UTF-8")
        ~status:`OK
        ~body:"Received operations stored"
        ())

let import_callback db_pool g data =
  let level = Int32.of_string (Re.Group.get g 1) in
  let out =
    let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax in
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        (* delegates *)
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun slot ->
              Db.exec
                Sql_requests.maybe_insert_delegate
                slot.Teztale_lib.Data.Delegate_operations.delegate)
            data.Teztale_lib.Data.delegate_operations
        in
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun block ->
              Db.exec
                Sql_requests.maybe_insert_delegate
                block.Teztale_lib.Data.Block.delegate)
            data.Teztale_lib.Data.blocks
        in
        (* sources *)
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun slot ->
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun op ->
                  Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                    (fun inc ->
                      Db.exec
                        Sql_requests.maybe_insert_source
                        inc.Teztale_lib.Data.Delegate_operations.source)
                    op.Teztale_lib.Data.Delegate_operations.mempool_inclusion)
                slot.Teztale_lib.Data.Delegate_operations.operations)
            data.Teztale_lib.Data.delegate_operations
        in
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun block ->
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun Teztale_lib.Data.Block.{source; _} ->
                  Db.exec Sql_requests.maybe_insert_source source)
                block.Teztale_lib.Data.Block.reception_times)
            data.Teztale_lib.Data.blocks
        in
        (* rights *)
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun Teztale_lib.Data.Delegate_operations.
                   {delegate; first_slot; endorsing_power; _} ->
              Db.exec
                Sql_requests.maybe_insert_endorsing_right
                (level, first_slot, endorsing_power, delegate))
            data.Teztale_lib.Data.delegate_operations
        in
        (* blocks *)
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun block ->
              Db.exec
                Sql_requests.maybe_insert_block
                Teztale_lib.Data.Block.
                  ( (level, block.timestamp, block.hash, block.round),
                    (block.predecessor, block.delegate) ))
            data.Teztale_lib.Data.blocks
        in
        (* operations *)
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun Teztale_lib.Data.Delegate_operations.{delegate; operations; _} ->
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun Teztale_lib.Data.Delegate_operations.{hash; kind; round; _} ->
                  Db.exec
                    Sql_requests.maybe_insert_operation
                    ( ( level,
                        hash,
                        kind = Teztale_lib.Consensus_ops.Endorsement,
                        round ),
                      delegate ))
                operations)
            data.Teztale_lib.Data.delegate_operations
        in
        (* block reception *)
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun block ->
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun Teztale_lib.Data.Block.
                       {source; application_time; validation_time} ->
                  Db.exec
                    Sql_requests.insert_received_block
                    ( application_time,
                      validation_time,
                      block.Teztale_lib.Data.Block.hash,
                      source ))
                block.Teztale_lib.Data.Block.reception_times)
            data.Teztale_lib.Data.blocks
        in
        (* operation reception *)
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun Teztale_lib.Data.Delegate_operations.{delegate; operations; _} ->
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun Teztale_lib.Data.Delegate_operations.
                       {kind; round; mempool_inclusion; _} ->
                  Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                    (fun Teztale_lib.Data.Delegate_operations.
                           {source; reception_time; errors} ->
                      Db.exec
                        Sql_requests.insert_received_operation
                        ( ( reception_time,
                            errors,
                            delegate,
                            kind = Teztale_lib.Consensus_ops.Endorsement ),
                          (round, source, level) ))
                    mempool_inclusion)
                operations)
            data.Teztale_lib.Data.delegate_operations
        in
        (* operation inclusion *)
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun Teztale_lib.Data.Delegate_operations.{delegate; operations; _} ->
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun Teztale_lib.Data.Delegate_operations.
                       {kind; round; block_inclusion; _} ->
                  Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                    (fun block_hash ->
                      Db.exec
                        Sql_requests.insert_included_operation
                        ( ( delegate,
                            kind = Teztale_lib.Consensus_ops.Endorsement,
                            round ),
                          (block_hash, level) ))
                    block_inclusion)
                operations)
            data.Teztale_lib.Data.delegate_operations
        in
        return_unit)
      db_pool
  in
  with_caqti_error out (fun () ->
      Cohttp_lwt_unix.Server.respond_string
        ~headers:
          (Cohttp.Header.init_with "content-type" "text/plain; charset=UTF-8")
        ~status:`OK
        ~body:"Level imported"
        ())

let routes :
    (Re.re
    * (Re.Group.t ->
      admins:(string * Bcrypt.hash) list ->
      users:(string * Bcrypt.hash) list ref ->
      (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t ->
      Cohttp.Header.t ->
      Cohttp.Code.meth ->
      Cohttp_lwt.Body.t ->
      (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t))
    list =
  [
    ( Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str "/rights"],
      fun g ~admins:_ ~users db_pool header meth body ->
        post_only_endpoint !users header meth (fun _source ->
            with_data
              Teztale_lib.Consensus_ops.rights_encoding
              body
              (endorsing_rights_callback db_pool g)) );
    ( Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str "/block"],
      fun g ~admins:_ ~users db_pool header meth body ->
        post_only_endpoint !users header meth (fun source ->
            with_data
              Teztale_lib.Data.block_data_encoding
              body
              (block_callback db_pool g source)) );
    ( Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str "/mempool"],
      fun g ~admins:_ ~users db_pool header meth body ->
        post_only_endpoint !users header meth (fun source ->
            with_data
              Teztale_lib.Consensus_ops.delegate_ops_encoding
              body
              (operations_callback db_pool g source)) );
    ( Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str "/import"],
      fun g ~admins ~users:_ db_pool header meth body ->
        post_only_endpoint admins header meth (fun _source ->
            with_data Teztale_lib.Data.encoding body (import_callback db_pool g))
    );
    ( Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str ".json"],
      fun g ~admins:_ ~users:_ db_pool _header meth _body ->
        get_only_endpoint meth (fun () ->
            let level = Int32.of_string (Re.Group.get g 1) in
            with_caqti_error (Exporter.data_at_level db_pool level) (fun data ->
                reply_public_json Teztale_lib.Data.encoding data)) );
    ( Re.seq
        [
          Re.str "/";
          Re.group (Re.rep1 Re.digit);
          Re.str "-";
          Re.group (Re.rep1 Re.digit);
          Re.str "/anomalies.json";
        ],
      fun g ~admins:_ ~users:_ db_pool _header meth _body ->
        get_only_endpoint meth (fun () ->
            let first_level = int_of_string (Re.Group.get g 1) in
            let last_level = int_of_string (Re.Group.get g 2) in
            with_caqti_error
              (let levels =
                 Stdlib.List.init
                   (last_level - first_level + 1)
                   (fun i -> Int32.of_int (first_level + i))
               in
               Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.concat_map_es
                 (Exporter.anomalies_at_level db_pool)
                 levels)
              (fun data ->
                let body =
                  Ezjsonm.value_to_string
                    (Data_encoding.Json.construct
                       (Data_encoding.list Teztale_lib.Data.Anomaly.encoding)
                       data)
                in
                Cohttp_lwt_unix.Server.respond_string
                  ~headers:
                    (Cohttp.Header.init_with
                       "content-type"
                       "application/json; charset=UTF-8")
                  ~status:`OK
                  ~body
                  ())) );
    ( Re.str "/user",
      fun _g ~admins ~users db_pool header meth body ->
        let reply_ok login () =
          reply_json
            Data_encoding.(obj2 (req "status" string) (req "login" string))
            ("OK", login)
        in
        with_admin_authentification admins header (fun _admin ->
            let methods = [`DELETE; `OPTIONS; `PUT] in
            match meth with
            | `PUT ->
                with_data Config.login_encoding body (fun (login, password) ->
                    let password = Bcrypt.hash password in
                    with_caqti_error
                      (Lwt_result.bind
                         (upsert_user db_pool login password)
                         (fun () -> refresh_users db_pool users))
                      (reply_ok login))
            | `DELETE ->
                with_data
                  Data_encoding.(obj1 (req "login" string))
                  body
                  (fun login ->
                    with_caqti_error
                      (Lwt_result.bind (delete_user db_pool login) (fun () ->
                           refresh_users db_pool users))
                      (reply_ok login))
            | `OPTIONS -> options_respond methods
            | _ -> method_not_allowed_respond methods) );
    ( Re.str "/users",
      fun _g ~admins:_ ~users:_ db_pool _header meth _body ->
        get_only_endpoint meth (fun () -> get_users db_pool) );
    ( Re.str "/stats.json",
      fun _g ~admins:_ ~users:_ db_pool _header _meth _body -> get_stats db_pool
    );
    ( Re.str "/head.json",
      fun _g ~admins:_ ~users:_ db_pool _header _meth _body -> get_head db_pool
    );
  ]
  |> List.map (fun (r, fn) -> (r |> Re.whole_string |> Re.compile, fn))

let callback ~public ~admins ~users db_pool _connection request body =
  let header = Cohttp.Request.headers request in
  let meth = Cohttp.Request.meth request in
  let uri = Cohttp.Request.uri request in
  let path = Uri.path uri in
  match
    List.find_map
      (fun (r, fn) ->
        match Re.exec_opt r path with Some g -> Some (fn g) | None -> None)
      routes
  with
  | Some fn -> fn ~admins ~users db_pool header meth body
  | None -> (
      match public with
      | None -> Cohttp_lwt_unix.Server.respond_not_found ()
      | Some docroot ->
          let path = match path with "" | "/" -> "/index.html" | _ -> path in
          let uri = Uri.of_string path in
          let fname = Cohttp.Path.resolve_local_file ~docroot ~uri in
          (* Resolving symlink here in order to have Magic_mime works with targeted file extension *)
          let fname =
            try Filename.concat (Filename.dirname fname) (Unix.readlink fname)
            with _ -> fname
          in
          Cohttp_lwt_unix.Server.respond_file
            ~headers:
              (Cohttp.Header.init_with "content-type" (Magic_mime.lookup fname))
            ~fname
            ())

(* Must exists somewhere but where ! *)
let print_location f ((fl, fc), (tl, tc)) =
  if Int.equal fl tl then
    if Int.equal fc tc then Format.fprintf f "line %i character %i" fl fc
    else Format.fprintf f "line %i characters %i-%i" fl fc tc
  else Format.fprintf f "lines %i-%i characters %i-%i" fl tl fc tc

let config =
  if Array.length Sys.argv < 2 then
    let () =
      Format.eprintf "%s needs a config file as argument@." Sys.executable_name
    in
    exit 1
  else if not (Sys.file_exists Sys.argv.(1)) then
    let () = Format.eprintf "%s is not a file@." Sys.argv.(1) in
    exit 1
  else
    let ic = open_in Sys.argv.(1) in
    match Ezjsonm.from_channel_result ic with
    | Error err ->
        Format.eprintf
          "Error in %s: %a:@ %s@."
          Sys.argv.(1)
          (Format.pp_print_option print_location)
          (Ezjsonm.read_error_location err)
          (Ezjsonm.read_error_description err) ;
        exit 1
    | Ok json -> (
        try Data_encoding.Json.destruct Config.encoding json
        with e ->
          let () =
            Format.eprintf
              "@[<v>@[Invalid configuration in %s:@ @[%a@]@]@ Configuration \
               file format is@ @[%a@]@]@."
              Sys.argv.(1)
              (Data_encoding.Json.print_error ?print_unknown:None)
              e
              Json_schema.pp
              (Data_encoding.Json.schema Config.encoding)
          in
          exit 1)

let () =
  let uri = Uri.of_string config.Config.db_uri in
  let users = ref [] in
  let admins =
    List.map (fun (u, p) -> (u, Bcrypt.hash p)) config.Config.admins
  in
  Lwt_main.run
    (match Caqti_lwt.connect_pool ~env:Sql_requests.env uri with
    | Error e -> Lwt_io.eprintl (Caqti_error.show e)
    | Ok pool ->
        Lwt.bind (maybe_alter_and_create_tables pool) (function
            | Error e -> Lwt_io.eprintl (Caqti_error.show e)
            | Ok () ->
                let stop, paf = Lwt.task () in
                let shutdown _ = Lwt.wakeup paf () in
                let _ = Sys.signal Sys.sigint (Sys.Signal_handle shutdown) in
                Lwt.bind
                  (Lwt.map
                     (List.find_map (function Error e -> Some e | _ -> None))
                     (Lwt_list.map_s
                        (fun (login, password) ->
                          let password = Bcrypt.hash password in
                          upsert_user pool login password)
                        config.Config.users))
                  (function
                    | Some e -> Lwt_io.eprintl (Caqti_error.show e)
                    | None ->
                        Lwt.bind (refresh_users pool users) (function
                            | Error e -> Lwt_io.eprintl (Caqti_error.show e)
                            | Ok () ->
                                let servers =
                                  List.map
                                    (fun con ->
                                      Lwt.bind
                                        (Conduit_lwt_unix.init
                                           ?src:con.Config.source
                                           ())
                                        (fun ctx ->
                                          let ctx =
                                            Cohttp_lwt_unix.Net.init ~ctx ()
                                          in
                                          let mode =
                                            match con.Config.tls with
                                            | Some Config.{crt; key} ->
                                                `TLS
                                                  ( `Crt_file_path crt,
                                                    `Key_file_path key,
                                                    `No_password,
                                                    `Port con.Config.port )
                                            | None ->
                                                `TCP (`Port con.Config.port)
                                          in
                                          Format.printf
                                            "Server listening at %a:%d@."
                                            (Format.pp_print_option
                                               ~none:(fun f () ->
                                                 Format.pp_print_string
                                                   f
                                                   "<default>")
                                               Format.pp_print_string)
                                            con.Config.source
                                            con.port ;
                                          Cohttp_lwt_unix.Server.create
                                            ~stop
                                            ~ctx
                                            ~mode
                                            (Cohttp_lwt_unix.Server.make
                                               ~callback:
                                                 (callback
                                                    ~public:
                                                      config
                                                        .Config.public_directory
                                                    ~admins
                                                    ~users
                                                    pool)
                                               ())))
                                    config.Config.network_interfaces
                                in
                                Lwt.join servers))))
