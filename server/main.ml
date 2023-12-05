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

let maybe_with_transaction (c : Config.t) (m : Lwt_mutex.t)
    (module D : Caqti_lwt.CONNECTION) f =
  if c.with_transaction then
    Lwt_mutex.with_lock m (fun () -> D.with_transaction f)
  else f ()

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

let with_caqti_error ~logger x f =
  Lwt.bind x (function
      | Ok x -> f x
      | Error e ->
          let body = Caqti_error.show e in
          Teztale_lib.Log.error logger (fun () -> body) ;
          Cohttp_lwt_unix.Server.respond_error ~body ())

let reply_json0 ?(status = `OK) ?headers:(headers_arg = []) body =
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

let reply_compressed_json0 ?status ?(headers = []) encoding data =
  let body =
    Ezjsonm.value_to_string (Data_encoding.Json.construct encoding data)
  in
  let compressed = Ezgzip.compress body in
  let headers = ("Content-Encoding", "gzip") :: headers in
  reply_json0 ?status ~headers compressed

(** Returns [data] as JSON (encoded via [encoding]) to the client *)
let reply_json ?status ?headers encoding data =
  reply_json0
    ?status
    ?headers
    (Ezjsonm.value_to_string (Data_encoding.Json.construct encoding data))

(** Same as {!reply_json} with extra "Access-Control-Allow-Origin: *" header. *)
let reply_public_json ?status ?(headers = []) encoding data =
  reply_json
    ?status
    ~headers:(("Access-Control-Allow-Origin", "*") :: headers)
    encoding
    data

let reply_compressed_json ?status ?headers request_header encoding data =
  let rec aux = function
    | [] | (_, Cohttp.Accept.Identity) :: _ ->
        reply_json ?status ?headers encoding data
    | (_, Cohttp.Accept.Gzip) :: _ ->
        reply_compressed_json0 ?status ?headers encoding data
    | _ :: t -> aux t
  in
  aux Cohttp.(Accept.qsort (Header.get_acceptable_encodings request_header))

let reply_public_compressed_json ?status ?(headers = []) encoding data =
  reply_compressed_json
    ?status
    ~headers:(("Access-Control-Allow-Origin", "*") :: headers)
    encoding
    data

let get_stats ~logger db_pool =
  let query =
    Caqti_request.Infix.(
      Caqti_type.(unit ->? tup3 int32 int32 Sql_requests.Type.time_protocol))
      "SELECT level, round, timestamp FROM blocks ORDER BY level DESC, round \
       DESC LIMIT 1"
  in
  with_caqti_error
    ~logger
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

let get_head ~logger db_pool =
  let query =
    Caqti_request.Infix.(Caqti_type.(unit ->? int32))
      "SELECT MAX (level) FROM blocks"
  in
  with_caqti_error
    ~logger
    (Caqti_lwt.Pool.use
       (fun (module Db : Caqti_lwt.CONNECTION) -> Db.find_opt query ())
       db_pool)
    (fun head_level ->
      reply_public_json Data_encoding.(obj1 (opt "level" int32)) head_level)

(** Fetch allowed users' logins from db. *)
let get_users ~logger db_pool =
  let query =
    Caqti_request.Infix.(Caqti_type.(unit ->* string))
      "SELECT name FROM nodes WHERE password IS NOT NULL"
  in
  with_caqti_error
    ~logger
    (Caqti_lwt.Pool.use
       (fun (module Db : Caqti_lwt.CONNECTION) -> Db.collect_list query ())
       db_pool)
    (fun users -> reply_public_json Data_encoding.(list string) users)

let get_levels_at_timestamp0 db_pool timestamp =
  let lower =
    Caqti_request.Infix.(Caqti_type.(int32 ->* tup2 int32 int32))
      "SELECT level, MIN(timestamp) FROM blocks, (SELECT MAX(level) AS m FROM \
       blocks WHERE timestamp <= ?) WHERE level = m OR level = m + 1 GROUP BY \
       level"
  in
  let upper =
    Caqti_request.Infix.(Caqti_type.(int32 ->* tup2 int32 int32))
      "SELECT level, MIN(timestamp) FROM blocks, (SELECT MIN(level) AS m FROM \
       blocks WHERE timestamp >= ?) WHERE level = m OR level = m - 1 GROUP BY \
       level"
  in
  Lwt_result.bind
    (Caqti_lwt.Pool.use
       (fun (module Db : Caqti_lwt.CONNECTION) ->
         Db.collect_list lower timestamp)
       db_pool)
    (function
      | [] ->
          Caqti_lwt.Pool.use
            (fun (module Db : Caqti_lwt.CONNECTION) ->
              Db.collect_list upper timestamp)
            db_pool
      | l -> Lwt_result.return l)

let get_levels_at_timestamp db_pool timestamp =
  Lwt_result.map
    (function
      | [] -> (None, None)
      | [((_, t) as r)] ->
          if Int32.compare timestamp t = -1 then (None, Some r)
          else (Some r, None)
      | ((_, t1) as a) :: ((_, t2) as b) :: _ ->
          if Int32.compare t1 t2 = -1 then (Some a, Some b) else (Some b, Some a))
    (get_levels_at_timestamp0 db_pool timestamp)

(** Fetch the list of allowed logins from db and update the list ref given as parameter. *)
let refresh_users db_pool users =
  Lwt_mutex.with_lock Sql_requests.Mutex.nodes (fun () ->
      let query =
        Caqti_request.Infix.(
          Caqti_type.(unit ->* tup2 string Sql_requests.Type.bcrypt_hash))
          "SELECT name, password FROM nodes WHERE password IS NOT NULL"
      in
      Lwt_result.map
        (fun users_from_db -> users := users_from_db)
        (Caqti_lwt.Pool.use
           (fun (module Db : Caqti_lwt.CONNECTION) -> Db.collect_list query ())
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
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      Lwt_mutex.with_lock Sql_requests.Mutex.nodes (fun () ->
          Db.exec query (login, password)))
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

let with_cache mutex request mem add (module Db : Caqti_lwt.CONNECTION) conf
    list =
  (* Note: even if data is already in cache,
     we add it again at the end in order to mark it as recent. *)
  if conf.Config.with_transaction then
    Lwt_mutex.with_lock mutex @@ fun () ->
    Db.with_transaction @@ fun () ->
    Lwt_result.map (fun () -> List.iter add list)
    @@ Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
         (fun x ->
           if mem x then
             Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax
             .return_unit
           else Db.exec request x)
         list
  else
    Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
      (fun x ->
        Lwt_result.map
          (fun () -> add x)
          (if mem x then
             Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax
             .return_unit
           else Db.exec request x))
      list

let may_insert_delegates =
  let module Cache =
    Aches.Vache.Set (Aches.Vache.LRU_Precise) (Aches.Vache.Strong)
      (Tezos_crypto.Signature.Public_key_hash)
  in
  let cache = Cache.create 1_000 in
  with_cache
    Sql_requests.Mutex.delegates
    Sql_requests.maybe_insert_delegate
    (Cache.mem cache)
    (Cache.add cache)

let may_insert_operations =
  let module Cache =
    Aches.Vache.Set (Aches.Vache.LRU_Precise) (Aches.Vache.Strong)
      (Tezos_base.TzPervasives.Operation_hash)
  in
  let cache = Cache.create 10_000 in
  let hash ((_, h, _, _), _) = h in
  with_cache
    Sql_requests.Mutex.operations
    Sql_requests.maybe_insert_operation
    (fun x -> Cache.mem cache @@ hash x)
    (fun x -> Cache.add cache @@ hash x)

let with_external_cache mutex request =
  (* We don't use the cache feature here, only the SQL transaction option. *)
  with_cache mutex request (fun _ -> false) (fun _ -> ())

let format_block_op level delegate (op : Teztale_lib.Consensus_ops.operation) =
  ((level, op.hash, op.kind = Endorsement, op.round), delegate)

let endorsing_rights_callback =
  let module Cache =
    Aches.Vache.Set (Aches.Vache.LRU_Precise) (Aches.Vache.Strong)
      (struct
        include Int32

        let hash = Int32.to_int
      end)
  in
  let cache = Cache.create 100 in
  fun ~logger conf db_pool g rights ->
    let level = Int32.of_string (Re.Group.get g 1) in
    let out =
      let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax in
      (* Note: even if data is already in cache,
         we add it again in order to mark it as recent. *)
      Lwt_result.map
        (fun () -> Cache.add cache level)
        (if Cache.mem cache level then
           let () =
             Teztale_lib.Log.debug logger (fun () ->
                 Format.asprintf "(%ld) CACHE.mem Level rights" level)
           in
           return_unit
         else
           let* () =
             Caqti_lwt.Pool.use
               (fun (module Db : Caqti_lwt.CONNECTION) ->
                 let* () =
                   let delegates =
                     List.map
                       (fun {Teztale_lib.Consensus_ops.address; _} -> address)
                       rights
                   in
                   may_insert_delegates (module Db) conf delegates
                 in
                 let rights =
                   List.map
                     (fun Teztale_lib.Consensus_ops.{address; first_slot; power} ->
                       (level, first_slot, power, address))
                     rights
                 in
                 with_external_cache
                   Sql_requests.Mutex.endorsing_rights
                   Sql_requests.maybe_insert_endorsing_right
                   (module Db)
                   conf
                   rights)
               db_pool
           in
           let () =
             Teztale_lib.Log.debug logger (fun () ->
                 Format.asprintf "(%ld) CACHE.add Level rights" level)
           in
           return_unit)
    in
    with_caqti_error ~logger out (fun () ->
        Cohttp_lwt_unix.Server.respond_string
          ~headers:
            (Cohttp.Header.init_with "content-type" "text/plain; charset=UTF-8")
          ~status:`OK
          ~body:"Endorsing rights"
          ())

let insert_operations_from_block (module Db : Caqti_lwt.CONNECTION) conf level
    block_hash operations =
  let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax in
  let* () =
    let operations =
      List.map
        (fun {Teztale_lib.Consensus_ops.delegate; op; _} ->
          format_block_op level delegate op)
        operations
    in
    may_insert_operations (module Db) conf operations
  in
  let operation_inclusion =
    List.map
      (fun op ->
        ( Teztale_lib.Consensus_ops.
            (op.delegate, op.op.kind = Endorsement, op.op.round),
          (block_hash, level) ))
      operations
  in
  with_external_cache
    Sql_requests.Mutex.operations_inclusion
    Sql_requests.insert_included_operation
    (module Db)
    conf
    operation_inclusion

module Block_lru_cache =
  Aches.Vache.Set (Aches.Vache.LRU_Precise) (Aches.Vache.Strong)
    (Tezos_base.TzPervasives.Block_hash)

let block_callback =
  let block_cache = Block_lru_cache.create 100 in
  let block_operations_cache = Block_lru_cache.create 100 in
  fun ~logger
      conf
      db_pool
      g
      source
      ( Teztale_lib.Data.Block.
          {delegate; timestamp; reception_times; round; hash; predecessor; _},
        cycle_info,
        (endorsements, preendorsements) ) ->
    let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax in
    let level = Int32.of_string (Re.Group.get g 1) in
    let out =
      Caqti_lwt.Pool.use
        (fun (module Db : Caqti_lwt.CONNECTION) ->
          let* () =
            (* Note: even if data is already in cache,
               we add it again in order to mark it as recent. *)
            Lwt_result.map
              (fun () -> Block_lru_cache.add block_cache hash)
              (if Block_lru_cache.mem block_cache hash then
                 let () =
                   Teztale_lib.Log.debug logger (fun () ->
                       Format.asprintf
                         "(%ld) CACHE.mem %a"
                         level
                         Tezos_base.TzPervasives.Block_hash.pp
                         hash)
                 in
                 return_unit
               else
                 let* () = may_insert_delegates (module Db) conf [delegate] in
                 let* () =
                   with_external_cache
                     Sql_requests.Mutex.blocks
                     Sql_requests.maybe_insert_block
                     (module Db)
                     conf
                     [
                       ((level, timestamp, hash, round), (predecessor, delegate));
                     ]
                 in
                 let* () =
                   match cycle_info with
                   | Some Teztale_lib.Data.{cycle; cycle_position; cycle_size}
                     ->
                       with_external_cache
                         Sql_requests.Mutex.cycles
                         Sql_requests.maybe_insert_cycle
                         (module Db)
                         conf
                         [(cycle, Int32.sub level cycle_position, cycle_size)]
                   | _ -> Lwt.return_ok ()
                 in
                 Teztale_lib.Log.debug logger (fun () ->
                     Format.asprintf
                       "(%ld) CACHE.add (block) %a"
                       level
                       Tezos_base.TzPervasives.Block_hash.pp
                       hash) ;
                 return_unit)
          in
          let* () =
            (* Validated blocks do not provide operations only applied
               one does so we need a second cache *)
            (* Note: even if data is already in cache,
               we add it again in order to mark it as recent. *)
            Lwt_result.map
              (fun () ->
                if endorsements <> [] then
                  Block_lru_cache.add block_operations_cache hash)
              (if Block_lru_cache.mem block_operations_cache hash then
                 return_unit
               else
                 let* () =
                   insert_operations_from_block
                     (module Db)
                     conf
                     (Int32.pred level)
                     hash
                     endorsements
                 in
                 let* () =
                   insert_operations_from_block
                     (module Db)
                     conf
                     level
                     hash
                     preendorsements
                 in
                 Teztale_lib.Log.debug logger (fun () ->
                     Format.asprintf
                       "(%ld) CACHE.add (block operations) %a"
                       level
                       Tezos_base.TzPervasives.Block_hash.pp
                       hash) ;
                 return_unit)
          in
          let* () =
            (* This data is non-redondant: we always treat it. *)
            Db.with_transaction @@ fun () ->
            Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
              (fun r ->
                let open Teztale_lib.Data.Block in
                Db.exec
                  Sql_requests.insert_received_block
                  (r.application_time, r.validation_time, hash, source))
              reception_times
          in
          Teztale_lib.Log.debug logger (fun () ->
              Format.asprintf
                "(%ld) OK Block reception times for %a"
                level
                Tezos_base.TzPervasives.Block_hash.pp
                hash) ;
          return_unit)
        db_pool
    in
    with_caqti_error ~logger out (fun () ->
        Cohttp_lwt_unix.Server.respond_string
          ~headers:
            (Cohttp.Header.init_with "content-type" "text/plain; charset=UTF-8")
          ~status:`OK
          ~body:
            (Format.asprintf
               "Block %a"
               Tezos_base.TzPervasives.Block_hash.pp
               hash)
          ())

let operations_callback ~logger conf db_pool g source operations =
  let level = Int32.of_string (Re.Group.get g 1) in
  let out =
    let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax in
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        let* () =
          let delegates =
            List.map
              (fun ({Teztale_lib.Consensus_ops.address; _}, _) -> address)
              operations
          in
          may_insert_delegates (module Db) conf delegates
        in
        Teztale_lib.Log.debug logger (fun () ->
            Printf.sprintf "(%ld) OK Delegates" level) ;
        let* () =
          let operations =
            List.flatten
            @@ List.map
                 (fun (rights, operations) ->
                   let delegate = rights.Teztale_lib.Consensus_ops.address in
                   List.map
                     (fun (op : Teztale_lib.Consensus_ops.received_operation) ->
                       format_block_op level delegate op.op)
                     operations)
                 operations
          in
          may_insert_operations (module Db) conf operations
        in
        Teztale_lib.Log.debug logger (fun () ->
            Printf.sprintf "(%ld) OK Operations" level) ;
        let* () =
          Db.with_transaction @@ fun () ->
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
            operations
        in
        Teztale_lib.Log.debug logger (fun () ->
            Printf.sprintf "(%ld) OK Operations reception times" level) ;
        Lwt.return_ok ())
      db_pool
  in
  with_caqti_error ~logger out (fun () ->
      Cohttp_lwt_unix.Server.respond_string
        ~headers:
          (Cohttp.Header.init_with "content-type" "text/plain; charset=UTF-8")
        ~status:`OK
        ~body:"Operations reception times"
        ())

let import_callback ~logger conf db_pool g data =
  let level = Int32.of_string (Re.Group.get g 1) in
  let out =
    let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax in
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        (* delegates *)
        let* () =
          let delegates =
            List.rev_map
              (fun {Teztale_lib.Data.Delegate_operations.delegate; _} ->
                delegate)
              data.Teztale_lib.Data.delegate_operations
          in
          let delegates' =
            List.rev_map
              (fun {Teztale_lib.Data.Block.delegate; _} -> delegate)
              data.Teztale_lib.Data.blocks
          in
          may_insert_delegates
            (module Db)
            conf
            (List.rev_append delegates delegates')
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
  with_caqti_error ~logger out (fun () ->
      Cohttp_lwt_unix.Server.respond_string
        ~headers:
          (Cohttp.Header.init_with "content-type" "text/plain; charset=UTF-8")
        ~status:`OK
        ~body:"Level imported"
        ())

let routes :
    (Re.re
    * (Re.Group.t ->
      logger:Teztale_lib.Log.t ->
      conf:Config.t ->
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
      fun g ~logger ~conf ~admins:_ ~users db_pool header meth body ->
        post_only_endpoint !users header meth (fun _source ->
            with_data
              Teztale_lib.Consensus_ops.rights_encoding
              body
              (endorsing_rights_callback ~logger conf db_pool g)) );
    ( Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str "/block"],
      fun g ~logger ~conf ~admins:_ ~users db_pool header meth body ->
        post_only_endpoint !users header meth (fun source ->
            with_data
              Teztale_lib.Data.block_data_encoding
              body
              (block_callback ~logger conf db_pool g source)) );
    ( Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str "/mempool"],
      fun g ~logger ~conf ~admins:_ ~users db_pool header meth body ->
        post_only_endpoint !users header meth (fun source ->
            with_data
              Teztale_lib.Consensus_ops.delegate_ops_encoding
              body
              (operations_callback ~logger conf db_pool g source)) );
    ( Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str "/import"],
      fun g ~logger ~conf ~admins ~users:_ db_pool header meth body ->
        post_only_endpoint admins header meth (fun _source ->
            with_data
              Teztale_lib.Data.encoding
              body
              (import_callback ~logger conf db_pool g)) );
    ( Re.seq [Re.str "/timestamp/"; Re.group (Re.rep1 Re.digit)],
      fun g ~logger ~conf:_ ~admins:_ ~users:_ db_pool _header meth _body ->
        get_only_endpoint meth (fun () ->
            let level = Int32.of_string (Re.Group.get g 1) in
            with_caqti_error
              ~logger
              (get_levels_at_timestamp db_pool level)
              (reply_public_json Teztale_lib.Data.surrounding_levels_encoding))
    );
    ( Re.str "/ping",
      fun _g ~logger:_ ~conf:_ ~admins:_ ~users:_ _db_pool _header meth _body ->
        get_only_endpoint meth (fun () ->
            let status = `OK in
            let body = Cohttp.Code.string_of_status status in
            Cohttp_lwt_unix.Server.respond_string ~status ~body ()) );
    ( Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str ".json"],
      fun g ~logger ~conf:_ ~admins:_ ~users:_ db_pool _header meth _body ->
        get_only_endpoint meth (fun () ->
            let level = Int32.of_string (Re.Group.get g 1) in
            with_caqti_error
              ~logger
              (Exporter.data_at_level_range db_pool (level, level))
              (fun data ->
                reply_public_json Teztale_lib.Data.encoding (List.hd data).data))
    );
    ( Re.seq
        [
          Re.str "/";
          Re.group (Re.rep1 Re.digit);
          Re.str "-";
          Re.group (Re.rep1 Re.digit);
          Re.str ".json";
        ],
      fun g ~logger ~conf ~admins:_ ~users:_ db_pool header meth _body ->
        get_only_endpoint meth (fun () ->
            let min = Re.Group.get g 1 in
            let max = Re.Group.get g 2 in
            let min' = int_of_string min in
            let max' = int_of_string max in
            if
              min' > max'
              || max' - min' > Int32.to_int conf.Config.max_batch_size
            then
              let body = "Invalid block range." in
              Cohttp_lwt_unix.Server.respond_error ~body ()
            else
              with_caqti_error
                ~logger
                (Exporter.data_at_level_range
                   db_pool
                   (Int32.of_string min, Int32.of_string max))
                (fun data ->
                  reply_public_compressed_json
                    header
                    Teztale_lib.Data.batch_encoding
                    data)) );
    ( Re.seq
        [
          Re.str "/";
          Re.group (Re.rep1 Re.digit);
          Re.str "-";
          Re.group (Re.rep1 Re.digit);
          Re.str "/anomalies.json";
        ],
      fun g ~logger ~conf:_ ~admins:_ ~users:_ db_pool _header meth _body ->
        get_only_endpoint meth (fun () ->
            let first_level = int_of_string (Re.Group.get g 1) in
            let last_level = int_of_string (Re.Group.get g 2) in
            with_caqti_error
              ~logger
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
      fun _g ~logger ~conf:_ ~admins ~users db_pool header meth body ->
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
                      ~logger
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
                      ~logger
                      (Lwt_result.bind (delete_user db_pool login) (fun () ->
                           refresh_users db_pool users))
                      (reply_ok login))
            | `OPTIONS -> options_respond methods
            | _ -> method_not_allowed_respond methods) );
    ( Re.str "/users",
      fun _g ~logger ~conf:_ ~admins:_ ~users:_ db_pool _header meth _body ->
        get_only_endpoint meth (fun () -> get_users ~logger db_pool) );
    ( Re.str "/stats.json",
      fun _g ~logger ~conf:_ ~admins:_ ~users:_ db_pool _header _meth _body ->
        get_stats ~logger db_pool );
    ( Re.str "/head.json",
      fun _g ~logger ~conf:_ ~admins:_ ~users:_ db_pool _header _meth _body ->
        get_head ~logger db_pool );
  ]
  |> List.map (fun (r, fn) -> (r |> Re.whole_string |> Re.compile, fn))

let callback ~conf ~admins ~users db_pool _connection request body =
  let logger = Teztale_lib.Log.logger () in
  let header = Cohttp.Request.headers request in
  let meth = Cohttp.Request.meth request in
  let uri = Cohttp.Request.uri request in
  let path = Uri.path uri in
  Teztale_lib.Log.info logger (fun () ->
      Printf.sprintf "%s %s" (Cohttp.Code.string_of_method meth) path) ;
  match
    List.find_map
      (fun (r, fn) ->
        match Re.exec_opt r path with Some g -> Some (fn g) | None -> None)
      routes
  with
  | Some fn -> fn ~logger ~conf ~admins ~users db_pool header meth body
  | None -> (
      match conf.Config.public_directory with
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

let conf =
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
  Teztale_lib.Log.verbosity := conf.Config.verbosity ;
  let logger = Teztale_lib.Log.logger () in
  let uri = Uri.of_string conf.Config.db_uri in
  let users = ref [] in
  let admins = List.map (fun (u, p) -> (u, Bcrypt.hash p)) conf.Config.admins in
  let code =
    Lwt_main.run
      (match Caqti_lwt.connect_pool ~env:Sql_requests.env uri with
      | Error e ->
          Lwt.bind
            (Lwt_io.eprintl (Caqti_error.show e))
            (fun () -> Lwt.return 1)
      | Ok pool ->
          Lwt.bind (maybe_alter_and_create_tables pool) (function
              | Error e ->
                  Lwt.bind
                    (Lwt_io.eprintl (Caqti_error.show e))
                    (fun () -> Lwt.return 1)
              | Ok () ->
                  let stop, paf = Lwt.task () in
                  let shutdown _ = Lwt.wakeup paf () in
                  let _ = Sys.signal Sys.sigint (Sys.Signal_handle shutdown) in
                  Lwt.bind
                    (Lwt.map
                       (List.find_map (function
                           | Error e -> Some e
                           | _ -> None))
                       (Lwt_list.map_s
                          (fun (login, password) ->
                            let password = Bcrypt.hash password in
                            upsert_user pool login password)
                          conf.Config.users))
                    (function
                      | Some e ->
                          Lwt.bind
                            (Lwt_io.eprintl (Caqti_error.show e))
                            (fun () -> Lwt.return 1)
                      | None ->
                          Lwt.bind (refresh_users pool users) (function
                              | Error e ->
                                  Lwt.bind
                                    (Lwt_io.eprintl (Caqti_error.show e))
                                    (fun () -> Lwt.return 1)
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
                                            Teztale_lib.Log.info
                                              logger
                                              (fun () ->
                                                Printf.sprintf
                                                  "Server listening at %s:%d."
                                                  (match con.Config.source with
                                                  | None -> "<default>"
                                                  | Some s -> s)
                                                  con.port) ;
                                            Cohttp_lwt_unix.Server.create
                                              ~stop
                                              ~ctx
                                              ~mode
                                              (Cohttp_lwt_unix.Server.make
                                                 ~callback:
                                                   (callback
                                                      ~conf
                                                      ~admins
                                                      ~users
                                                      pool)
                                                 ())))
                                      conf.Config.network_interfaces
                                  in
                                  Lwt.bind (Lwt.join servers) (fun () ->
                                      Lwt.return 0)))))
  in
  exit code
