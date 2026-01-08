(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2022 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let maybe_with_transaction (c : Config.t) (module D : Caqti_lwt.CONNECTION) f =
  if c.with_transaction <> NONE then D.with_transaction f else f ()

let maybe_with_metrics = Sql_requests.maybe_with_metrics

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
  let open Lwt.Syntax in
  let* str = Cohttp_lwt.Body.to_string body in
  match Ezjsonm.from_string_result str with
  | Error err ->
      Cohttp_lwt_unix.Server.respond_string
        ~headers:
          (Cohttp.Header.init_with "content-type" "text/plain; charset=UTF-8")
        ~status:`Bad_request
        ~body:(Ezjsonm.read_error_description err)
        ()
  | Ok json -> f json

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
        Lib_teztale_base.Log.error logger (fun () -> body) ;
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
      Caqti_type.(unit ->? t3 int32 int32 Sql_requests.Type.time_protocol))
      "SELECT level, round, timestamp FROM blocks ORDER BY level DESC, round \
       DESC LIMIT 1"
  in
  let version_encoding =
    Data_encoding.(
      obj4
        (req "teztale_version" string)
        (req "teztale_commit_ref" string)
        (req "tezos_branch" string)
        (req "tezos_commit_ref" string))
  in
  let highest_block_encoding =
    Data_encoding.(
      obj3 (req "level" int32) (req "round" int32) (req "timestamp" string))
  in
  let version =
    Lib_teztale_base.Version.
      (teztale_version, teztale_commit_ref, tezos_branch, tezos_commit_ref)
  in
  with_caqti_error
    ~logger
    (Caqti_lwt_unix.Pool.use
       (fun (module Db : Caqti_lwt.CONNECTION) -> Db.find_opt query ())
       db_pool)
    (fun highest_block ->
      let highest_block =
        match highest_block with
        | Some (level, round, timestamp) ->
            Some (level, round, Tezos_base.Time.Protocol.to_notation timestamp)
        | None -> None
      in
      reply_public_json
        Data_encoding.(
          obj2
            (opt "highest_block" highest_block_encoding)
            (req "version" version_encoding))
        (highest_block, version))

let get_available_data ~logger ~conf db_pool boundaries =
  let select_available_data_request =
    Caqti_request.Infix.(
      Caqti_type.(t2 int32 int32) ->* Caqti_type.(t3 int32 int32 bool))
      "SELECT \n\
      \  b.level,\n\
      \  b.round,\n\
      \  n.id IS NOT NULL\n\
      \  FROM blocks b\n\
      \  LEFT JOIN blocks n ON b.id = n.predecessor\n\
      \  WHERE b.level >= ? AND b.level <= ?"
  in
  with_caqti_error
    ~logger
    (Caqti_lwt_unix.Pool.use
       (fun (module Db : Caqti_lwt.CONNECTION) ->
         maybe_with_metrics conf "select_available_data" @@ fun () ->
         Db.fold select_available_data_request List.cons boundaries [])
       db_pool)
    (fun list ->
      reply_public_json Data_encoding.(list (tup3 int32 int32 bool)) list)

let get_missing_data ~logger ~conf db_pool boundaries =
  let select_missing_data_request =
    Caqti_request.Infix.(
      Caqti_type.(t2 int32 int32)
      ->* Caqti_type.(t4 int32 int32 Sql_requests.Type.public_key_hash bool))
      "SELECT DISTINCT\n\
      \  missing_blocks.level,\n\
      \  missing_blocks.round,\n\
      \  delegates.address,\n\
      \  EXISTS (\n\
      \   SELECT 1 FROM blocks\n\
      \   LEFT JOIN blocks_reception\n\
      \     ON blocks_reception.block = blocks.id\n\
      \   WHERE blocks.level = missing_blocks.level\n\
      \   AND blocks.round = missing_blocks.round\n\
      \   AND blocks_reception.id IS NOT NULL\n\
      \  )\n\
      \  FROM missing_blocks\n\
      \  JOIN delegates ON delegates.id = missing_blocks.baker\n\
      \  WHERE missing_blocks.level >= ? AND missing_blocks.level <= ?"
  in
  with_caqti_error
    ~logger
    (Caqti_lwt_unix.Pool.use
       (fun (module Db : Caqti_lwt.CONNECTION) ->
         maybe_with_metrics conf "select_missing_data" @@ fun () ->
         Db.fold select_missing_data_request List.cons boundaries [])
       db_pool)
    (fun list ->
      reply_public_json
        Data_encoding.(
          list
            (tup4
               int32
               int32
               Tezos_crypto.Signature.Public_key_hash.encoding
               bool))
        list)

let get_head ~logger conf db_pool =
  let query =
    Caqti_request.Infix.(Caqti_type.(unit ->? int32))
      "SELECT MAX (level) FROM blocks"
  in
  with_caqti_error
    ~logger
    (Caqti_lwt_unix.Pool.use
       (fun (module Db : Caqti_lwt.CONNECTION) ->
         maybe_with_metrics conf "get_head" @@ fun () -> Db.find_opt query ())
       db_pool)
    (fun level ->
      let open Data in
      let res = Option.map (fun level -> {level}) level in
      reply_public_json (Data_encoding.option head_encoding) res)

(** Fetch allowed users' logins from db. *)
let get_users ~logger conf db_pool =
  let query =
    Caqti_request.Infix.(Caqti_type.(unit ->* string))
      "SELECT name FROM nodes WHERE password IS NOT NULL"
  in
  with_caqti_error
    ~logger
    (Caqti_lwt_unix.Pool.use
       (fun (module Db : Caqti_lwt.CONNECTION) ->
         maybe_with_metrics conf "get_users" @@ fun () ->
         Db.collect_list query ())
       db_pool)
    (fun users -> reply_public_json Data_encoding.(list string) users)

let get_levels_at_timestamp0 db_pool timestamp =
  let open Lwt_result.Syntax in
  let lower =
    Caqti_request.Infix.(Caqti_type.(int32 ->* t2 int32 int32))
      "SELECT level, MIN(timestamp) FROM blocks, (SELECT MAX(level) AS m FROM \
       blocks WHERE timestamp <= ?) WHERE level = m OR level = m + 1 GROUP BY \
       level"
  in
  let upper =
    Caqti_request.Infix.(Caqti_type.(int32 ->* t2 int32 int32))
      "SELECT level, MIN(timestamp) FROM blocks, (SELECT MIN(level) AS m FROM \
       blocks WHERE timestamp >= ?) WHERE level = m OR level = m - 1 GROUP BY \
       level"
  in
  let* res =
    Caqti_lwt_unix.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        Db.collect_list lower timestamp)
      db_pool
  in
  match res with
  | [] ->
      Caqti_lwt_unix.Pool.use
        (fun (module Db : Caqti_lwt.CONNECTION) ->
          Db.collect_list upper timestamp)
        db_pool
  | l -> Lwt_result.return l

let get_levels_at_timestamp db_pool timestamp =
  let open Lwt_result.Syntax in
  let+ levels = get_levels_at_timestamp0 db_pool timestamp in
  match levels with
  | [] -> (None, None)
  | [((_, t) as r)] ->
      if Int32.compare timestamp t = -1 then (None, Some r) else (Some r, None)
  | ((_, t1) as a) :: ((_, t2) as b) :: _ ->
      if Int32.compare t1 t2 = -1 then (Some a, Some b) else (Some b, Some a)

(** Fetch the list of allowed logins from db and update the list ref given as parameter. *)
let refresh_users conf db_pool users =
  let open Lwt_result.Syntax in
  Lwt_mutex.with_lock Sql_requests.Mutex.nodes (fun () ->
      let query =
        Caqti_request.Infix.(
          Caqti_type.(unit ->* t2 string Sql_requests.Type.bcrypt_hash))
          "SELECT name, password FROM nodes WHERE password IS NOT NULL"
      in
      let+ users_from_db =
        Caqti_lwt_unix.Pool.use
          (fun (module Db : Caqti_lwt.CONNECTION) ->
            maybe_with_metrics conf "refresh_users" @@ fun () ->
            Db.collect_list query ())
          db_pool
      in
      users := users_from_db)

(** Insert a new user (i.e. a teztale archiver) into the database.
    If the user already exists, password is updated.
  *)
let upsert_user conf db_pool login password =
  let query =
    Caqti_request.Infix.(
      Caqti_type.(t2 string Sql_requests.Type.bcrypt_hash ->. unit))
      "INSERT INTO nodes(name, password) VALUES ($1, $2) ON CONFLICT (name) DO \
       UPDATE SET password = $2"
  in
  Caqti_lwt_unix.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      maybe_with_metrics conf "upsert_user" @@ fun () ->
      Lwt_mutex.with_lock Sql_requests.Mutex.nodes (fun () ->
          Db.exec query (login, password)))
    db_pool

(** Delete a user from db (i.e. removes feeder permissions) *)
let delete_user conf db_pool login =
  let query =
    Caqti_request.Infix.(Caqti_type.(string ->. unit))
      "UPDATE nodes SET password = NULL WHERE name = $1"
  in
  Caqti_lwt_unix.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      maybe_with_metrics conf "delete_user" @@ fun () -> Db.exec query login)
    db_pool

let maybe_create_tables db_pool =
  Caqti_lwt_unix.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
        (fun req ->
          Db.exec (Caqti_request.Infix.(Caqti_type.(unit ->. unit)) req) ())
        Sql_requests.create_tables)
    db_pool

let maybe_alter_tables db_pool =
  let open Lwt.Syntax in
  Caqti_lwt_unix.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      Lwt_list.iter_s
        (fun reqs ->
          let* res =
            Db.with_transaction (fun () ->
                Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                  (fun req ->
                    Db.exec
                      (Caqti_request.Infix.(Caqti_type.(unit ->. unit)) req)
                      ())
                  reqs)
          in
          match res with
          | Ok () -> Lwt.return_unit
          | Error e ->
              Lwt_io.eprintlf
                "\"ALTER TABLE ADD COLUMN IF NOT EXISTS\" expression is not \
                 supported by sqlite, if the following error is because the \
                 column already exists ignore it:\n\
                 %s"
                (Caqti_error.show e))
        Sql_requests.alter_tables
      |> Lwt_result.ok)
    db_pool

let maybe_alter_and_create_tables db_pool =
  let open Lwt_result.Syntax in
  let* () = maybe_alter_tables db_pool in
  maybe_create_tables db_pool

let with_cache mutex request mem add (module Db : Caqti_lwt.CONNECTION) conf
    list =
  let open Lwt_result.Syntax in
  (* Note: even if data is already in cache,
     we add it again at the end in order to mark it as recent. *)
  if conf.Config.with_transaction = FULL then
    Lwt_mutex.with_lock mutex @@ fun () ->
    Db.with_transaction @@ fun () ->
    let+ () =
      Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
        (fun x ->
          if mem x then
            Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax
            .return_unit
          else Db.exec request x)
        list
    in
    List.iter add list
  else
    Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
      (fun x ->
        let+ () =
          if mem x then
            Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax
            .return_unit
          else Db.exec request x
        in
        add x)
      list

let without_cache mutex request =
  (* We don't use the cache feature here,
     but we want to reuse the mutex and transactions handling from above.
     Typically used by functions when you need to process multiple requests
     before marking the key as done in an external cache,
     or when it is relevant to use a cache the whole list as single item.
     (e.g. use the level as cache key instead of one key for each list element) *)
  with_cache mutex request (fun _ -> false) (fun _ -> ())

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

let format_block_op level delegate
    (op : Lib_teztale_base.Consensus_ops.operation) =
  ((level, op.hash, op.kind = Attestation, op.round), delegate)

let attesting_rights_callback =
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
      let+ () =
        if Cache.mem cache level then
          let () =
            Lib_teztale_base.Log.debug logger (fun () ->
                Format.asprintf "(%ld) CACHE.mem Level rights" level)
          in
          return_unit
        else
          let* () =
            maybe_with_metrics conf "maybe_insert_attesting_right__list"
            @@ fun () ->
            Caqti_lwt_unix.Pool.use
              (fun (module Db : Caqti_lwt.CONNECTION) ->
                let* () =
                  let delegates =
                    List.map
                      (fun {Lib_teztale_base.Consensus_ops.address; _} ->
                        address)
                      rights
                  in
                  may_insert_delegates (module Db) conf delegates
                in
                let rights =
                  List.map
                    (fun Lib_teztale_base.Consensus_ops.
                           {address; first_slot; power}
                       -> (level, first_slot, power, address))
                    rights
                in
                without_cache
                  Sql_requests.Mutex.attesting_rights
                  Sql_requests.maybe_insert_attesting_right
                  (module Db)
                  conf
                  rights)
              db_pool
          in
          let () =
            Lib_teztale_base.Log.debug logger (fun () ->
                Format.asprintf "(%ld) CACHE.add Level rights" level)
          in
          return_unit
      in
      Cache.add cache level
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
        (fun {Lib_teztale_base.Consensus_ops.delegate; op; _} ->
          format_block_op level delegate op)
        operations
    in
    may_insert_operations (module Db) conf operations
  in
  let operation_inclusion =
    List.map
      (fun op ->
        ( Lib_teztale_base.Consensus_ops.
            (op.delegate, op.op.kind = Attestation, op.op.round),
          (block_hash, level) ))
      operations
  in
  maybe_with_metrics conf "insert_included_operations" @@ fun () ->
  without_cache
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
      ( Lib_teztale_base.Data.Block.
          {delegate; timestamp; reception_times; round; hash; predecessor; _},
        cycle_info,
        (attestations, preattestations),
        baking_rights )
    ->
    let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax in
    let level = Int32.of_string (Re.Group.get g 1) in
    let out =
      Caqti_lwt_unix.Pool.use
        (fun (module Db : Caqti_lwt.CONNECTION) ->
          let* () =
            (* Note: even if data is already in cache,
               we add it again in order to mark it as recent. *)
            let+ () =
              if Block_lru_cache.mem block_cache hash then
                let () =
                  Lib_teztale_base.Log.debug logger (fun () ->
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
                  maybe_with_metrics conf "maybe_insert_block" @@ fun () ->
                  without_cache
                    Sql_requests.Mutex.blocks
                    Sql_requests.maybe_insert_block
                    (module Db)
                    conf
                    [((level, timestamp, hash, round), (predecessor, delegate))]
                in
                let* () =
                  match cycle_info with
                  | Some
                      Lib_teztale_base.Data.{cycle; cycle_position; cycle_size}
                    ->
                      maybe_with_metrics conf "maybe_insert_cycle" @@ fun () ->
                      without_cache
                        Sql_requests.Mutex.cycles
                        Sql_requests.maybe_insert_cycle
                        (module Db)
                        conf
                        [(cycle, Int32.sub level cycle_position, cycle_size)]
                  | _ -> Lwt.return_ok ()
                in
                Lib_teztale_base.Log.debug logger (fun () ->
                    Format.asprintf
                      "(%ld) CACHE.add (block) %a"
                      level
                      Tezos_base.TzPervasives.Block_hash.pp
                      hash) ;
                return_unit
            in
            Block_lru_cache.add block_cache hash
          in
          let* () =
            (* Validated blocks do not provide operations only applied
               one does so we need a second cache *)
            (* Note: even if data is already in cache,
               we add it again in order to mark it as recent. *)
            let+ () =
              if Block_lru_cache.mem block_operations_cache hash then
                return_unit
              else
                let* () =
                  insert_operations_from_block
                    (module Db)
                    conf
                    (Int32.pred level)
                    hash
                    attestations
                in
                let* () =
                  insert_operations_from_block
                    (module Db)
                    conf
                    level
                    hash
                    preattestations
                in
                Lib_teztale_base.Log.debug logger (fun () ->
                    Format.asprintf
                      "(%ld) CACHE.add (block operations) %a"
                      level
                      Tezos_base.TzPervasives.Block_hash.pp
                      hash) ;
                return_unit
            in
            if attestations <> [] then
              Block_lru_cache.add block_operations_cache hash
          in
          let* () =
            maybe_with_metrics conf "insert_received_block__list" @@ fun () ->
            (* This data is non-redondant: we always treat it. *)
            maybe_with_transaction conf (module Db) @@ fun () ->
            Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
              (fun r ->
                let open Lib_teztale_base.Data.Block in
                Db.exec
                  Sql_requests.insert_received_block
                  (r.application_time, r.validation_time, hash, source))
              reception_times
          in
          let* () =
            match
              List.filter
                (fun x -> x.Lib_teztale_base.Data.round <> round)
                baking_rights
            with
            | [] -> return_unit
            | baking_rights ->
                maybe_with_metrics conf "insert_missing_block__list"
                @@ fun () ->
                maybe_with_transaction conf (module Db) @@ fun () ->
                Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                  (fun r ->
                    Db.exec
                      Sql_requests.insert_missing_block
                      ( source,
                        level,
                        r.Lib_teztale_base.Data.round,
                        r.Lib_teztale_base.Data.delegate ))
                  baking_rights
          in
          let* () =
            maybe_with_metrics conf "delete_missing_block__list" @@ fun () ->
            maybe_with_transaction conf (module Db) @@ fun () ->
            Db.exec Sql_requests.delete_missing_block (source, level, round)
          in
          Lib_teztale_base.Log.debug logger (fun () ->
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
    Caqti_lwt_unix.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        let* () =
          let delegates =
            List.map
              (fun ({Lib_teztale_base.Consensus_ops.address; _}, _) -> address)
              operations
          in
          may_insert_delegates (module Db) conf delegates
        in
        Lib_teztale_base.Log.debug logger (fun () ->
            Printf.sprintf "(%ld) OK Delegates" level) ;
        let* () =
          let operations =
            List.flatten
            @@ List.map
                 (fun (rights, operations) ->
                   let delegate =
                     rights.Lib_teztale_base.Consensus_ops.address
                   in
                   List.map
                     (fun (op :
                            Lib_teztale_base.Consensus_ops.received_operation)
                        -> format_block_op level delegate op.op)
                     operations)
                 operations
          in
          may_insert_operations (module Db) conf operations
        in
        Lib_teztale_base.Log.debug logger (fun () ->
            Printf.sprintf "(%ld) OK Operations" level) ;
        let* () =
          maybe_with_metrics conf "insert_received_operation__list" @@ fun () ->
          maybe_with_transaction conf (module Db) @@ fun () ->
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun (right, ops) ->
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun op ->
                  Db.exec
                    Sql_requests.insert_received_operation
                    Lib_teztale_base.Consensus_ops.
                      ( ( op.reception_time,
                          op.errors,
                          right.Lib_teztale_base.Consensus_ops.address,
                          op.op.kind = Attestation ),
                        (op.op.round, source, level) ))
                ops)
            operations
        in
        Lib_teztale_base.Log.debug logger (fun () ->
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
    Caqti_lwt_unix.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        (* delegates *)
        let* () =
          let delegates =
            List.rev_map
              (fun {Lib_teztale_base.Data.Delegate_operations.delegate; _} ->
                delegate)
              data.Lib_teztale_base.Data.delegate_operations
          in
          let delegates' =
            List.rev_map
              (fun {Lib_teztale_base.Data.Block.delegate; _} -> delegate)
              data.Lib_teztale_base.Data.blocks
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
                        inc.Lib_teztale_base.Data.Delegate_operations.source)
                    op
                      .Lib_teztale_base.Data.Delegate_operations
                       .mempool_inclusion)
                slot.Lib_teztale_base.Data.Delegate_operations.operations)
            data.Lib_teztale_base.Data.delegate_operations
        in
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun block ->
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun Lib_teztale_base.Data.Block.{source; _} ->
                  Db.exec Sql_requests.maybe_insert_source source)
                block.Lib_teztale_base.Data.Block.reception_times)
            data.Lib_teztale_base.Data.blocks
        in
        (* rights *)
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun Lib_teztale_base.Data.Delegate_operations.
                   {delegate; first_slot; attesting_power; _}
               ->
              Db.exec
                Sql_requests.maybe_insert_attesting_right
                (level, first_slot, attesting_power, delegate))
            data.Lib_teztale_base.Data.delegate_operations
        in
        (* blocks *)
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun block ->
              Db.exec
                Sql_requests.maybe_insert_block
                Lib_teztale_base.Data.Block.
                  ( (level, block.timestamp, block.hash, block.round),
                    (block.predecessor, block.delegate) ))
            data.Lib_teztale_base.Data.blocks
        in
        (* operations *)
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun Lib_teztale_base.Data.Delegate_operations.
                   {delegate; operations; _}
               ->
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun Lib_teztale_base.Data.Delegate_operations.
                       {hash; kind; round; _}
                   ->
                  Db.exec
                    Sql_requests.maybe_insert_operation
                    ( ( level,
                        hash,
                        kind = Lib_teztale_base.Consensus_ops.Attestation,
                        round ),
                      delegate ))
                operations)
            data.Lib_teztale_base.Data.delegate_operations
        in
        (* block reception *)
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun block ->
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun Lib_teztale_base.Data.Block.
                       {source; application_time; validation_time}
                   ->
                  Db.exec
                    Sql_requests.insert_received_block
                    ( application_time,
                      validation_time,
                      block.Lib_teztale_base.Data.Block.hash,
                      source ))
                block.Lib_teztale_base.Data.Block.reception_times)
            data.Lib_teztale_base.Data.blocks
        in
        (* operation reception *)
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun Lib_teztale_base.Data.Delegate_operations.
                   {delegate; operations; _}
               ->
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun Lib_teztale_base.Data.Delegate_operations.
                       {kind; round; mempool_inclusion; _}
                   ->
                  Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                    (fun Lib_teztale_base.Data.Delegate_operations.
                           {source; reception_time; errors}
                       ->
                      Db.exec
                        Sql_requests.insert_received_operation
                        ( ( reception_time,
                            errors,
                            delegate,
                            kind = Lib_teztale_base.Consensus_ops.Attestation ),
                          (round, source, level) ))
                    mempool_inclusion)
                operations)
            data.Lib_teztale_base.Data.delegate_operations
        in
        (* operation inclusion *)
        let* () =
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun Lib_teztale_base.Data.Delegate_operations.
                   {delegate; operations; _}
               ->
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun Lib_teztale_base.Data.Delegate_operations.
                       {kind; round; block_inclusion; _}
                   ->
                  Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                    (fun block_hash ->
                      Db.exec
                        Sql_requests.insert_included_operation
                        ( ( delegate,
                            kind = Lib_teztale_base.Consensus_ops.Attestation,
                            round ),
                          (block_hash, level) ))
                    block_inclusion)
                operations)
            data.Lib_teztale_base.Data.delegate_operations
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

let extract_boundaries g =
  let min = Re.Group.get g 1 in
  let max = Re.Group.get g 2 in
  (Int32.of_string min, Int32.of_string max)

(*
  - /<LEVEL>/rights
      Used by archiver to send attesting rights informations for a given level.
  - /<LEVEL>/block
      Used by archiver to send data about a block for a given level.
  - /<LEVEL>/mempool
      Used by archiver to send data about consensus operations for a given level.
  - /<LEVEL>/import
      Use by archiver to import past data recorded locally.
  - /timestamp/<TIMESTAMP>
      Get the levels (if any) before and after a given timestamp.
  - /ping
      Only return a 200 OK response with "200 OK" body content.
  - /<LEVEL>.json
      Get information about rounds, blocks, (pre)attestations for a given level.
  - /<LEVEL_MIN>-<LEVEL_MAX>.json
      Same as <LEVEL>.json, but for a range of levels. Min and max levels are included in data.
  - /<LEVEL_MIN>-<LEVEL_MAX>/anomalies.json
      Return all problem detected (missed|sequestred|forgotten attestation).
  - /users
      Get the list of allowed archivers
  - /stats.json
      Get the highest level, its highest round, and corresponding timestamp.
  - /available.json
      Get the list of (level, round) available in database.
  - /missing.json
      Get the list of (level, round, baker).
  - /head.json
      Get the highest level recorded in database.
  - /metrics
      Some openmetrics data about OCaml GC and SQL requests execution times.
*)
let routes :
    (Re.re
    * (Re.Group.t ->
      logger:Lib_teztale_base.Log.t ->
      conf:Config.t ->
      admins:(string * Bcrypt.hash) list ->
      users:(string * Bcrypt.hash) list ref ->
      (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t ->
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
              Lib_teztale_base.Consensus_ops.rights_encoding
              body
              (attesting_rights_callback ~logger conf db_pool g)) );
    ( Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str "/block"],
      fun g ~logger ~conf ~admins:_ ~users db_pool header meth body ->
        post_only_endpoint !users header meth (fun source ->
            with_data
              Lib_teztale_base.Data.Archiver.raw_block_data_encoding
              body
              (block_callback ~logger conf db_pool g source)) );
    ( Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str "/mempool"],
      fun g ~logger ~conf ~admins:_ ~users db_pool header meth body ->
        post_only_endpoint !users header meth (fun source ->
            with_data
              Lib_teztale_base.Consensus_ops.delegate_ops_encoding
              body
              (operations_callback ~logger conf db_pool g source)) );
    ( Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str "/import"],
      fun g ~logger ~conf ~admins ~users:_ db_pool header meth body ->
        post_only_endpoint admins header meth (fun _source ->
            with_data
              Lib_teztale_base.Data.encoding
              body
              (import_callback ~logger conf db_pool g)) );
    ( Re.seq [Re.str "/timestamp/"; Re.group (Re.rep1 Re.digit)],
      fun g ~logger ~conf:_ ~admins:_ ~users:_ db_pool _header meth _body ->
        get_only_endpoint meth (fun () ->
            let level = Int32.of_string (Re.Group.get g 1) in
            with_caqti_error
              ~logger
              (get_levels_at_timestamp db_pool level)
              (reply_public_json
                 Lib_teztale_base.Data.surrounding_levels_encoding)) );
    ( Re.str "/ping",
      fun _g ~logger:_ ~conf:_ ~admins:_ ~users:_ _db_pool _header meth _body ->
        get_only_endpoint meth (fun () ->
            let status = `OK in
            let body = Cohttp.Code.string_of_status status in
            Cohttp_lwt_unix.Server.respond_string ~status ~body ()) );
    ( Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str ".json"],
      fun g ~logger ~conf ~admins:_ ~users:_ db_pool _header meth _body ->
        get_only_endpoint meth (fun () ->
            let level = Int32.of_string (Re.Group.get g 1) in
            with_caqti_error
              ~logger
              (Exporter.data_at_level_range conf db_pool (level, level))
              (fun data ->
                reply_public_json
                  Lib_teztale_base.Data.encoding
                  (List.hd data).data)) );
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
            let min, max = extract_boundaries g in
            if min > max || Int32.sub max min > conf.Config.max_batch_size then
              let body = "Invalid block range." in
              Cohttp_lwt_unix.Server.respond_error ~body ()
            else
              with_caqti_error
                ~logger
                (Exporter.data_at_level_range conf db_pool (min, max))
                (fun data ->
                  reply_public_compressed_json
                    header
                    Lib_teztale_base.Data.batch_encoding
                    data)) );
    ( Re.str "/user",
      fun _g ~logger ~conf ~admins ~users db_pool header meth body ->
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
                    let open Lwt_result.Syntax in
                    let password = Bcrypt.hash password in
                    with_caqti_error
                      ~logger
                      (let* () = upsert_user conf db_pool login password in
                       refresh_users conf db_pool users)
                      (reply_ok login))
            | `DELETE ->
                with_data
                  Data_encoding.(obj1 (req "login" string))
                  body
                  (fun login ->
                    let open Lwt_result.Syntax in
                    with_caqti_error
                      ~logger
                      (let* () = delete_user conf db_pool login in
                       refresh_users conf db_pool users)
                      (reply_ok login))
            | `OPTIONS -> options_respond methods
            | _ -> method_not_allowed_respond methods) );
    ( Re.str "/users",
      fun _g ~logger ~conf ~admins:_ ~users:_ db_pool _header meth _body ->
        get_only_endpoint meth (fun () -> get_users conf ~logger db_pool) );
    ( Re.str "/stats.json",
      fun _g ~logger ~conf:_ ~admins:_ ~users:_ db_pool _header _meth _body ->
        get_stats ~logger db_pool );
    ( Re.seq
        [
          Re.str "/";
          Re.group (Re.rep1 Re.digit);
          Re.str "-";
          Re.group (Re.rep1 Re.digit);
          Re.str "/available.json";
        ],
      fun g ~logger ~conf ~admins:_ ~users:_ db_pool _header _meth _body ->
        let boundaries = extract_boundaries g in
        get_available_data ~logger ~conf db_pool boundaries );
    ( Re.seq
        [
          Re.str "/";
          Re.group (Re.rep1 Re.digit);
          Re.str "-";
          Re.group (Re.rep1 Re.digit);
          Re.str "/missing.json";
        ],
      fun g ~logger ~conf ~admins:_ ~users:_ db_pool _header _meth _body ->
        let boundaries = extract_boundaries g in
        get_missing_data ~logger ~conf db_pool boundaries );
    ( Re.str "/head.json",
      fun _g ~logger ~conf ~admins:_ ~users:_ db_pool _header _meth _body ->
        get_head conf ~logger db_pool );
    ( Re.str "/metrics",
      fun _g
          ~logger:_
          ~conf:_
          ~admins:_
          ~users:_
          _db_pool
          _header
          _meth
          _body
        ->
        let open Lwt.Syntax in
        let* data = Prometheus.CollectorRegistry.(collect default) in
        let body =
          Fmt.to_to_string Prometheus_app.TextFormat_0_0_4.output data
        in
        let headers =
          Cohttp.Header.init_with "Content-Type" "text/plain; version=0.0.4"
        in
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~headers ~body () );
  ]
  |> List.map (fun (r, fn) -> (r |> Re.whole_string |> Re.compile, fn))

let callback ~conf ~admins ~users db_pool _connection request body =
  let logger = Lib_teztale_base.Log.logger () in
  let header = Cohttp.Request.headers request in
  let meth = Cohttp.Request.meth request in
  let uri = Cohttp.Request.uri request in
  let path = Uri.path uri in
  Lib_teztale_base.Log.info logger (fun () ->
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
          let headers =
            Cohttp.Header.add_list
              (Cohttp.Header.init_with "content-type" (Magic_mime.lookup fname))
              [
                ("Access-Control-Allow-Origin", "*");
                ("Access-Control-Allow-Methods", "GET, POST, OPTIONS");
                ( "Access-Control-Allow-Headers",
                  "Access-Control-Allow-Headers, Origin,Accept, \
                   X-Requested-With, Content-Type, \
                   Access-Control-Request-Method, \
                   Access-Control-Request-Headers" );
                ( "Cache-Control",
                  "no-store, no-cache, must-revalidate, max-age=0" );
              ]
          in
          Cohttp_lwt_unix.Server.respond_file ~headers ~fname ())

(* Must exists somewhere but where ! *)
let print_location f ((fl, fc), (tl, tc)) =
  if Int.equal fl tl then
    if Int.equal fc tc then Format.fprintf f "line %i character %i" fl fc
    else Format.fprintf f "line %i characters %i-%i" fl fc tc
  else Format.fprintf f "lines %i-%i characters %i-%i" fl tl fc tc

let parse_conf file =
  (* [non_dir_file] from [Cmdliner] already checks that the file exists. *)
  let ic = open_in file in
  match Ezjsonm.from_channel_result ic with
  | Error err ->
      Format.eprintf
        "Error in %s: %a:@ %s@."
        file
        (Format.pp_print_option print_location)
        (Ezjsonm.read_error_location err)
        (Ezjsonm.read_error_description err) ;
      exit 1
  | Ok json -> (
      try Data_encoding.Json.destruct Config.encoding json
      with e ->
        let () =
          Format.eprintf
            "@[<v>@[Invalid configuration in %s:@ @[%a@]@]@ Configuration file \
             format is@ @[%a@]@]@."
            file
            (Data_encoding.Json.print_error ?print_unknown:None)
            e
            Json_schema.pp
            (Data_encoding.Json.schema Config.encoding)
        in
        exit 1)

let run (conf : Config.t) =
  let open Lwt.Syntax in
  Lib_teztale_base.Log.verbosity := conf.Config.verbosity ;
  let logger = Lib_teztale_base.Log.logger () in
  let uri = Uri.of_string conf.Config.db_uri in
  let users = ref [] in
  let admins = List.map (fun (u, p) -> (u, Bcrypt.hash p)) conf.Config.admins in
  let show_error_and_exit e =
    let* () = Lwt_io.eprintl (Caqti_error.show e) in
    Lwt.return 1
  in
  let ( let*! ) v f =
    let* res = v in
    match res with Error e -> show_error_and_exit e | Ok v -> f v
  in
  Lwt_main.run
    (let res = Caqti_lwt_unix.connect_pool ~env:Sql_requests.env uri in
     match res with
     | Error e -> show_error_and_exit e
     | Ok pool ->
         let*! () = maybe_alter_and_create_tables pool in
         let stop, paf = Lwt.task () in
         let shutdown _ = Lwt.wakeup paf () in
         let _ = Sys.signal Sys.sigint (Sys.Signal_handle shutdown) in
         let*! () =
           let+ list =
             Lwt_list.map_s
               (fun (login, password) ->
                 let password = Bcrypt.hash password in
                 upsert_user conf pool login password)
               conf.Config.users
           in
           List.find_map (function Error e -> Some e | _ -> None) list
           |> Option.fold ~none:(Ok ()) ~some:(fun e -> Error e)
         in
         let*! () = refresh_users conf pool users in
         let servers =
           List.map
             (fun con ->
               let* ctx = Conduit_lwt_unix.init ?src:con.Config.source () in
               let ctx = Cohttp_lwt_unix.Net.init ~ctx () in
               let mode =
                 match con.Config.tls with
                 | Some Config.{crt; key} ->
                     `TLS
                       ( `Crt_file_path crt,
                         `Key_file_path key,
                         `No_password,
                         `Port con.Config.port )
                 | None -> `TCP (`Port con.Config.port)
               in
               let promise =
                 Cohttp_lwt_unix.Server.create
                   ~stop
                   ~ctx
                   ~mode
                   (Cohttp_lwt_unix.Server.make
                      ~callback:(callback ~conf ~admins ~users pool)
                      ())
               in
               Lib_teztale_base.Log.info logger (fun () ->
                   Printf.sprintf
                     "Server listening at %s:%d."
                     (match con.Config.source with
                     | None -> "<default>"
                     | Some s -> s)
                     con.port) ;
               promise)
             conf.Config.network_interfaces
         in
         let* () = Lwt.join servers in
         Lwt.return 0)

let () =
  let open Cmdliner in
  let version = Tezos_version_value.Bin_version.octez_version_string in
  let doc = "The teztale archivers' aggregator/formatter" in
  let config =
    let doc = "The configuration file" in
    Arg.(
      required & pos 0 (some non_dir_file) None & info [] ~docv:"CONFIG" ~doc)
  in
  let action config = run (parse_conf config) in
  let info = Cmd.info Sys.executable_name ~version ~doc in
  let term = Term.(const action $ config) in
  exit @@ Cmd.eval' @@ Cmd.v info term
