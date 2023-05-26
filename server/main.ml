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

let rec find_in_ocamlres path dir =
  match (path, dir) with
  | [], _ | _ :: _, [] -> None
  | [x], `File (y, a) :: t ->
      if String.equal x y then Some a else find_in_ocamlres path t
  | _ :: _ :: _, `File _ :: t -> find_in_ocamlres path t
  | [_], _ :: t -> find_in_ocamlres path t
  | x :: p, `Dir (y, a) :: t ->
      if String.equal x y then find_in_ocamlres p a else find_in_ocamlres path t

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

let with_source_authentification rights header f =
  match Cohttp.Header.get_authorization header with
  | Some (`Basic ((user, _) as login)) when List.mem login rights -> f user
  | Some (`Basic _) ->
      Cohttp_lwt_unix.Server.respond_string
        ~headers:
          (Cohttp.Header.init_with "content-type" "text/plain; charset=UTF-8")
        ~status:`Forbidden
        ~body:"Unknown source/Wrong login"
        ()
  | Some _ | None ->
      Cohttp_lwt_unix.Server.respond_need_auth ~auth:(`Basic "Who are you?") ()

let post_only_endpoint rights header meth f =
  let methods = [`POST; `OPTIONS] in
  match meth with
  | `POST -> with_source_authentification rights header f
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

let reply_public_json encoding data =
  let body =
    Ezjsonm.value_to_string (Data_encoding.Json.construct encoding data)
  in
  let headers =
    Cohttp.Header.init_with "content-type" "application/json; charset=UTF-8"
  in
  let headers = Cohttp.Header.add headers "Access-Control-Allow-Origin" "*" in
  Cohttp_lwt_unix.Server.respond_string ~headers ~status:`OK ~body ()

let one_level_json =
  Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str ".json"]
  |> Re.whole_string |> Re.compile

let anomalies_range =
  Re.seq
    [
      Re.str "/";
      Re.group (Re.rep1 Re.digit);
      Re.str "-";
      Re.group (Re.rep1 Re.digit);
      Re.str "/anomalies.json";
    ]
  |> Re.whole_string |> Re.compile

let level_rights =
  Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str "/rights"]
  |> Re.whole_string |> Re.compile

let level_block =
  Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str "/block"]
  |> Re.whole_string |> Re.compile

let level_mempool =
  Re.seq [Re.str "/"; Re.group (Re.rep1 Re.digit); Re.str "/mempool"]
  |> Re.whole_string |> Re.compile

let get_summery_query =
  Caqti_request.Infix.(
    Caqti_type.(unit ->! tup3 int32 int32 Sql_requests.Type.time_protocol))
    "SELECT level, round, timestamp FROM blocks ORDER BY level, round LIMIT 1"

let get_summary db_pool =
  let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax in
  let* max_level, round, timestamp =
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) -> Db.find get_summery_query ())
      db_pool
  in
  return
    (Format.asprintf
       "<!DOCTYPE html><html><head><title>Teztale \
        status</title></head><body><h1>Teztale status</h1><p>Highest recorded \
        block is level %li round %li (%a)</p><p><a \
        href=\"visualization/\">Vizualize data</a></p></body></html>"
       max_level
       round
       Tezos_base.Time.Protocol.pp_hum
       timestamp)

let get_head db_pool =
  let query =
    Caqti_request.Infix.(Caqti_type.(unit ->! int32))
      "SELECT MAX (level) FROM blocks"
  in
  with_caqti_error
    (Caqti_lwt.Pool.use
       (fun (module Db : Caqti_lwt.CONNECTION) -> Db.find query ())
       db_pool)
    (fun head_level ->
      reply_public_json Data_encoding.(obj1 (req "level" int32)) head_level)

let maybe_create_tables db_pool =
  Caqti_lwt.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.with_transaction (fun () ->
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun req ->
              Db.exec (Caqti_request.Infix.(Caqti_type.(unit ->. unit)) req) ())
            Sql_requests.create_tables))
    db_pool

let maybe_alter_tables db_pool =
  Caqti_lwt.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.with_transaction (fun () ->
          Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
            (fun req ->
              Lwt.bind
                (Db.exec
                   (Caqti_request.Infix.(Caqti_type.(unit ->. unit)) req)
                   ())
                (* IF NOT EXISTS expression not supported by sqlite instead ignore error if column already exists *)
                  (fun _ -> Lwt.return_ok ()))
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
              (op.delegate, op.op.kind = Endorsement, op.op.round, level) ))
      operations
  in
  Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
    (fun op ->
      Db.exec
        Sql_requests.insert_included_operation
        ( Teztale_lib.Consensus_ops.
            (op.delegate, op.op.kind = Endorsement, op.op.round, op.op.round),
          (block_hash, level) ))
    operations

let endorsing_rights_callback db_pool g rights =
  let level = Int32.of_string (Re.Group.get g 1) in
  let out =
    let open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax in
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        Db.with_transaction (fun () ->
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
              rights))
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
        Db.with_transaction (fun () ->
            let* () = Db.exec Sql_requests.maybe_insert_source source in
            let level = Int32.of_string (Re.Group.get g 1) in
            let* () =
              Db.exec
                Sql_requests.maybe_insert_block
                ((level, timestamp, hash, round), (predecessor, delegate))
            in

            let* () =
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun (_, reception_time) ->
                  Db.exec
                    Sql_requests.insert_received_block
                    (reception_time, hash, source))
                reception_times
            in
            let* () =
              insert_operations_from_block
                (module Db)
                (Int32.pred level)
                hash
                endorsements
            in
            insert_operations_from_block (module Db) level hash preendorsements))
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
        Db.with_transaction (fun () ->
            let* () = Db.exec Sql_requests.maybe_insert_source source in
            let* () =
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun (delegate, _) ->
                  Db.exec Sql_requests.maybe_insert_delegate delegate)
                operations
            in
            let* () =
              Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                (fun (delegate, ops) ->
                  Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                    (fun (op : Teztale_lib.Consensus_ops.received_operation) ->
                      Db.exec
                        Sql_requests.maybe_insert_operation
                        Teztale_lib.Consensus_ops.
                          ( ( level,
                              op.op.hash,
                              op.op.kind = Endorsement,
                              op.op.round ),
                            ( delegate,
                              op.op.kind = Endorsement,
                              op.op.round,
                              level ) ))
                    ops)
                operations
            in
            Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
              (fun (delegate, ops) ->
                Tezos_lwt_result_stdlib.Lwtreslib.Bare.List.iter_es
                  (fun op ->
                    Db.exec
                      Sql_requests.insert_received_operation
                      Teztale_lib.Consensus_ops.
                        ( ( op.reception_time,
                            op.errors,
                            delegate,
                            op.op.kind = Endorsement ),
                          (op.op.round, op.op.round, source, level) ))
                  ops)
              operations))
      db_pool
  in
  with_caqti_error out (fun () ->
      Cohttp_lwt_unix.Server.respond_string
        ~headers:
          (Cohttp.Header.init_with "content-type" "text/plain; charset=UTF-8")
        ~status:`OK
        ~body:"Received operations stored"
        ())

let callback rights db_pool _connection request body =
  let header = Cohttp.Request.headers request in
  let meth = Cohttp.Request.meth request in
  let uri = Cohttp.Request.uri request in
  let path = Uri.path uri in
  match Re.exec_opt level_rights path with
  | Some g ->
      post_only_endpoint rights header meth (fun _source ->
          with_data
            Teztale_lib.Consensus_ops.rights_encoding
            body
            (endorsing_rights_callback db_pool g))
  | None -> (
      match Re.exec_opt level_block path with
      | Some g ->
          post_only_endpoint rights header meth (fun source ->
              with_data
                Teztale_lib.Data.block_data_encoding
                body
                (block_callback db_pool g source))
      | None -> (
          match Re.exec_opt level_mempool path with
          | Some g ->
              post_only_endpoint rights header meth (fun source ->
                  with_data
                    Teztale_lib.Consensus_ops.delegate_ops_encoding
                    body
                    (operations_callback db_pool g source))
          | None -> (
              match Re.exec_opt one_level_json path with
              | Some g -> (
                  let methods = [`GET; `OPTIONS] in
                  match meth with
                  | `GET ->
                      let level = Int32.of_string (Re.Group.get g 1) in
                      with_caqti_error
                        (Exporter.data_at_level db_pool level)
                        (fun data ->
                          reply_public_json Teztale_lib.Data.encoding data)
                  | `OPTIONS -> options_respond methods
                  | _ -> method_not_allowed_respond methods)
              | None -> (
                  match Re.exec_opt anomalies_range path with
                  | Some g -> (
                      let methods = [`GET; `OPTIONS] in
                      match meth with
                      | `GET ->
                          let first_level = int_of_string (Re.Group.get g 1) in
                          let last_level = int_of_string (Re.Group.get g 2) in
                          with_caqti_error
                            (let levels =
                               Stdlib.List.init
                                 (last_level - first_level + 1)
                                 (fun i -> Int32.of_int (first_level + i))
                             in
                             Tezos_lwt_result_stdlib.Lwtreslib.Bare.List
                             .concat_map_es
                               (Exporter.anomalies_at_level db_pool)
                               levels)
                            (fun data ->
                              let body =
                                Ezjsonm.value_to_string
                                  (Data_encoding.Json.construct
                                     (Data_encoding.list
                                        Teztale_lib.Data.Anomaly.encoding)
                                     data)
                              in
                              Cohttp_lwt_unix.Server.respond_string
                                ~headers:
                                  (Cohttp.Header.init_with
                                     "content-type"
                                     "application/json; charset=UTF-8")
                                ~status:`OK
                                ~body
                                ())
                      | `OPTIONS -> options_respond methods
                      | _ -> method_not_allowed_respond methods)
                  | None -> (
                      let subpath = String.split_on_char '/' path in
                      match subpath with
                      | [] | [_] | [""; ""] | [_; "index.html"] ->
                          with_caqti_error (get_summary db_pool) (fun body ->
                              Cohttp_lwt_unix.Server.respond_string
                                ~headers:
                                  (Cohttp.Header.init_with
                                     "content-type"
                                     "text/html; charset=UTF-8")
                                ~status:`OK
                                ~body
                                ())
                      | [_; "head.json"] -> get_head db_pool
                      | [_; "visualization"; "js"; "local_config.js"] ->
                          let body = "const server_adress = \"../\";" in
                          Cohttp_lwt_unix.Server.respond_string
                            ~headers:
                              (Cohttp.Header.init_with
                                 "content-type"
                                 "text/javascript; charset=UTF-8")
                            ~status:`OK
                            ~body
                            ()
                      | _ :: pre :: subpath -> (
                          if pre <> "visualization" then
                            Cohttp_lwt_unix.Server.respond_not_found ~uri ()
                          else
                            let path, subpath =
                              match subpath with
                              | [] | [""] -> ("index.html", ["index.html"])
                              | _ -> (path, subpath)
                            in
                            match
                              find_in_ocamlres subpath Visualization.root
                            with
                            | Some body ->
                                Cohttp_lwt_unix.Server.respond_string
                                  ~headers:
                                    (Cohttp.Header.init_with
                                       "content-type"
                                       (Magic_mime.lookup path))
                                  ~status:`OK
                                  ~body
                                  ()
                            | None ->
                                Cohttp_lwt_unix.Server.respond_not_found ~uri ()
                          ))))))

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
  Lwt_main.run
    (match Caqti_lwt.connect_pool uri with
    | Error e -> Lwt_io.eprintl (Caqti_error.show e)
    | Ok pool ->
        Lwt.bind (maybe_alter_and_create_tables pool) (function
            | Error e -> Lwt_io.eprintl (Caqti_error.show e)
            | Ok () ->
                let stop, paf = Lwt.task () in
                let shutdown _ = Lwt.wakeup paf () in
                let _ = Sys.signal Sys.sigint (Sys.Signal_handle shutdown) in
                let servers =
                  List.map
                    (fun con ->
                      Lwt.bind
                        (Conduit_lwt_unix.init ?src:con.Config.source ())
                        (fun ctx ->
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
                          Format.printf
                            "Server listening at %a:%d@."
                            (Format.pp_print_option
                               ~none:(fun f () ->
                                 Format.pp_print_string f "<default>")
                               Format.pp_print_string)
                            con.Config.source
                            con.port ;
                          Cohttp_lwt_unix.Server.create
                            ~stop
                            ~ctx
                            ~mode
                            (Cohttp_lwt_unix.Server.make
                               ~callback:(callback config.Config.users pool)
                               ())))
                    config.Config.network_interfaces
                in
                Lwt.join servers))
