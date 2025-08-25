(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Agent_state
open Jingoo.Jg_types

type _ key += Http_server_k : Http_server.t key | Continue_k : bool key

module Http_server_key = struct
  type t = unit

  type r = Http_server.t

  let proj : type a. a key -> (t * (a, r) eq) option = function
    | Http_server_k -> Some ((), Eq)
    | _ -> None

  let compare () () = 0
end

module Continue_key = struct
  type t = unit

  type r = bool

  let proj : type a. a key -> (t * (a, r) eq) option = function
    | Continue_k -> Some ((), Eq)
    | _ -> None

  let compare () () = 0
end

let () =
  register_key (module Http_server_key) ;
  register_key (module Continue_key)

type ('a, 'uri) Remote_procedure.t += Quit : (unit, 'uri) Remote_procedure.t

module Quit = struct
  let name = "internals.quit"

  type 'uri t = unit

  type r = unit

  let of_remote_procedure : type a.
      (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Quit -> Some ()
    | _ -> None

  let to_remote_procedure () = Quit

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Quit -> Eq
    | _ -> Neq

  let encoding _uri_encoding = Data_encoding.empty

  let r_encoding = Data_encoding.empty

  let tvalue_of_r () = Tnull

  let expand ~self:_ ~run:_ () = ()

  let resolve ~self:_ _resolver () = ()

  let run state () =
    Agent_state.add Continue_k false state ;
    let* () = Process.clean_up () in
    unit

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_ () = ()
end

let agent_should_continue state =
  Agent_state.find ~default:true Continue_k state

let () = Remote_procedure.register (module Quit)

type start_http_server = {directory : string; http_port : string option}

type start_http_server_r = {port : int}

type ('a, 'uri) Remote_procedure.t +=
  | Start_http_server :
      start_http_server
      -> (start_http_server_r, 'uri) Remote_procedure.t

module Start_http_server = struct
  let name = "builtins.start_http_server"

  type 'uri t = start_http_server

  type r = start_http_server_r

  let of_remote_procedure : type a.
      (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Start_http_server args -> Some args
    | _ -> None

  let to_remote_procedure args = Start_http_server args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Start_http_server _ -> Eq
    | _ -> Neq

  let encoding _uri_encoding =
    Data_encoding.(
      conv
        (fun {directory; http_port} -> (directory, http_port))
        (fun (directory, http_port) -> {directory; http_port})
        (obj2 (dft "directory" string ".") (opt "http_port" string)))

  let r_encoding =
    Data_encoding.(
      conv
        (function {port : int} -> port)
        (fun port -> {port})
        (obj1 (req "port" int31)))

  let tvalue_of_r {port} = Tobj [("port", Tint port)]

  let expand ~self:_ ~run {directory; http_port} =
    let directory = run directory in
    let http_port = Option.map run http_port in
    {directory; http_port}

  let resolve ~self:_ _resolver {directory; http_port} = {directory; http_port}

  let run state args =
    match Agent_state.find_opt Http_server_k state with
    | Some _ ->
        Test.fail "An HTTP server has already been started on this agent"
    | None ->
        let port = Option.map int_of_string args.http_port in
        let http = Http_server.create ~directory:args.directory ?port () in
        let* () = Http_server.run http in
        let* () = Http_server.wait_for_ready http in
        Agent_state.add Http_server_k http state ;
        return {port = Http_server.port http}

  let on_completion ~on_new_service ~on_new_metrics_source:_ res =
    let open Services_cache in
    on_new_service "http" Http_server Http res.port
end

let () = Remote_procedure.register (module Start_http_server)

type action = Create | Extract

let action_encoding =
  Data_encoding.string_enum [("create", Create); ("extract", Extract)]

type 'uri tar = {contents : string; archive : 'uri; action : action}

type ('a, 'uri) Remote_procedure.t +=
  | Tar : 'uri tar -> (unit, 'uri) Remote_procedure.t

module Tar = struct
  let name = "builtins.tar"

  type 'uri t = 'uri tar

  type r = unit

  let of_remote_procedure : type a.
      (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Tar args -> Some args
    | _ -> None

  let to_remote_procedure args = Tar args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Tar _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    Data_encoding.(
      conv
        (fun {contents; archive; action} -> (contents, archive, action))
        (fun (contents, archive, action) -> {contents; archive; action})
        (obj3
           (req "contents" string)
           (req "archive" uri_encoding)
           (req "action" action_encoding)))

  let r_encoding = Data_encoding.empty

  let tvalue_of_r () = Tnull

  let expand ~self ~run args =
    let contents = run args.contents in
    let archive =
      Remote_procedure.global_uri_of_string ~self ~run args.archive
    in
    {args with contents; archive}

  let resolve ~self resolver args =
    let archive =
      Remote_procedure.file_agent_uri ~self ~resolver args.archive
    in
    {args with archive}

  let run state args =
    match args.action with
    | Extract ->
        let* archive =
          Http_client.local_path_from_agent_uri
            (Agent_state.http_client state)
            args.archive
        in
        let* () = Helpers.mkdir ~p:true args.contents in
        Helpers.exec "tar" ["xzvf"; archive; "-C"; args.contents]
    | Create -> (
        match args.archive with
        | Owned {name = archive} ->
            let* () = Helpers.mkdir ~p:true (Filename.dirname archive) in
            Helpers.exec "tar" ["czvf"; archive; args.contents]
        | _ -> Test.fail "Cannot create a remote archive")

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_ () = ()
end

let () = Remote_procedure.register (module Tar)

type 'uri prefetch = {uri : 'uri}

type ('a, 'uri) Remote_procedure.t +=
  | Prefetch : 'uri prefetch -> (unit, 'uri) Remote_procedure.t

module Prefetch = struct
  let name = "builtins.prefetch"

  type 'uri t = 'uri prefetch

  type r = unit

  let of_remote_procedure : type a.
      (a, 'uri) Remote_procedure.t -> 'uri t option = function
    | Prefetch args -> Some args
    | _ -> None

  let to_remote_procedure args = Prefetch args

  let unify : type a. (a, 'uri) Remote_procedure.t -> (a, r) Remote_procedure.eq
      = function
    | Prefetch _ -> Eq
    | _ -> Neq

  let encoding uri_encoding =
    Data_encoding.(conv (fun {uri} -> uri) (fun uri -> {uri}) uri_encoding)

  let r_encoding = Data_encoding.empty

  let tvalue_of_r () = Tnull

  let expand ~self ~run {uri} =
    let uri = Remote_procedure.global_uri_of_string ~self ~run uri in
    {uri}

  let resolve ~self resolver {uri} =
    let uri = Remote_procedure.file_agent_uri ~self ~resolver uri in
    {uri}

  let run state args =
    let* name =
      Http_client.local_path_from_agent_uri
        ~keep_name:true
        (Agent_state.http_client state)
        args.uri
    in
    let* () = Helpers.exec "chmod" ["+x"; name] in
    let* () = Helpers.exec "mv" [name; Agent_state.home_dir state] in
    unit

  let on_completion ~on_new_service:_ ~on_new_metrics_source:_ () = ()
end

let () = Remote_procedure.register (module Prefetch)
