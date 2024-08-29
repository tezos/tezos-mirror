(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type user = {login : string; password : string}

type interface = {address : string; port : int}

let fresh_name base =
  let i = ref (-1) in
  fun () ->
    incr i ;
    base ^ "-" ^ string_of_int !i

module Server = struct
  let fresh_name = fresh_name "teztale-server"

  type conf = {
    name : string;
    interface : interface;
    users : user list;
    admin : user;
  }

  type filenames = {conf_filename : string; db_filename : string}

  type t = {process : Process.t; filenames : filenames; conf : conf}

  let make_conf ~name ~address ~port ~users ~admin =
    {name; interface = {address; port}; users; admin}

  (* We could use the libs from teztale in order to build the conf file using ocaml types
     and printing it as json into a file, but using a string also tests that nothing
     changed/broke with existing conf files.
  *)
  let dump_conf ?runner conf =
    let tmp fn = Temp.file ?runner fn in
    let conf_filename = tmp (Printf.sprintf "%s.conf.json" conf.name) in
    let db_filename = tmp (Printf.sprintf "%s.sqlite" conf.name) in
    let pp_login fmt {login; password} =
      Format.fprintf fmt {|{"login": "%s", "password": "%s"}|} login password
    in
    let pp_login_list =
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt ";")
        pp_login
    in
    let pp_interface fmt {address; port} =
      Format.fprintf fmt {|{"address": "%s", "port": %d}|} address port
    in
    let contents =
      Format.asprintf
        {|{
  "db": "sqlite3:%s",
  "interfaces": [%a],
  "users": [%a],
  "admins": [%a],
  "with_transaction": "FULL"
}|}
        db_filename
        pp_interface
        conf.interface
        pp_login_list
        conf.users
        pp_login
        conf.admin
    in
    let* () =
      match runner with
      | None -> write_file conf_filename ~contents |> Lwt.return
      | Some runner ->
          let cmd =
            Runner.Shell.(
              redirect_stdout (cmd [] "echo" [contents]) conf_filename)
          in
          let cmd, args = Runner.wrap_with_ssh runner cmd in
          Process.run cmd args
    in
    Lwt.return {conf_filename; db_filename}

  let make ?name ?(address = "127.0.0.1") ?port ?(users = [])
      ?(admin = {login = "admin"; password = "password"}) () =
    let port = match port with Some port -> port | None -> Port.fresh () in
    let name = match name with Some name -> name | None -> fresh_name () in
    make_conf ~name ~address ~port ~users ~admin

  let run ?runner ?(path = Uses.path Constant.teztale_server) ?name ?address
      ?port ?users ?admin () =
    let conf = make ?name ?address ?port ?users ?admin () in
    let* filenames = dump_conf ?runner conf in
    let process =
      Process.spawn ~name:conf.name ?runner path [filenames.conf_filename]
    in
    Lwt.return {process; filenames; conf}

  (** Return *)
  let add_user {conf = {interface; admin; _}; _} user =
    let user =
      JSON.parse
        ~origin:__LOC__
        (Printf.sprintf
           {|{"login": "%s", "password": "%s"}|}
           user.login
           user.password)
    in
    let url =
      Format.asprintf
        "http://%s:%s@%s:%d/user"
        admin.login
        admin.password
        interface.address
        interface.port
    in
    Curl.put url user |> Runnable.run
    |> Lwt.map (fun json ->
           match JSON.(get "status" json |> as_string) with
           | "OK" -> Ok ()
           | status ->
               let msg =
                 Printf.sprintf "%s: teztale answered with: %s" __LOC__ status
               in
               Error (Failure msg)
           | exception e -> Error e)
end

module Archiver = struct
  let fresh_name = fresh_name "teztale-archiver"

  type conf = {name : string; user : user; feed : interface list}

  type t = {process : Process.t; conf : conf}

  let run ?runner ?(path = Uses.path Constant.teztale_archiver) ?name ~node_port
      user feed =
    let name = match name with Some name -> name | None -> fresh_name () in
    let node_endpoint = Format.asprintf "http://127.0.0.1:%d" node_port in
    let conf = {name; user; feed} in
    let args =
      "--endpoint" :: node_endpoint
      :: List.fold_left
           (fun acc feed ->
             "feed"
             :: Printf.sprintf
                  "http://%s:%s@%s:%d"
                  user.login
                  user.password
                  feed.address
                  feed.port
             :: acc)
           []
           conf.feed
    in
    let process = Process.spawn ~name:conf.name ?runner path args in
    Lwt.return {process; conf}
end
