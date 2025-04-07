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
    public_directory : string option;
  }

  type filenames = {conf_filename : string; db_filename : string}

  type t = {process : Process.t; filenames : filenames; conf : conf}

  let make_conf ~name ~address ~port ~users ~admin ?public_directory () =
    {name; interface = {address; port}; users; admin; public_directory}

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
    let pp_public_directory fmt public_directory =
      match public_directory with
      | None -> Format.fprintf fmt ""
      | Some str -> Format.fprintf fmt {|,
  "public_directory": "%s"|} str
    in
    let contents =
      (* Verbosity needs to be set to a minimum level of INFO
         because wait_for_readiness can't be used with a lower verbosity *)
      Format.asprintf
        {|{
  "db": "sqlite3:%s",
  "interfaces": [%a],
  "users": [%a],
  "admins": [%a],
  "with_transaction": "FULL",
  "verbosity": "INFO"%a
}|}
        db_filename
        pp_interface
        conf.interface
        pp_login_list
        conf.users
        pp_login
        conf.admin
        pp_public_directory
        conf.public_directory
    in
    let* () = Helpers.write_file ?runner ~contents conf_filename in
    Lwt.return {conf_filename; db_filename}

  let make ?name ?(address = "0.0.0.0") ?port ?(users = [])
      ?(admin = {login = "admin"; password = "password"}) ?public_directory () =
    let port = match port with Some port -> port | None -> Port.fresh () in
    let name = match name with Some name -> name | None -> fresh_name () in
    make_conf ~name ~address ~port ~users ~admin ?public_directory ()

  let run ?runner ?(path = Uses.path Constant.teztale_server) ?name ?address
      ?port ?users ?admin ?public_directory () =
    let conf = make ?name ?address ?port ?users ?admin ?public_directory () in
    let* filenames = dump_conf ?runner conf in
    let process =
      Process.spawn ~name:conf.name ?runner path [filenames.conf_filename]
    in
    Lwt.return {process; filenames; conf}

  let wait_for_readiness t =
    Log.info "Wait for %s to be ready" t.conf.name ;
    let stdout = Process.stdout t.process in
    let suffix =
      Printf.sprintf
        "Server listening at %s:%d."
        t.conf.interface.address
        t.conf.interface.port
    in
    let rec wait () =
      let* line = Lwt_io.read_line stdout in
      if String.ends_with ~suffix line then Lwt.return_unit else wait ()
    in
    wait ()

  let add_user ?runner {conf = {interface; admin; _}; _} ?public_address user =
    let user =
      JSON.parse
        ~origin:__LOC__
        (Printf.sprintf
           {|{"login": "%s", "password": "%s"}|}
           user.login
           user.password)
    in
    let address =
      match public_address with None -> interface.address | Some v -> v
    in
    let url =
      Format.asprintf
        "http://%s:%s@%s:%d/user"
        admin.login
        admin.password
        address
        interface.port
    in
    Curl.put ?runner url user |> Runnable.run
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
