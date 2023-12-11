(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Testing
   -------
   Component:    Client configuration
   Invocation:   dune exec tezt/tests/main.exe -- --file client_config.ml
   Subject:      .
*)

let iteri l f = Lwt_list.iteri_s f l

let iter l f = Lwt_list.iter_s f l

let additional_bootstrap_accounts =
  Protocol.register_test
    ~__FILE__
    ~title:"additional bootstrap accounts"
    ~tags:["client"; "bootstrap"; "accounts"]
  @@ fun protocol ->
  let* _node, client =
    Client.init_with_protocol
      ~additional_bootstrap_account_count:2
      `Client
      ~protocol
      ()
  in
  let* bootstrap6 = Client.show_address ~alias:"bootstrap6" client in
  let* bootstrap7 = Client.show_address ~alias:"bootstrap7" client in
  Client.transfer
    ~amount:(Tez.of_int 2)
    ~giver:bootstrap6.public_key_hash
    ~receiver:bootstrap7.public_key_hash
    client

let input_config_files : JSON.t option list =
  [
    None;
    Some
      (JSON.annotate ~origin:"example client configuration"
      @@ `O
           [
             ("confirmations", `Float 1.0);
             ("endpoint", `String "http://127.0.0.1:8732");
             ("remote_signer", `String "http://127.0.0.2");
             ("web_port", `Float 8080.0);
             ("password_filename", `String "/tmp/doesnt_exist");
           ]);
  ]

let write_config_file client (config : JSON.t) =
  let output_file = Temp.file "octez-client.config_file.json" in
  let config =
    JSON.put
      ( "base_dir",
        JSON.annotate ~origin:"Client.base_dir"
        @@ `String (Client.base_dir client) )
      config
  in
  Log.debug "Writing to %s: %s" output_file (JSON.encode config) ;
  JSON.encode_to_file output_file config ;
  (output_file, config)

(* Tests that calling [[--config-file config_dict]? config init -o tmp_file]
   works and yields valid json. *)
let test_config_init () =
  Test.register
    ~__FILE__
    ~title:"Config init"
    ~tags:["config"; "init"]
    ~uses_node:false
  @@ fun () ->
  let* client = Client.init () in
  iteri input_config_files @@ fun index input_config_opt ->
  let output =
    Temp.file (string_of_int index ^ "-octez-client.ouput_config_file.json")
  in
  let in_file =
    Option.map
      (fun input_config -> write_config_file client input_config |> fst)
      input_config_opt
  in
  let* () = Client.config_init ?config_file:in_file client ~output in
  (* Try loading the file as json, to check it is valid  *)
  let (output : JSON.t) = JSON.parse_file output in
  Log.debug "Received output config: %s" (JSON.encode output) ;
  unit

(* Tests that calling `config init -o tmp_file` and
   feeding its result to `octez-client --config-file` works
   and yields the same result (i.e. calling `octez-client
   --config-file tmp_file config init -o tmp_file2 yields
   a `tmp_file2` that is similar to `tmp_file`).
   `config_dict` specifies the content of the initial config file
   to use or None not to specify one. *)
let test_config_init_roundtrip () =
  Test.register
    ~__FILE__
    ~title:"Config init roundtrip"
    ~tags:["config"; "init"]
    ~uses_node:false
  @@ fun () ->
  Log.info "Config init roundtrip" ;
  let* client = Client.init () in
  iteri input_config_files @@ fun index input_config_opt ->
  let* in_file, json1 =
    match input_config_opt with
    | None ->
        let in_file =
          Temp.file
            (string_of_int index ^ "-octez-client.input_config_file.json")
        in
        let* () = Client.config_init ~output:in_file client in
        (* Execute an arbitrary effectless command to verify
           that the configuration file is valid. *)
        let* (_ : string list) =
          Client.list_understood_protocols ~config_file:in_file client
        in
        return (in_file, JSON.parse_file in_file)
    | Some input_config -> return (write_config_file client input_config)
  in
  let output =
    Temp.file (string_of_int index ^ "-octez-client.ouput_config_file.json")
  in
  let* () = Client.config_init ~config_file:in_file client ~output in
  (* Try loading the file as json, to check it is valid  *)
  let json2 = JSON.parse_file output in
  Check.((json1 = json2) json ~__LOC__ ~error_msg:"Expected %R, got %L") ;
  unit

(* Tests of `octez-client config show` *)
let test_config_show () =
  Test.register
    ~__FILE__
    ~title:"Config show"
    ~tags:["config"; "show"]
    ~uses_node:false
  @@ fun () ->
  let* client = Client.init () in
  Log.info "Config show" ;
  (* Tests that calling `config show` works, with or without
     specifying `--config-file` *)
  iter input_config_files @@ fun input_config_opt ->
  let config_file =
    Option.map
      (fun input_config -> write_config_file client input_config |> fst)
      input_config_opt
  in
  let* (_ : string) = Client.config_show ?config_file client in
  unit

let test_config_show_roundtrip () =
  Test.register
    ~__FILE__
    ~title:"Config show roundtrip"
    ~tags:["config"; "show"]
    ~uses_node:false
  @@ fun () ->
  Log.info "Config show roundtrip" ;
  let* client = Client.init () in
  let cmd_line_args =
    [
      [];
      [
        ("--endpoint", "http://127.0.0.1:9732");
        ("--wait", "3");
        ("--remote-signer", "http://10.0.0.2");
        ("--password-filename", "/tmp/doesnt_exist_either");
      ];
    ]
  in
  (* Tests calling `config show` with or without `--config-file`
     and with some command line parameters (`cmd_line_arg`). It
     then parses the output to check its valid json and to check
     that command line parameters were honored.Then it feeds this
     output to a new call to `--config-file file config show` and
     checks that the json returned by this second call agrees with
     what was specified by `file`. This is a roundtrip test using a
     small matrix. *)
  iteri input_config_files @@ fun index input_config_opt ->
  iter cmd_line_args @@ fun cmd_line_args ->
  let in_file1 =
    Option.map
      (fun input_config -> write_config_file client input_config |> fst)
      input_config_opt
  in
  let* output =
    (* Pass command line parameters *)
    let cmd =
      List.concat_map (fun (arg, value) -> [arg; value]) cmd_line_args
      @ ["config"; "show"]
    in
    (* Pass command line parameters *)
    Client.spawn_command ?config_file:in_file1 client cmd
    |> Process.check_and_read_stdout
  in
  let output_json1 = JSON.parse ~origin:"config show" output in

  (* Verify that command line parameters were honored *)
  let () =
    let cmd_line_flag_to_json_field = function
      | "--wait" -> "confirmations"
      | s ->
          let s' =
            if String.starts_with ~prefix:"--" s then
              String.(sub s 2 (length s - 2))
            else s
          in
          String.map (function '-' -> '_' | c -> c) s'
    in
    cmd_line_args
    |> List.iter @@ fun (flag, input_value) ->
       let stringify = function
         | `String s -> s
         | `Float f ->
             if Float.is_integer f then string_of_int (int_of_float f)
             else string_of_float f
         | json ->
             Test.fail
               "Unsupported command-line argument type: %s"
               (JSON.encode_u json)
       in
       let output_value =
         JSON.(
           output_json1
           |-> cmd_line_flag_to_json_field flag
           |> unannotate |> stringify)
       in
       Check.(
         (input_value = output_value)
           string
           ~__LOC__
           ~error_msg:"Expected %R, got %L")
  in
  (* Write output of first call to `config show` to disk, to pass it
     to second call below *)
  let in_file2 =
    Temp.file (string_of_int index ^ "-octez-client.input_config_file2.json")
  in
  JSON.encode_to_file in_file2 output_json1 ;
  (* Use previous ouput file as input now *)
  let* (stdout : string) = Client.config_show ~config_file:in_file2 client in
  let output_json2 = JSON.parse ~origin:"config show" stdout in
  Check.(
    (output_json1 = output_json2) json ~__LOC__ ~error_msg:"Expected %R, got %L") ;
  unit

(* Tests that calling `config show` works, with a valid web/node port *)
let test_config_validation () =
  Test.register
    ~__FILE__
    ~title:"Test the clients config validation"
    ~tags:["config"; "validation"]
    ~uses_node:false
  @@ fun () ->
  let valid =
    [
      `O [("node_port", `Float 8732.0)];
      `O [("node_port", `Float 58732.0)];
      `O [("web_port", `Float 8732.0)];
      `O [("web_port", `Float 58732.0)];
    ]
  in
  let invalid =
    [
      `O [("node_port", `Float 158732.0)];
      `O [("node_port", `Float (-8732.0))];
      `O [("web_port", `Float 158732.0)];
      `O [("web_port", `Float (-8732.0))];
    ]
  in
  let* client = Client.init () in
  let config_file config =
    let config = JSON.annotate ~origin:"config" config in
    write_config_file client config |> fst
  in
  let* () =
    iter valid @@ fun config ->
    let* (_ : string) =
      Client.config_show ~config_file:(config_file config) client
    in
    unit
  in
  let* () =
    iter invalid @@ fun config ->
    Client.spawn_config_show ~config_file:(config_file config) client
    |> Process.check_error
  in
  unit

let register_protocol_independent () =
  test_config_init () ;
  test_config_init_roundtrip () ;
  test_config_show () ;
  test_config_show_roundtrip () ;
  test_config_validation ()

let register ~protocols = additional_bootstrap_accounts protocols
