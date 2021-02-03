(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component: Client - proxy mode
   Invocation: dune exec tezt/tests/main.exe -- --file proxy.ml
   Subject: Tests of the client's --mode proxy.
  *)

(** Returns: a node and a proxy client *)
let init ~protocol () =
  let* node = Node.init [Synchronisation_threshold 0] in
  let* client = Client.init ~node () in
  let* () = Client.activate_protocol ~protocol client in
  Log.info "Activated protocol." ;
  Client.set_mode (Proxy node) client ;
  let* () = Client.bake_for client in
  Log.info "Baked 1 block." ;
  Lwt.return (node, client)

(** Test.
    Test that [tezos-client --mode proxy --protocol P] fails
    when the endpoint's protocol is not [P].
 *)
let test_wrong_proto protocol =
  Test.register
    ~__FILE__
    ~title:(Printf.sprintf "wrong proto (%s)" (Protocol.name protocol))
    ~tags:[Protocol.tag protocol; "proxy"; "bake"]
  @@ fun () ->
  let* node = Node.init [] in
  let* client = Client.init ~node () in
  let* () = Client.activate_protocol ~protocol client in
  Log.info "Activated protocol." ;
  let* () = Client.bake_for client in
  Log.info "Baked once: the protocol is now %s." (Protocol.name protocol) ;
  Client.set_mode (Proxy node) client ;
  let other_proto = List.find (( <> ) protocol) Protocol.all_protocols in
  let* stderr =
    Client.spawn_bake_for ~protocol:other_proto client
    |> Process.check_and_read_stderr ~expect_failure:true
  in
  let matches re s = try Re.Str.search_forward re s 0 >= 0 with _ -> false in
  let regexp =
    Re.Str.regexp
    @@ Format.sprintf
         "Protocol passed to the proxy (%s) and protocol of the node (%s) \
          differ"
         (Protocol.hash other_proto)
         (Protocol.hash protocol)
  in
  if matches regexp stderr then return ()
  else Test.fail "Did not fail as expected: %s" stderr

(** Test.
    Bake a few blocks in proxy mode.
 *)
let test_bake protocol =
  Test.register
    ~__FILE__
    ~title:(Printf.sprintf "bake (%s)" (Protocol.name protocol))
    ~tags:[Protocol.tag protocol; "proxy"; "bake"]
  @@ fun () ->
  let* node = Node.init [] in
  let* client = Client.init ~node () in
  let* () = Client.activate_protocol ~protocol client in
  Log.info "Activated protocol." ;
  Client.set_mode (Proxy node) client ;
  let* () = repeat 10 (fun () -> Client.bake_for client) in
  Log.info "Baked 10 blocks." ;
  let* level = Node.wait_for_level node 11 in
  Log.info "Level is now %d." level ;
  return ()

(** Test.
    Do some transfers and bakes the corresponding blocks in proxy mode.
 *)
let test_transfer protocol =
  Test.register
    ~__FILE__
    ~title:(Printf.sprintf "transfer (%s)" (Protocol.name protocol))
    ~tags:[Protocol.tag protocol; "proxy"; "transfer"]
  @@ fun () ->
  let* (_, client) = init ~protocol () in
  let* () =
    Client.transfer
      ~wait:"none"
      ~amount:Tez.(of_int 5)
      ~giver:"bootstrap1"
      ~receiver:"bootstrap2"
      client
  in
  Log.info "Transferred 5 tez." ;
  let* () = Client.bake_for client in
  Log.info "Baked block for bootstrap1." ;
  let* () =
    Client.transfer
      ~wait:"none"
      ~amount:Tez.(of_int 10)
      ~giver:"bootstrap2"
      ~receiver:"bootstrap3"
      client
  in
  Log.info "Transferred 10 tez." ;
  let* () = Client.bake_for ~key:"bootstrap2" client in
  Log.info "Baked block for bootstrap2." ;
  return ()

(** Module containing tests regarding where RPCs are executed: on
    the node or locally. *)
module Location = struct
  type rpc_exec_location =
    | Local  (** RPC executed locally (proxy mode) *)
    | Distant  (** RPC executed by the node (proxy mode) *)
    | Unknown  (** Client doesn't output location info (vanilla mode) *)

  let location_to_string = function
    | Local ->
        "Local"
    | Distant ->
        "Distant"
    | Unknown ->
        "Unknown"

  type clients = {vanilla : Client.t; alternative : Client.t}

  type alt_mode = Proxy (* | Light : later on *)

  let alt_mode_to_string = function Proxy -> "proxy"

  let chain_id = "main"

  let block_id = "head"

  (** [output] is the output of executing [rpc get rpc_path] *)
  let parse_rpc_exec_location ?query_string output rpc_path =
    let matches re s =
      try Re.Str.search_forward re s 0 >= 0 with _ -> false
    in
    let re prefix suffix =
      let re_str =
        Printf.sprintf
          "proxy_rpc_ctxt: %s [a-zA-Z_]+ [A-Z]+ %s %s"
          prefix
          ( Re.Str.quote
          @@ Client.rpc_path_query_to_string ?query_string rpc_path )
          suffix
      in
      Re.Str.regexp re_str
    in
    let re_local = re "Done" "locally" in
    let re_http = re "Delegating" "to http" in
    if matches re_local output then Local
    else if matches re_http output then Distant
    else Unknown

  (** Calls [rpc get] on the given [client] but specifies an alternative
      environment to make sure the location where the RPC executes is
      printed to output. *)
  let rpc_get ?query_string client rpc_path =
    let env = String_map.singleton "TEZOS_LOG" "proxy_rpc_ctxt->debug" in
    Client.spawn_rpc ~env ?query_string Client.GET rpc_path client
    |> Process.check_and_read_both

  (** Check that executing [rpc get rpc_path] on client causes the RPC
      to be executed on the given location ([expected_loc]) *)
  let check_location alt_mode client rpc_path expected_loc =
    let* (_, stderr) = rpc_get client rpc_path in
    let actual_loc = parse_rpc_exec_location stderr rpc_path in
    if actual_loc <> expected_loc then
      Test.fail
        "Expected %s client to execute %s on this location: %s. But found: %s."
        (alt_mode_to_string alt_mode)
        (Client.rpc_path_query_to_string rpc_path)
        (location_to_string expected_loc)
        (location_to_string actual_loc) ;
    Lwt.return_unit

  let check_locations alt_mode client =
    let paths_n_locations =
      [ ( ["chains"; chain_id; "blocks"; block_id; "context"; "delegates"],
          Local );
        (["chains"; chain_id; "blocks"], Distant);
        (["network"; "self"], Distant) ]
    in
    Lwt_list.iter_s
      (fun (rpc_path, expected_loc) ->
        check_location alt_mode client rpc_path expected_loc)
      paths_n_locations

  let locations_title protocol =
    Printf.sprintf "rpc get's location (%s)" (Protocol.name protocol)

  let locations_tags alt_mode protocol =
    [ Protocol.tag protocol;
      alt_mode_to_string alt_mode;
      "location";
      "rpc";
      "get" ]

  (** Test.
      Check the location where an RPC is executed by the proxy client. *)
  let test_locations_proxy protocol =
    let alt_mode = Proxy in
    Test.register
      ~__FILE__
      ~title:(locations_title protocol)
      ~tags:(locations_tags alt_mode protocol)
    @@ fun () ->
    let* (_, client) = init ~protocol () in
    check_locations alt_mode client

  (** Check the output of [rpc get] on a number on RPC between two
      clients are equivalent. One of them is a vanilla client ([--mode client]) while the
      other client uses an alternative mode ([--mode proxy]). *)
  let check_equivalence alt_mode {vanilla; alternative} =
    let alt_mode_string = alt_mode_to_string alt_mode in
    let compared =
      let add_rpc_path_prefix rpc_path =
        "chains" :: chain_id :: "blocks" :: block_id :: rpc_path
      in
      [ (add_rpc_path_prefix ["context"; "constants"], []);
        (add_rpc_path_prefix ["helpers"; "baking_rights"], []);
        (add_rpc_path_prefix ["helpers"; "baking_rights"], [("all", "true")]);
        (add_rpc_path_prefix ["helpers"; "current_level"], []);
        (add_rpc_path_prefix ["minimal_valid_time"], []);
        (add_rpc_path_prefix ["context"; "constants"], []);
        (add_rpc_path_prefix ["context"; "constants"; "errors"], []);
        (add_rpc_path_prefix ["context"; "delegates"], []);
        (add_rpc_path_prefix ["context"; "nonces"; "3"], []);
        (add_rpc_path_prefix ["helpers"; "endorsing_rights"], []);
        (add_rpc_path_prefix ["helpers"; "levels_in_current_cycle"], []);
        (add_rpc_path_prefix ["votes"; "ballot_list"], []);
        (add_rpc_path_prefix ["votes"; "ballots"], []);
        (add_rpc_path_prefix ["votes"; "current_period_kind"], []);
        (add_rpc_path_prefix ["votes"; "current_proposal"], []);
        (add_rpc_path_prefix ["votes"; "current_quorum"], []);
        (add_rpc_path_prefix ["votes"; "listings"], []);
        (add_rpc_path_prefix ["votes"; "proposals"], []) ]
    in
    let perform (rpc_path, query_string) =
      let* (vanilla_out, vanilla_err) = rpc_get ~query_string vanilla rpc_path
      and* (alt_out, alt_err) = rpc_get ~query_string alternative rpc_path in
      if vanilla_out <> alt_out then
        Test.fail
          "rpc get %s yields different results for the vanilla client and the \
           %s client. Output of vanilla client is:\n\
           %s\n\
           while output of the alternative client is:\n\
           %s\n"
          (Client.rpc_path_query_to_string ~query_string rpc_path)
          alt_mode_string
          vanilla_out
          alt_out
      else
        match
          ( parse_rpc_exec_location vanilla_err ~query_string rpc_path,
            parse_rpc_exec_location alt_err ~query_string rpc_path )
        with
        | (Unknown, Local) ->
            (* There should be no match in the vanilla output,
               because the vanilla client doesn't deal with alternative stuff.
               That is why [Unknown] is matched here. *)
            Log.info
              "%s client, %s: same answer than vanilla client ✓"
              alt_mode_string
              (Client.rpc_path_query_to_string ~query_string rpc_path) ;
            Log.info
              "%s client, %s: done locally ✓"
              alt_mode_string
              (Client.rpc_path_query_to_string ~query_string rpc_path) ;
            Lwt.return_unit
        | (loc, Local) ->
            Test.fail
              "Vanilla client should not output whether an RPC (here: %s) is \
               executed locally or delegated to the endpoint. Expected %s but \
               found %s. Inspected log:\n\
               %s\n"
              (Client.rpc_path_query_to_string ~query_string rpc_path)
              (location_to_string Unknown)
              (location_to_string loc)
              vanilla_err
        | (_, loc) ->
            Test.fail
              "%s client should execute RPC %s locally: expected %s but found \
               %s. Inspected log:\n\
               %s"
              alt_mode_string
              (Client.rpc_path_query_to_string ~query_string rpc_path)
              (location_to_string Distant)
              (location_to_string loc)
              alt_err
    in
    Lwt_list.iter_s perform compared

  let compare_title protocol =
    Printf.sprintf "compare rpc get (%s)" (Protocol.name protocol)

  let compare_tags alt_mode protocol =
    [Protocol.tag protocol; alt_mode_to_string alt_mode; "rpc"; "get"]

  (** Test.
      Check that executing a number of RPCs with a vanilla client and
      an alternative client yield the same results. *)
  let test_compare_proxy protocol =
    let alt_mode = Proxy in
    Test.register
      ~__FILE__
      ~title:(compare_title protocol)
      ~tags:(compare_tags alt_mode protocol)
    @@ fun () ->
    let* (node, alternative) = init ~protocol () in
    let* vanilla = Client.init ~node () in
    let clients = {vanilla; alternative} in
    check_equivalence alt_mode clients
end

let register protocol =
  test_bake protocol ;
  test_transfer protocol ;
  test_wrong_proto protocol ;
  Location.test_locations_proxy protocol ;
  Location.test_compare_proxy protocol
