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
   Component: Client - light mode
   Invokation: dune exec tezt/tests/main.exe -- --file light.ml
   Subject: Tests of the client's --mode light option
   Dependencies: tezt/tests/proxy.ml
*)

let init_light ~protocol =
  (* Note that this code CANNOT be in tezt/lib_tezos/client.ml
     because it uses RPC.*.get_current_level, which depends on client.ml
     already. In other words, putting this code in client.ml would
     create a cyclic dependency *)
  let* client, node0, node1 = Client.init_light () in
  Log.info "Activating protocol %s" @@ Protocol.tag protocol ;
  let endpoint = Client.(Node node0) in
  let* () = Client.activate_protocol ~endpoint ~protocol client in
  (* Set client temporarily to vanilla mode: needed when baking,
     because nodes are temporarily out-of-sync for "HEAD" which the light
     mode doesn't like *)
  let mode_received = Client.get_mode client in
  let is_light_mode = function Client.Light _ -> true | _ -> false in
  assert (is_light_mode mode_received) ;
  Client.set_mode (Client.Client (Some (Node node0), None)) client ;
  let* () =
    Client.bake_for_and_wait ~endpoint ~keys:[Constant.bootstrap1.alias] client
  in
  let* () =
    Client.bake_for_and_wait ~endpoint ~keys:[Constant.bootstrap2.alias] client
  in
  let level = Node.get_level node0 in
  let () =
    Log.info "Waiting for node %s to be at level %d" (Node.name node1) level
  in
  let* _ = Node.wait_for_level node1 level in
  Log.info "All nodes are at level %d" level ;
  (* Set mode again: back to light mode *)
  Client.set_mode mode_received client ;
  assert (Client.get_mode client |> is_light_mode) ;
  return (node0, client)

let test_no_endpoint () =
  Test.register
    ~__FILE__
    ~title:"mode light no endpoint"
    ~tags:["client"; "light"; "cli"]
  @@ fun () ->
  let min_agreement = 1.0 in
  let uris = List.map (fun port -> sf "http://localhost:%d" port) [666; 667] in
  let endpoints =
    (* As the client should fail before contacting the node, we don't need
       to start a node in this test. Hence we pass an empty list of endpoints
       when creating the client below. *)
    []
  in
  let client = Client.create_with_mode (Light (min_agreement, endpoints)) in
  let* () = Client.write_sources_file ~min_agreement ~uris client in
  let*? process =
    RPC.Client.spawn client @@ RPC.get_chain_block_context_contracts ()
  in
  let* stderr = Process.check_and_read_stderr ~expect_failure:true process in
  let regexp =
    rex "Value of --endpoint is .*. If you did not specify --endpoint, .*"
  in
  Check.((stderr =~ regexp) ~error_msg:"expected value =~ %R, got %L") ;
  unit

let test_endpoint_not_in_sources () =
  Test.register
    ~__FILE__
    ~title:"mode light endpoint not in sources"
    ~tags:["client"; "light"; "cli"]
  @@ fun () ->
  let min_agreement = 1.0 in
  let mk_node_endpoint rpc_port = Client.Node (Node.create ~rpc_port []) in
  (* The mismatch is that the port of [endpoint] is not in [sources].
   * We use the port to disambiguate, because disambiguating
   * with the host is complicated, because of Client.address
   * that delegates to Runner.address; which, to make it short,
   * defaults the host to "localhost". *)
  let endpoint = mk_node_endpoint 666 in
  let sources_ports = [667; 668] in
  let endpoints =
    (* Endpoints stored in the client's mode, used by Client *)
    List.map mk_node_endpoint sources_ports
  in
  let uris =
    (* URIs written to sources.json *)
    List.map (fun port -> sf "http://localhost:%d" port) sources_ports
  in
  let client = Client.create_with_mode (Light (min_agreement, endpoints)) in
  let* () = Client.write_sources_file ~min_agreement ~uris client in
  let*? process =
    RPC.Client.spawn ~endpoint client
    @@ RPC.get_chain_block_context_contracts ()
  in
  let* stderr = Process.check_and_read_stderr ~expect_failure:true process in
  let regexp =
    rex "Value of --endpoint is .*. If you did not specify --endpoint, .*"
  in
  Check.((stderr =~ regexp) ~error_msg:"expected value =~ %R, got %L") ;
  unit

let do_transfer ?(amount = Tez.one) ?(giver = Constant.bootstrap1.alias)
    ?(receiver = Constant.bootstrap2.alias) client =
  Log.info "Transfer %s from %s to %s" (Tez.to_string amount) giver receiver ;
  Client.transfer ~wait:"none" ~amount ~giver ~receiver client

let test_transfer =
  Protocol.register_test
    ~__FILE__
    ~title:"(Light) transfer"
    ~tags:["light"; "client"; "transfer"]
  @@ fun protocol ->
  let* _, client = init_light ~protocol in
  do_transfer client

let test_bake =
  Protocol.register_test
    ~__FILE__
    ~title:"(Light) bake"
    ~tags:["light"; "client"; "bake"]
  @@ fun protocol ->
  let* _, client = init_light ~protocol in
  let giver = Constant.bootstrap1.alias in
  let* () = do_transfer ~giver client in
  Client.bake_for_and_wait ~keys:[giver] client

module NoUselessRpc = struct
  (** [starts_with prefix s] returns [true] iff [prefix] is a prefix of [s]. *)
  let starts_with ~(prefix : string) (s : string) : bool =
    Re.Str.string_match (Re.Str.regexp ("^" ^ prefix)) s 0

  (** This test checks that the light client never does a useless RPC.

    I.e. it checks that if the light client requested
    [/chains/<main>/blocks/<head>/context/merkle_tree/some_path]
    it doesn't later request
    [/chains/<main>/blocks/<head>/context/merkle_tree/some_path/some_other_path]

    In this scenario, the light client should look directly in the data within the tree received by the first request.

    For this, this test inspects the debug output produced by
    setting TEZOS_LOG to light_mode->debug. This causes the client
    to print the RPCs done to retrieve pieces of the context (do_rpc lines):

    light_mode: API call: do_rpc v1
    light_mode: API call: get v1;constants
    light_mode: API call: get v1;first_level
    light_mode: API call: do_rpc pending_migration_balance_updates
 *)
  let test_no_useless_rpc ?query_string path client =
    (* This test's implementation is similar to [Proxy.test_context_suffix_no_rpc]*)
    let env = String_map.singleton "TEZOS_LOG" "light_mode->debug" in
    let* stderr =
      Client.spawn_rpc ~env ?query_string Client.GET path client
      |> Process.check_and_read_stderr
    in
    let lines = String.split_on_char '\n' stderr in
    let rpc_path_regexp =
      Re.Str.regexp {|.*light_mode: API call: do_rpc \(.*\)|}
    in
    let extract_rpc_path line =
      (* Groups are 1-based (0 is for the whole match). *)
      if Re.Str.string_match rpc_path_regexp line 0 then
        Some (Re.Str.matched_group 1 line)
      else None
    in
    let context_queries = lines |> List.filter_map extract_rpc_path in
    let rec test_no_overlap_rpc = function
      | [] -> ()
      | query_after :: queries_before ->
          List.iter
            (fun query_before ->
              if starts_with ~prefix:query_before query_after then
                Test.fail
                  {|Query %s should not be followed by query %s because the
                 latter is a suffix of the former. Hence the light mode
                 should reuse the data of the first query.|}
                  query_before
                  query_after
              else ())
            queries_before ;
          test_no_overlap_rpc queries_before
    in
    assert (List.compare_length_with context_queries 2 >= 0) ;
    Lwt.return @@ test_no_overlap_rpc (List.rev context_queries)

  (** Test.
      See [test_no_useless_rpc] *)
  let test =
    Protocol.register_test
      ~__FILE__
      ~title:"(Light) No useless RPC call"
      ~tags:["light"; "rpc"; "get"]
    @@ fun protocol ->
    let* _, client = init_light ~protocol in
    let paths =
      [
        (["helpers"; "baking_rights"], []);
        (["helpers"; "baking_rights"], [("all", "true")]);
        (["context"; "delegates"], []);
        (["context"; "nonces"; "3"], []);
        (["helpers"; "endorsing_rights"], []);
        (["votes"; "ballot_list"], []);
        (["votes"; "ballots"], []);
        (["votes"; "current_period"], []);
        (["votes"; "current_proposal"], []);
        (["votes"; "current_quorum"], []);
        (["votes"; "listings"], []);
        (["votes"; "proposals"], []);
      ]
    in
    let paths =
      if Protocol.(number protocol > number Mumbai) then
        (["helpers"; "attestation_rights"], []) :: paths
      else paths
    in
    Lwt_list.iter_s
      (fun (sub_path, query_string) ->
        test_no_useless_rpc
          ~query_string
          ("chains" :: "main" :: "blocks" :: "head" :: sub_path)
          client)
      paths
end

(** Test.
    Test that [octez-client --mode light --sources ... --protocol P] fails
    when the endpoint's protocol is not [P].
 *)
let test_wrong_proto =
  Protocol.register_test
    ~__FILE__
    ~title:"(Light) Wrong proto"
    ~tags:["light"; "proto"]
  @@ fun protocol ->
  let* _, client = init_light ~protocol in
  Proxy.wrong_proto protocol client

let test_locations =
  let open Proxy.Location in
  let alt_mode = Light in
  Protocol.register_test
    ~__FILE__
    ~title:"(Light) RPC get's location"
    ~tags:(locations_tags alt_mode)
  @@ fun protocol ->
  let* _, client = init_light ~protocol in
  check_locations alt_mode client

let test_compare_light =
  let open Proxy.Location in
  let alt_mode = Light in
  Protocol.register_test
    ~__FILE__
    ~title:"(Light) Compare RPC get"
    ~tags:(compare_tags alt_mode)
  @@ fun protocol ->
  let* node, light_client = init_light ~protocol in
  let* vanilla = Client.init ~endpoint:(Node node) () in
  let clients = {vanilla; alternative = light_client} in
  let tz_log =
    [("proxy_rpc", "debug"); ("light_mode", "debug"); ("proxy_getter", "debug")]
  in
  check_equivalence ~tz_log alt_mode clients

let register_protocol_independent () =
  test_no_endpoint () ;
  test_endpoint_not_in_sources () ;
  Proxy.test_supported_protocols_like_mockup `Light

let register ~protocols =
  test_transfer protocols ;
  test_bake protocols ;
  NoUselessRpc.test protocols ;
  test_wrong_proto protocols ;
  test_locations protocols ;
  test_compare_light protocols
