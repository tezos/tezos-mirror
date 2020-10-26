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
 *)

open Lwt.Infix

let init_light ~protocol =
  let get_current_level =
    match protocol with
    | Protocol.Alpha ->
        RPC.get_current_level
    | _ ->
        Test.fail "Unsupported protocol: %s" @@ Protocol.name protocol
  in
  (* Note that this code CANNOT be in tezt/lib_tezos/client.ml
     because it uses RPC.*.get_current_level, which depends on client.ml
     already. In other words, putting this code in client.ml would
     create a cyclic dependency *)
  let* (client, nodes) = Client.init_light () in
  let node0 = List.nth nodes 0 in
  Log.info "Activating protocol %s" @@ Protocol.tag protocol ;
  let* () = Client.activate_protocol ~node:node0 ~protocol client in
  (* Set client temporarily to vanilla mode: needed when baking,
     because nodes are temporarily out-of-sync for "HEAD" which the light
     mode doesn't like *)
  let mode_received = Client.get_mode client in
  let is_light_mode = function Client.Light _ -> true | _ -> false in
  assert (is_light_mode mode_received) ;
  Client.set_mode (Client.Client (Some node0)) client ;
  let* () =
    let bakers =
      match protocol with
      | Protocol.Alpha | Florence ->
          [Constant.bootstrap1.identity; Constant.bootstrap1.identity]
      | Edo ->
          [Constant.bootstrap1.alias; Constant.bootstrap1.alias]
    in
    Lwt_list.iter_s (fun key -> Client.bake_for ~node:node0 ~key client) bakers
  in
  let* level_json = get_current_level ~node:node0 client in
  let level = JSON.(level_json |-> "level" |> as_int) in
  let* () =
    Lwt_list.iter_s (fun node ->
        Log.info "Waiting for node %s to be at level %d" (Node.name node) level ;
        Node.wait_for_level node level >>= fun _ -> Lwt.return_unit)
    @@ List.tl nodes
  in
  Log.info "All nodes are at level %d" level ;
  (* Set mode again: back to light mode *)
  Client.set_mode mode_received client ;
  assert (Client.get_mode client |> is_light_mode) ;
  return client

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
  let* client = init_light ~protocol in
  do_transfer client

let test_bake =
  Protocol.register_test
    ~__FILE__
    ~title:"(Light) bake"
    ~tags:["light"; "client"; "bake"]
  @@ fun protocol ->
  let* client = init_light ~protocol in
  let giver =
    match protocol with
    | Protocol.Edo ->
        Constant.bootstrap1.alias
    | Florence | Alpha ->
        Constant.bootstrap1.identity
  in
  let* () = do_transfer ~giver client in
  Client.bake_for ~key:giver client

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
      | [] ->
          ()
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
    assert (List.length context_queries >= 2) ;
    Lwt.return @@ test_no_overlap_rpc (List.rev context_queries)

  (** Test.
      See [test_no_useless_rpc] *)
  let test =
    Protocol.register_test
      ~__FILE__
      ~title:"(Light) No useless RPC call"
      ~tags:["light"; "rpc"; "get"]
    @@ fun protocol ->
    let* client = init_light ~protocol in
    let paths =
      [ (["helpers"; "baking_rights"], []);
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
        (["votes"; "proposals"], []) ]
    in
    Lwt_list.iter_s
      (fun (sub_path, query_string) ->
        test_no_useless_rpc
          ~query_string
          ("chains" :: "main" :: "blocks" :: "head" :: sub_path)
          client)
      paths
end

let register ~protocols =
  test_transfer ~protocols ;
  test_bake ~protocols ;
  NoUselessRpc.test ~protocols
