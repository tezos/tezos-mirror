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

(** [matches re s] checks if [s] matches [re]. Note in particular that this supports multiline strings. *)
let matches re s = try Re.Str.search_forward re s 0 >= 0 with _ -> false

(** Returns: a node and a proxy client *)
let init ~protocol () =
  let* node = Node.init [Synchronisation_threshold 0] in
  let* client = Client.init ~node () in
  let* () = Client.activate_protocol ~protocol client in
  Log.info "Activated protocol." ;
  Client.set_mode (Proxy node) client ;
  let* () = Client.bake_for client in
  Log.info "Baked 1 block: protocol is now %s" (Protocol.name protocol) ;
  Lwt.return (node, client)

(** Test.
    This test checks that the proxy client creates its cache for
    RPC answers at most once for a given (chain, block) pair.
*)
let test_cache_at_most_once ?query_string path protocol =
  Test.register
    ~__FILE__
    ~title:
      (sf
         "(%s) (Proxy) (%s) Cache at most once"
         (Protocol.name protocol)
         (Client.rpc_path_query_to_string ?query_string path))
    ~tags:[Protocol.tag protocol; "proxy"; "rpc"; "get"]
  @@ fun () ->
  let* (_, client) = init ~protocol () in
  let env =
    [("TEZOS_LOG", Protocol.daemon_name protocol ^ ".proxy_rpc->debug")]
    |> List.to_seq |> String_map.of_seq
  in
  let* stderr =
    Client.spawn_rpc ~env ?query_string Client.GET path client
    |> Process.check_and_read_stderr
  in
  let lines = String.split_on_char '\n' stderr in
  let proxy_cache_regexp =
    Re.Str.regexp
      {|^.*proxy_rpc: proxy cache created for chain \([a-zA-Z0-9]*\) and block \([a-zA-Z0-9]*\)|}
  in
  let extract_chain_block line =
    (* Groups are 1-based (0 is for the whole match). *)
    if Re.Str.string_match proxy_cache_regexp line 0 then
      Some (Re.Str.matched_group 1 line, Re.Str.matched_group 2 line)
    else None
  in
  let chain_block_list = lines |> List.filter_map extract_chain_block in
  let find_duplicate l =
    let rec go with_duplicates without_duplicates =
      match (with_duplicates, without_duplicates) with
      | ([], []) ->
          None
      | (hd_dup :: tl_dup, hd_nodup :: tl_nodup) ->
          if hd_dup = hd_nodup then go tl_dup tl_nodup else Some hd_dup
      | _ ->
          assert false
    in
    go (List.sort Stdlib.compare l) (List.sort_uniq Stdlib.compare l)
  in
  if chain_block_list = [] then
    Test.fail
      "Proxy cache should have been created when executing %s"
      (String.concat "/" path) ;
  find_duplicate chain_block_list
  |> Option.iter (fun (chain, block) ->
         Test.fail
           "proxy RPC cache for chain %s and block %s created more than once"
           chain
           block)
  |> Lwt.return

let test_cache_at_most_once protocol =
  let paths =
    [ (["context"; "constants"], []);
      (["helpers"; "baking_rights"], []);
      (["helpers"; "baking_rights"], [("all", "true")]);
      (["helpers"; "current_level"], []);
      (["minimal_valid_time"], []);
      (["context"; "constants"], []);
      (["context"; "constants"; "errors"], []);
      (["context"; "delegates"], []);
      (["context"; "nonces"; "3"], []);
      (["helpers"; "endorsing_rights"], []);
      (["helpers"; "levels_in_current_cycle"], []);
      (["votes"; "ballot_list"], []);
      (["votes"; "ballots"], []);
      (["votes"; "current_proposal"], []);
      (["votes"; "current_quorum"], []);
      (["votes"; "listings"], []);
      (["votes"; "proposals"], []) ]
  in
  List.iter
    (fun (sub_path, query_string) ->
      test_cache_at_most_once
        ~query_string
        ("chains" :: "main" :: "blocks" :: "head" :: sub_path)
        protocol)
    paths

(** [starts_with prefix s] returns [true] iff [prefix] is a prefix of [s]. *)
let starts_with ~(prefix : string) (s : string) : bool =
  Re.Str.string_match (Re.Str.regexp ("^" ^ prefix)) s 0

(** Test.
    This test checks that the proxy client never does a useless RPC.

    I.e. it checks that if the proxy client requested
    [/chains/<main>/blocks/<head>/context/raw/bytes/some_path]
    it doesn't later request
    [/chains/<main>/blocks/<head>/context/raw/bytes/some_path/some_other_path]

    In this scenario, the proxy client should look directly in the data within the tree received by the first request.

    For this, this test inspects the debug output produced by
    setting TEZOS_LOG to alphas.proxy_rpc->debug. This causes the client
    to print the RPCs done to get pieces of the context:

    alpha.proxy_rpc: P/v1/constants
    alpha.proxy_rpc: Received tree of size 1
    alpha.proxy_rpc: P/v1/first_level
    alpha.proxy_rpc: Received tree of size 1
    alpha.proxy_rpc: P/cycle/0/random_seed
    alpha.proxy_rpc: Received tree of size 1
    alpha.proxy_rpc: P/cycle/0/roll_snapshot
    alpha.proxy_rpc: Received tree of size 1
    alpha.proxy_rpc: P/cycle/0/last_roll/0

    where [P] is [/chains/<main>/blocks/<head>/context/raw/bytes]
 *)
let test_context_suffix_no_rpc ?query_string path protocol =
  Test.register
    ~__FILE__
    ~title:
      (sf
         "(%s) (Proxy) (%s) No useless RPC call"
         (Protocol.name protocol)
         (Client.rpc_path_query_to_string ?query_string path))
    ~tags:[Protocol.tag protocol; "proxy"; "rpc"; "get"]
  @@ fun () ->
  let* (_, client) = init ~protocol () in
  let env =
    [("TEZOS_LOG", Protocol.daemon_name protocol ^ ".proxy_rpc->debug")]
    |> List.to_seq |> String_map.of_seq
  in
  let* stderr =
    Client.spawn_rpc ~env ?query_string Client.GET path client
    |> Process.check_and_read_stderr
  in
  let lines = String.split_on_char '\n' stderr in
  let rpc_path_regexp =
    Re.Str.regexp
      {|.*proxy_rpc: /chains/<main>/blocks/<head>/context/raw/bytes/\(.*\)|}
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
                "Query %s should not be followed by query %s because the \
                 latter is a suffix of the former. Hence the proxy should \
                 reuse the data of the first query."
                query_before
                query_after
            else ())
          queries_before ;
        test_no_overlap_rpc queries_before
  in
  assert (List.length context_queries >= 2) ;
  Lwt.return @@ test_no_overlap_rpc (List.rev context_queries)

let test_context_suffix_no_rpc protocol =
  let paths =
    ( match protocol with
    | Protocol.Alpha ->
        []
    | _ ->
        [(["votes"; "current_period_kind"], [])] )
    @ [ (["helpers"; "baking_rights"], []);
        (["helpers"; "baking_rights"], [("all", "true")]);
        (["context"; "delegates"], []);
        (["context"; "nonces"; "3"], []);
        (["helpers"; "endorsing_rights"], []);
        (["votes"; "ballot_list"], []);
        (["votes"; "ballots"], []);
        (["votes"; "current_proposal"], []);
        (["votes"; "current_quorum"], []);
        (["votes"; "listings"], []);
        (["votes"; "proposals"], []) ]
  in
  List.iter
    (fun (sub_path, query_string) ->
      test_context_suffix_no_rpc
        ~query_string
        ("chains" :: "main" :: "blocks" :: "head" :: sub_path)
        protocol)
    paths

(** Test.
    Test that [tezos-client --mode proxy --protocol P] fails
    when the endpoint's protocol is not [P].
 *)
let test_wrong_proto protocol =
  Test.register
    ~__FILE__
    ~title:(sf "(%s) (Proxy) Wrong proto" (Protocol.name protocol))
    ~tags:[Protocol.tag protocol; "proxy"; "bake"]
  @@ fun () ->
  let* (_, client) = init ~protocol () in
  let other_proto =
    match List.find_opt (( <> ) protocol) Protocol.all with
    | None ->
        Test.fail
          "No other protocol than %s is available."
          (Protocol.name protocol)
    | Some other_proto ->
        other_proto
  in
  let* stderr =
    Client.spawn_bake_for ~protocol:other_proto client
    |> Process.check_and_read_stderr ~expect_failure:true
  in
  let regexp =
    Re.Str.regexp
    @@ Format.sprintf
         ".*Protocol passed to the proxy (%s) and protocol of the node (%s) \
          differ."
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
    ~title:(sf "(%s) (Proxy) Bake" (Protocol.name protocol))
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
    ~title:(sf "(%s) (Proxy) Transfer" (Protocol.name protocol))
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

  let log_line_prefix =
    Re.Str.regexp "[A-Z][a-z]+[ 0-9:\\.]+ - proxy_rpc_ctxt: +"

  (** [output] is the output of executing [rpc get rpc_path] *)
  let parse_rpc_exec_location ?query_string output rpc_path =
    let log = Re.Str.global_replace log_line_prefix "" output in
    let re prefix =
      let re_str =
        Printf.sprintf
          "%s[ a-zA-Z]*: [A-Z]+\\(\n\\| \\)%s"
          prefix
          ( Re.Str.quote
          @@ Client.rpc_path_query_to_string ?query_string rpc_path )
      in
      Re.Str.regexp re_str
    in
    let re_local = re "locally done" in
    let re_http = re "delegating to http" in
    if matches re_local log then Local
    else if matches re_http log then Distant
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
      ~title:(sf "(%s) (Proxy) RPC get's location" (Protocol.name protocol))
      ~tags:(locations_tags alt_mode protocol)
    @@ fun () ->
    let* (_, client) = init ~protocol () in
    check_locations alt_mode client

  (** Check the output of [rpc get] on a number on RPC between two
      clients are equivalent. One of them is a vanilla client ([--mode client]) while the
      other client uses an alternative mode ([--mode proxy]). *)
  let check_equivalence protocol alt_mode {vanilla; alternative} =
    let alt_mode_string = alt_mode_to_string alt_mode in
    let compared =
      let add_rpc_path_prefix rpc_path =
        "chains" :: chain_id :: "blocks" :: block_id :: rpc_path
      in
      ( match protocol with
      | Protocol.Alpha ->
          []
      | _ ->
          [(add_rpc_path_prefix ["votes"; "current_period_kind"], [])] )
      @ [ (add_rpc_path_prefix ["context"; "constants"], []);
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

  let compare_tags alt_mode protocol =
    [Protocol.tag protocol; alt_mode_to_string alt_mode; "rpc"; "get"]

  (** Test.
      Check that executing a number of RPCs with a vanilla client and
      an alternative client yield the same results. *)
  let test_compare_proxy protocol =
    let alt_mode = Proxy in
    Test.register
      ~__FILE__
      ~title:(sf "(%s) (Proxy) Compare RPC get" (Protocol.name protocol))
      ~tags:(compare_tags alt_mode protocol)
    @@ fun () ->
    let* (node, alternative) = init ~protocol () in
    let* vanilla = Client.init ~node () in
    let clients = {vanilla; alternative} in
    check_equivalence protocol alt_mode clients
end

let register protocol =
  test_bake protocol ;
  test_transfer protocol ;
  test_wrong_proto protocol ;
  test_context_suffix_no_rpc protocol ;
  test_cache_at_most_once protocol ;
  Location.test_locations_proxy protocol ;
  Location.test_compare_proxy protocol
