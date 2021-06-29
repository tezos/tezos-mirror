(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Tocqueville Group, Inc. <marcin.pastudzki@tqtezos.com> *)
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

(** Testing
    -------
    Component:    RPC-HTTP
    Invocation:   dune build @src/lib_rpc_http/runtest
    Subject:      Basic unit tests for HTTP server running RPC services.

                  These tests concern themselves mainly with ACL feature
                  RPC HTTP server.
*)

module Arbitrary = struct
  open QCheck
  open RPC_server.Acl

  let ipv4 =
    let open QCheck in
    map ~rev:Ipaddr.V4.to_int32 Ipaddr.V4.of_int32 int32
    |> set_print Ipaddr.V4.to_string

  let ipv6 =
    let open QCheck in
    map ~rev:Ipaddr.V6.to_int64 Ipaddr.V6.of_int64 (pair int64 int64)
    |> set_print Ipaddr.V6.to_string

  let addr_port_id =
    let gen =
      let open Gen in
      let open P2p_point.Id in
      let* addr = map Ipaddr.V4.to_string @@ gen ipv4
      and* port = opt @@ gen Qcheck_helpers.uint16 in
      pure {addr; port; peer_id = None}
    in
    make gen ~print:P2p_point.Id.addr_port_id_to_string

  let meth_matcher : meth_matcher arbitrary =
    oneofl
      [Any; Exact `GET; Exact `PUT; Exact `POST; Exact `PATCH; Exact `DELETE]

  let chunk_matcher : chunk_matcher arbitrary =
    let of_string s = Literal s in
    let gen =
      let open Gen in
      oneof [char_range '0' '9'; char_range 'A' 'Z'; char_range 'a' 'z']
    in
    let chunk = make Gen.(string_size ~gen (1 -- 32)) in
    choose [always Wildcard; map of_string chunk]

  let path_matcher : path_matcher arbitrary =
    let cm = list_of_size Gen.(1 -- 5) chunk_matcher in
    choose
      [
        map (fun l -> FollowedByAnySuffix l) cm;
        map (fun l : path_matcher -> Exact l) cm;
      ]

  let matcher : matcher arbitrary =
    pair meth_matcher path_matcher
    |> map (fun (meth, path) -> {meth; path})
    |> set_print matcher_to_string

  let pp_matchers =
    let open Format in
    pp_print_list (fun ppf m -> Format.fprintf ppf "%s" (matcher_to_string m))

  let acl : t arbitrary =
    let m = list_of_size Gen.(1 -- 10) matcher in
    choose
      [
        map (fun m -> Deny_all {except = m}) m;
        map (fun m -> Allow_all {except = m}) m;
      ]
    |> set_print (function
           | Allow_all {except} ->
               Format.asprintf "Blacklist: [%a]" pp_matchers except
           | Deny_all {except} ->
               Format.asprintf "Whitelist: [%a]" pp_matchers except)

  let policy : policy arbitrary =
    let open Gen in
    let rec add_to_policy policy n =
      if n > 0 then
        let* acl = gen acl and* endpoint = gen addr_port_id in
        add_to_policy (put_policy (endpoint, acl) policy) (n - 1)
      else pure policy
    in
    let gen_policy =
      let* n = 1 -- 5 in
      add_to_policy empty_policy n
    in
    make gen_policy ~print:policy_to_string

  (* We test the property that if [searched_for] was found in some
     [policy], then it also must be found in [put_policy added_one
     policy]. Because chances of choosing at random an address that
     is already present in the [policy] are minuscule, we choose
     that separately and then decide randomly whether to add it to
     the random policy or not. *)
  type find_policy_setup = {
    policy : policy;
    searched_for : P2p_point.Id.addr_port_id;
    added_entry : P2p_point.Id.addr_port_id * t;
  }

  let find_policy_setup : find_policy_setup arbitrary =
    let open QCheck in
    let generate_entry =
      let open Gen in
      let* endpoint = gen addr_port_id and* acl = gen acl in
      pure (endpoint, acl)
    in
    let generate =
      let open Gen in
      let* p = gen policy
      and* (searched_for, searched_acl) = generate_entry
      and* added_entry = generate_entry in
      let* policy =
        oneofl [p; RPC_server.Acl.put_policy (searched_for, searched_acl) p]
      in
      pure {policy; searched_for; added_entry}
    in
    make generate
end

let example_policy =
  `A
    [
      `O
        [
          ("address", `String "localhost");
          ("whitelist", `A [`String "/chains/**"]);
        ];
      `O
        [
          ("address", `String "localhost:8732");
          ("blacklist", `A [`String "POST/**"; `String "PUT/**"]);
        ];
      `O
        [
          ("address", `String "192.168.0.3");
          ("blacklist", `A [`String "/monitor/**"]);
        ];
      `O
        [
          ("address", `String "192.168.1.5:8732");
          ( "whitelist",
            `A [`String "GET/**"; `String "DELETE/chains/*/invalid_blocks/*"] );
        ];
    ]
  |> Data_encoding.Json.destruct RPC_server.Acl.policy_encoding

let acl_testable =
  let pp_matchers fmt matchers =
    let open Format in
    pp_print_list
      ~pp_sep:(fun fmt () -> pp_print_string fmt ";@ ")
      (fun fmt m -> pp_print_string fmt (RPC_server.Acl.matcher_to_string m))
      fmt
      matchers ;
    pp_print_string fmt "]"
  in
  let pp fmt = function
    | RPC_server.Acl.Allow_all {except} ->
        Format.fprintf fmt "Blacklist:@ [" ;
        pp_matchers fmt except
    | RPC_server.Acl.Deny_all {except} ->
        Format.fprintf fmt "Whitelist:@ [" ;
        pp_matchers fmt except
  in
  Alcotest.testable pp @@ fun left right ->
  match (left, right) with
  | (Allow_all {except = l}, Allow_all {except = r})
  | (Deny_all {except = l}, Deny_all {except = r}) ->
      l = r
  | _ -> false

let pp_policy ppf policy =
  Format.fprintf ppf "%s" (RPC_server.Acl.policy_to_string policy)

let test_codec_identity =
  let open QCheck in
  Test.make
    ~name:"Encoding and decoding an ACL is an identity function."
    Arbitrary.policy
    (fun policy ->
      let json =
        Data_encoding.Json.construct RPC_server.Acl.policy_encoding policy
      in
      let decoded =
        Data_encoding.Json.destruct RPC_server.Acl.policy_encoding json
      in
      Qcheck_helpers.qcheck_eq ~pp:pp_policy policy decoded)

(* Assert that the result of searching [searched_for] in
   [policy] is never worse than the result of searching in
   [put_policy added_one policy], where we consider:
   - finding None worse than finding Some _ and
   - finding Some _ no worse than finding anything.

   Given results before_put and after_put, compare_results
   returns [true] if the comparison is satisfactory or [false]
   otherwise. *)
let check_find_policy =
  let open QCheck in
  let assert_results_satisfactory before_put after_put =
    match (before_put, after_put) with
    | (Some _, None) -> false
    | (_, _) -> true
  in
  Test.make
    ~name:"put_policy preserves existing entries."
    Arbitrary.find_policy_setup
    (fun {policy; searched_for; added_entry} ->
      let open RPC_server.Acl in
      let search_str = P2p_point.Id.addr_port_id_to_string searched_for in
      let before = find_policy_by_domain_name policy search_str in
      let after =
        find_policy_by_domain_name (put_policy added_entry policy) search_str
      in
      assert_results_satisfactory before after)

let mk_acl ((tag, matchers) : [`Whitelist | `Blacklist] * string list) =
  let open RPC_server.Acl in
  let except = List.map parse matchers in
  match tag with
  | `Whitelist -> Deny_all {except}
  | `Blacklist -> Allow_all {except}

let check_acl_search (description : string)
    (expected : ([`Whitelist | `Blacklist] * string list) option)
    (addr : string) =
  Alcotest.check
    (Alcotest.option acl_testable)
    description
    (Option.map mk_acl expected)
    (RPC_server.Acl.find_policy_by_domain_name example_policy addr)

let test_finding_policy =
  Alcotest.test_case "policy matching rules" `Quick (fun () ->
      check_acl_search
        "An exact match is when address and port match exactly."
        (Some (`Whitelist, ["GET/**"; "DELETE/chains/*/invalid_blocks/*"]))
        "192.168.1.5:8732" ;
      check_acl_search
        "When port is present in ACL and does not match given port, then it's \
         not a match."
        None
        "192.168.1.5:5431" ;
      check_acl_search
        "If policy omits a port, any port matches"
        (Some (`Blacklist, ["/monitor/**"]))
        "192.168.0.3:8732" ;
      check_acl_search
        "If policy omits a port, any port matches"
        (Some (`Blacklist, ["/monitor/**"]))
        "192.168.0.3:9732" ;
      check_acl_search
        "The first matching rule returns immediately"
        (Some (`Whitelist, ["/chains/**"]))
        "localhost:8732")

let ensure_default_policy_parses =
  let open QCheck in
  Test.make
    ~name:"default policy parses and is of correct type"
    Arbitrary.ipv6
    (fun ip_addr ->
      let expected =
        let open Ipaddr.V6 in
        if scope ip_addr = Interface then `Blacklist else `Whitelist
      in
      RPC_server.Acl.(acl_type (default ip_addr) = expected))

let ensure_unsafe_rpcs_blocked =
  let known_unsafe_rpcs =
    (* These are just examples. Do not rely on it being a complete list. *)
    [
      (`DELETE, ["chains"; "main"; "invalid_blocks"; "hash"]);
      ( `GET,
        [
          "fetch_protocol"; "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK";
        ] );
      (`GET, ["network"; "peers"]);
      (`GET, ["network"; "points"]);
      (`GET, ["stats"; "gc"]);
      (`GET, ["stats"; "memory"]);
      (`GET, ["workers"; "block_validator"]);
      (`GET, ["workers"; "chain_validators"]);
      (`GET, ["workers"; "prevalidators"; "main"]);
      (`PATCH, ["chains"; "main"]);
      (`POST, ["chains"; "main"; "mempool"; "filter"]);
      (`POST, ["chains"; "main"; "mempool"; "request_operations"]);
      (`POST, ["injection"; "block"]);
      (`POST, ["injection"; "protocol"]);
    ]
  in
  Alcotest.test_case
    "make sure the default policy blocks known particularly unsafe RPCs"
    `Quick
    (fun () ->
      List.iter
        (fun (meth, path) ->
          Alcotest.check'
            Alcotest.bool
            ~msg:
              (Format.sprintf
                 "%s /%s should be blocked by default!"
                 (Resto.string_of_meth meth)
                 (String.concat "/" path))
            ~expected:false
            ~actual:RPC_server.Acl.(allowed ~meth ~path secure))
        known_unsafe_rpcs)

let () =
  let open Qcheck_helpers in
  Alcotest.run
    "tezos-rpc-http"
    [
      ( "qcheck",
        qcheck_wrap
          [test_codec_identity; check_find_policy; ensure_default_policy_parses]
      );
      ("find_policy_matching_rules", [test_finding_policy]);
      ("ensure_unsafe_rpcs_blocked", [ensure_unsafe_rpcs_blocked]);
    ]
