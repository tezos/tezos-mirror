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
    Invocation:   dune exec src/lib_rpc_http/test/main.exe \
                  -- --file test_rpc_http.ml
    Subject:      Basic unit tests for HTTP server running RPC services.

                  These tests concern themselves mainly with ACL feature
                  RPC HTTP server.
*)

module Generator = struct
  open QCheck2
  open RPC_server.Acl
  open Tz_gen

  let meth_matcher : meth_matcher Gen.t =
    Gen.oneofl
      [Any; Exact `GET; Exact `PUT; Exact `POST; Exact `PATCH; Exact `DELETE]

  let chunk_matcher : chunk_matcher Gen.t =
    let open Gen in
    let of_string s = Literal s in
    let gen =
      oneof [char_range '0' '9'; char_range 'A' 'Z'; char_range 'a' 'z']
    in
    let chunk = string_size ~gen (1 -- 32) in
    oneof [return Wildcard; map of_string chunk]

  let path_matcher : path_matcher Gen.t =
    let open Gen in
    let cm = list_size (1 -- 5) chunk_matcher in
    oneof
      [
        map (fun l -> FollowedByAnySuffix l) cm;
        map (fun l : path_matcher -> Exact l) cm;
      ]

  let matcher : matcher Gen.t =
    let open Gen in
    pair meth_matcher path_matcher |> map (fun (meth, path) -> {meth; path})

  let pp_matchers =
    let open Format in
    pp_print_list (fun ppf m -> Format.fprintf ppf "%s" (matcher_to_string m))

  let acl : t Gen.t =
    let open Gen in
    let m = list_size (1 -- 10) matcher in
    oneof
      [
        map (fun m -> Deny_all {except = m}) m;
        map (fun m -> Allow_all {except = m}) m;
      ]

  let acl_to_string = function
    | Allow_all {except} -> Format.asprintf "Blacklist: [%a]" pp_matchers except
    | Deny_all {except} -> Format.asprintf "Whitelist: [%a]" pp_matchers except

  let policy : policy Gen.t =
    let open Gen in
    let rec add_to_policy policy n =
      if n > 0 then
        let* acl and* endpoint = addr_port_id in
        add_to_policy (put_policy (endpoint, acl) policy) (n - 1)
      else pure policy
    in
    let* n = 1 -- 5 in
    add_to_policy empty_policy n

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

  let find_policy_setup_to_string {policy; searched_for; added_entry} =
    let endpoint, acl = added_entry in
    Format.asprintf
      "{\n%s\n%s\n(%s, %s)\n}"
      (RPC_server.Acl.policy_to_string policy)
      (P2p_point.Id.addr_port_id_to_string searched_for)
      (P2p_point.Id.addr_port_id_to_string endpoint)
      (acl_to_string acl)

  let find_policy_setup : find_policy_setup Gen.t =
    let open Gen in
    let generate_entry =
      let* endpoint = addr_port_id and* acl in
      pure (endpoint, acl)
    in
    let* p = policy
    and* searched_for, searched_acl = generate_entry
    and* added_entry = generate_entry in
    let* policy =
      oneofl [p; RPC_server.Acl.put_policy (searched_for, searched_acl) p]
    in
    pure {policy; searched_for; added_entry}

  let gen_string_with_nl =
    let open Gen in
    let nl = pure '\n' in
    let char = frequency [(5, char); (1, nl)] in
    string_small_of char

  let gen_value =
    let open Gen in
    oneof
      [
        map (fun i -> `Int i) small_int;
        map (fun i -> `Z (Z.of_int i)) int;
        map (fun s -> `String s) gen_string_with_nl;
      ]
end

let resolve_domain_name =
  let resolver =
    String.Map.of_seq
    @@ List.to_seq
         [
           ( "localhost",
             List.map Ipaddr.V6.of_int64 [(0L, 1L); (0L, 281472812449793L)] );
           ("127.0.0.1", List.map Ipaddr.V6.of_int64 [(0L, 281472812449793L)]);
         ]
  in
  fun addr -> String.Map.find_opt addr resolver |> Option.value ~default:[]

let resolve_domain_names_in_policy =
  RPC_server.Acl.Internal_for_test.resolve_domain_names (fun {addr; port; _} ->
      resolve_domain_name addr
      |> List.map (fun addr -> (addr, port))
      |> Lwt.return)

let example_policy =
  `A
    [
      `O [("address", `String "localhost:22"); ("blacklist", `A [])];
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
  | Allow_all {except = l}, Allow_all {except = r}
  | Deny_all {except = l}, Deny_all {except = r} ->
      l = r
  | _ -> false

let pp_policy ppf policy =
  Format.fprintf ppf "%s" (RPC_server.Acl.policy_to_string policy)

let test_codec_identity =
  let open QCheck2 in
  Test.make
    ~name:"Encoding and decoding an ACL is an identity function."
    ~print:RPC_server.Acl.policy_to_string
    Generator.policy
    (fun policy ->
      let json =
        Data_encoding.Json.construct RPC_server.Acl.policy_encoding policy
      in
      let decoded =
        Data_encoding.Json.destruct RPC_server.Acl.policy_encoding json
      in
      Qcheck2_helpers.qcheck_eq ~pp:pp_policy policy decoded)

(* Assert that the result of searching [searched_for] in
   [policy] is never worse than the result of searching in
   [put_policy added_one policy], where we consider:
   - finding None worse than finding Some _ and
   - finding Some _ no worse than finding anything.

   Given results before_put and after_put, compare_results
   returns [true] if the comparison is satisfactory or [false]
   otherwise. *)
let check_find_policy =
  let open QCheck2 in
  let assert_results_satisfactory before_put after_put =
    match (before_put, after_put) with Some _, None -> false | _, _ -> true
  in
  Test.make
    ~name:"put_policy preserves existing entries."
    ~print:Generator.find_policy_setup_to_string
    Generator.find_policy_setup
    (fun {policy; searched_for = {addr; port; _}; added_entry} ->
      let open RPC_server.Acl in
      let before = find_policy policy (addr, port) in
      let after = find_policy (put_policy added_entry policy) (addr, port) in
      assert_results_satisfactory before after)

let mk_acl ((tag, matchers) : [`Whitelist | `Blacklist] * string list) =
  let open RPC_server.Acl in
  let except = List.map parse matchers in
  match tag with
  | `Whitelist -> Deny_all {except}
  | `Blacklist -> Allow_all {except}

let check_acl_search (description : string) (policy : RPC_server.Acl.policy)
    (expected : ([`Whitelist | `Blacklist] * string list) option)
    (addr : string * int option) =
  Alcotest.check
    (Alcotest.option acl_testable)
    description
    (Option.map mk_acl expected)
    (RPC_server.Acl.find_policy policy addr)

let test_finding_policy =
  Alcotest_lwt.test_case_sync "policy matching rules" `Quick (fun () ->
      check_acl_search
        "An exact match is when address and port match exactly."
        example_policy
        (Some (`Whitelist, ["GET/**"; "DELETE/chains/*/invalid_blocks/*"]))
        ("192.168.1.5", Some 8732) ;
      check_acl_search
        "When port is present in ACL and does not match given port, then it's \
         not a match."
        example_policy
        None
        ("192.168.1.5", Some 5431) ;
      check_acl_search
        "If policy omits a port, any port matches"
        example_policy
        (Some (`Blacklist, ["/monitor/**"]))
        ("192.168.0.3", Some 8732) ;
      check_acl_search
        "If policy omits a port, any port matches"
        example_policy
        (Some (`Blacklist, ["/monitor/**"]))
        ("192.168.0.3", Some 9732) ;
      check_acl_search
        "The first matching rule returns immediately"
        example_policy
        (Some (`Whitelist, ["/chains/**"]))
        ("localhost", Some 8732))

let ensure_default_policy_parses =
  let open QCheck2 in
  Test.make
    ~name:"default policy parses and is of correct type"
    ~print:Ipaddr.V6.to_string
    Tz_gen.ipv6
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
  Alcotest_lwt.test_case_sync
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

let test_matching_with_name_resolving =
  let to_test =
    [
      ("::1", Some 22, Some (`Blacklist, []));
      ("::1", Some 8732, Some (`Whitelist, ["/chains/**"]));
    ]
  in
  Alcotest_lwt.test_case_sync
    "make sure addresses match well with domain name resolving"
    `Quick
    (fun () ->
      Lwt_main.run
        (let open Lwt_syntax in
         let* policy = resolve_domain_names_in_policy example_policy in
         List.iter
           (fun (ip_addr, port, expected) ->
             check_acl_search
               "a domain name should match an appropriate IP address"
               policy
               expected
               (ip_addr, port))
           to_test ;
         return_unit))

let test_media_type_pp_parse =
  let open Tezos_rpc_http.Media_type.Command_line in
  let inputs = [Any; Json; Binary] in
  let to_string = function
    | Any -> "Any"
    | Json -> "Json"
    | Binary -> "Binary"
  in
  Alcotest_lwt.test_case_sync
    "Media_type.Command_line.pp/parse"
    `Quick
    (fun () ->
      List.iter
        (fun m ->
          let s = Format.asprintf "%a" pp_parameter m in
          let mm = parse_cli_parameter s in
          match mm with
          | None ->
              Format.kasprintf
                Stdlib.failwith
                "No parsing back for %s (%s)"
                (to_string m)
                s
          | Some mm when m <> mm ->
              Format.kasprintf
                Stdlib.failwith
                "No round trip for %s (%s) (%s)"
                (to_string m)
                s
                (to_string mm)
          | Some mm -> assert (m = mm))
        inputs)

let pp_value fmt v =
  Format.fprintf
    fmt
    "%s"
    (match v with
    | `Int i -> string_of_int i
    | `Z z -> Z.to_string z ^ "z"
    | `String s -> "\"" ^ s ^ "\"")

let eq_value x y =
  match (x, y) with
  | `Int x, `Int y -> x = y
  | `Z x, `Z y -> Z.equal x y
  | `String x, `String y -> x = y
  | _, _ -> false

let value_enc =
  let open Data_encoding in
  union
    [
      (* Use an encoding which can potentially overlap when concatenated *)
      case
        (Tag 0)
        ~title:"int"
        Data_encoding.int31
        (function `Int i -> Some i | _ -> None)
        (fun i -> `Int i);
      (* Use a simple variable size data encoding *)
      case
        (Tag 1)
        ~title:"z"
        Data_encoding.(obj1 (req "z" z))
        (function `Z z -> Some z | _ -> None)
        (fun z -> `Z z);
      (* This encoding can have \n in json, they should be escaped *)
      case
        (Tag 2)
        ~title:"string"
        Data_encoding.string
        (function `String s -> Some s | _ -> None)
        (fun s -> `String s);
    ]

module Client =
  Resto_cohttp_client.Client.Make
    (Tezos_rpc.Encoding)
    (Resto_cohttp_client.Client.OfCohttp (Cohttp_lwt_unix.Client))

let test_media_types_chunks =
  let open QCheck2 in
  let open Tezos_rpc_http.Media_type in
  let open Lwt_syntax in
  Qcheck2_helpers.qcheck_make_lwt
    ~name:"Can destruct chunks with multiple values"
    ~count:5_000
    ~print:(fun (media_type, values) ->
      Format.asprintf
        "@[<hov 4>%s -> [@,%a@,]@]"
        (match media_type with
        | `Json -> "json"
        | `Bson -> "bson"
        | `Octet_stream -> "octet-stream")
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
           (fun fmt (v, split) ->
             Format.fprintf
               fmt
               "%a%s"
               pp_value
               v
               (if split then "/split" else "")))
        values)
    ~extract:Lwt_main.run
    ~gen:
      (* TODO: support bson *)
      Gen.(
        pair
          (oneofl [`Json; `Octet_stream; `Bson])
          (small_list (pair Generator.gen_value bool)))
  @@ fun (media_type, values) ->
  let media_type =
    match media_type with
    | `Json -> json
    | `Bson -> bson
    | `Octet_stream -> octet_stream
  in
  let buf = Buffer.create 100 in
  let rchunks = ref [] in
  List.iter
    (fun (i, split) ->
      let chunk = media_type.construct value_enc i in
      if split then (
        (* The test input says this value should be split over two chunks *)
        let mid = String.length chunk / 2 in
        let chunk0 = String.sub chunk 0 mid in
        let chunk1 = String.sub chunk mid (String.length chunk - mid) in
        (* First half in current chunk *)
        Buffer.add_string buf chunk0 ;
        (* Finish chunk0 *)
        rchunks := Buffer.contents buf :: !rchunks ;
        (* Start new chunk *)
        Buffer.reset buf ;
        (* Second half in next chunk *)
        Buffer.add_string buf chunk1)
      else Buffer.add_string buf chunk)
    values ;
  if Buffer.length buf > 0 then
    (* Finish current chunk *)
    rchunks := Buffer.contents buf :: !rchunks ;
  Buffer.reset buf ;
  let chunks = List.rev !rchunks in
  let stream = Lwt_stream.of_list chunks in
  let reconstructed = ref [] in
  let+ () =
    Client.Internal_for_tests.parse_stream
      media_type
      value_enc
      ~log_raw_chunk:(fun _ -> return_unit)
      ~on_chunk:(fun e -> reconstructed := e :: !reconstructed)
      ~on_close:ignore
      stream
  in
  let reconstructed = List.rev !reconstructed in
  let original = List.map fst values in
  Qcheck2_helpers.qcheck_eq
    ~eq:(List.equal eq_value)
    ~pp:(fun fmt ->
      Format.fprintf
        fmt
        "@[<hov>[@,%a@,]@]"
        Format.(
          pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ") pp_value))
    ~__LOC__
    original
    reconstructed

let () =
  let open Qcheck2_helpers in
  Lwt_main.run
  @@ Alcotest_lwt.run
       ~__FILE__
       "tezos-rpc-http"
       [
         ( "qcheck",
           qcheck_wrap_lwt
             [
               test_codec_identity;
               check_find_policy;
               ensure_default_policy_parses;
             ] );
         ("find_policy_matching_rules", [test_finding_policy]);
         ("ensure_unsafe_rpcs_blocked", [ensure_unsafe_rpcs_blocked]);
         ( "test_matching_with_name_resolving",
           [test_matching_with_name_resolving] );
         ("test_media_type_pp_parse", [test_media_type_pp_parse]);
         ("test_media_types_chunks", qcheck_wrap_lwt [test_media_types_chunks]);
       ]
