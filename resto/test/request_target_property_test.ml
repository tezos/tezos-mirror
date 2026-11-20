(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Resto server
   Invocation:   dune build @resto/test/runtest
   Subject:      Unit tests of [Server.normalize_request_target], and
                 properties of it on arbitrary request targets.
*)

let normalize = Resto_cohttp_server.Server.normalize_request_target

let mk ?(meth = `GET) resource =
  {(Cohttp.Request.make (Uri.of_string "/")) with resource; meth}

(* Mirrors the request target validation of cohttp's request parser
   ([Cohttp.Request.is_valid_uri]): only targets accepted by it can reach
   [normalize_request_target].

   Mirrored from cohttp 5.3.1~octez (see [manifest/externals.ml]). Check
   it again whenever cohttp is updated. *)
let is_valid_target ~meth resource =
  resource = "*" || meth = `CONNECT
  ||
  match Uri.scheme (Uri.of_string resource) with
  | Some _ -> true
  | None -> String.length resource > 0 && resource.[0] = '/'

(* Mirrors the target form classification of [Cohttp.Request.uri].

   Mirrored from cohttp 5.3.1~octez (see [manifest/externals.ml]). Check
   it again whenever cohttp is updated. *)
let is_absolute_form (req : Cohttp.Request.t) =
  req.resource <> "*" && req.meth <> `CONNECT
  && Option.is_some (Uri.scheme (Uri.of_string req.resource))

let dispatched_path req =
  Resto.Utils.decode_split_path (Uri.path (Cohttp.Request.uri req))

(* Sends [req] again to another server as an origin-form request, as a
   forwarding proxy does, and returns the path that server dispatches. *)
let forwarded_path req =
  let uri = Cohttp.Request.uri req in
  let forwarded =
    Uri.make
      ~scheme:"http"
      ~host:"forward"
      ~path:(Uri.path uri)
      ~query:(Uri.query uri)
      ()
  in
  dispatched_path (normalize (mk (Uri.path_and_query forwarded)))

(* Unit tests for [normalize_request_target]: absolute-form targets must be
   rewritten so that cohttp removes dot segments, as for origin-form. *)
let test_normalize_request_target () =
  let check ?meth ~expected_resource ~expected_path target =
    let req = normalize (mk ?meth target) in
    let path = Uri.path (Cohttp.Request.uri req) in
    if req.resource <> expected_resource || path <> expected_path then
      Format.kasprintf
        failwith
        "normalize_request_target %S: got resource %S and path %S, expected \
         resource %S and path %S"
        target
        req.resource
        path
        expected_resource
        expected_path ;
    (* Idempotence *)
    let req' = normalize req in
    if req'.resource <> req.resource then
      Format.kasprintf
        failwith
        "normalize_request_target is not idempotent on %S"
        target
  in
  (* Absolute-form: dot segments are removed. *)
  check
    "http://localhost/blue/../foo/bar"
    ~expected_resource:"/blue/../foo/bar"
    ~expected_path:"/foo/bar" ;
  check
    "http://localhost/blue/./../foo/./bar"
    ~expected_resource:"/blue/./../foo/./bar"
    ~expected_path:"/foo/bar" ;
  check
    "http://localhost/../../foo/bar"
    ~expected_resource:"/../../foo/bar"
    ~expected_path:"/foo/bar" ;
  (* Percent-encoded dots are unreserved characters: they are decoded, then
     removed as dot segments. *)
  check
    "http://localhost/blue/%2e%2e/foo/bar"
    ~expected_resource:"/blue/../foo/bar"
    ~expected_path:"/foo/bar" ;
  (* The query is kept verbatim. *)
  check
    "http://localhost/blue/../foo/bar?a=b%20c&d"
    ~expected_resource:"/blue/../foo/bar?a=b%20c&d"
    ~expected_path:"/foo/bar" ;
  (* Empty path. *)
  check "http://localhost" ~expected_resource:"/" ~expected_path:"/" ;
  check "https://localhost:8732" ~expected_resource:"/" ~expected_path:"/" ;
  (* Absolute-form without authority: the path is rooted, so that leading
     dot segments are removed as for origin-form targets (ocaml-uri keeps
     them on a rootless path, see [normalize_request_target]). *)
  check
    "http:../foo/bar"
    ~expected_resource:"/../foo/bar"
    ~expected_path:"/foo/bar" ;
  check
    "http:blue/../../foo/bar"
    ~expected_resource:"/blue/../../foo/bar"
    ~expected_path:"/foo/bar" ;
  check
    "HTTP:%2e%2e/foo/bar"
    ~expected_resource:"/../foo/bar"
    ~expected_path:"/foo/bar" ;
  check
    "file:../foo/bar"
    ~expected_resource:"/../foo/bar"
    ~expected_path:"/foo/bar" ;
  (* The method does not matter, except for CONNECT. *)
  check
    ~meth:`HEAD
    "http://localhost/blue/../foo/bar"
    ~expected_resource:"/blue/../foo/bar"
    ~expected_path:"/foo/bar" ;
  check
    ~meth:`OPTIONS
    "http://localhost/blue/../foo/bar"
    ~expected_resource:"/blue/../foo/bar"
    ~expected_path:"/foo/bar" ;
  (* Origin-form is left unchanged (cohttp already normalizes it), even when
     it contains a colon or an absolute URI in its query. *)
  check
    "/a:b/../foo/bar"
    ~expected_resource:"/a:b/../foo/bar"
    ~expected_path:"/foo/bar" ;
  check
    "/foo/bar?u=http://h/../x"
    ~expected_resource:"/foo/bar?u=http://h/../x"
    ~expected_path:"/foo/bar" ;
  check
    "/blue/../foo/bar"
    ~expected_resource:"/blue/../foo/bar"
    ~expected_path:"/foo/bar" ;
  check "/foo/bar" ~expected_resource:"/foo/bar" ~expected_path:"/foo/bar" ;
  (* Asterisk-form and authority-form are left unchanged. *)
  check ~meth:`OPTIONS "*" ~expected_resource:"*" ~expected_path:"" ;
  check ~meth:`CONNECT "*" ~expected_resource:"*" ~expected_path:"" ;
  check
    ~meth:`CONNECT
    "localhost:8732"
    ~expected_resource:"localhost:8732"
    ~expected_path:""

(* The [Host] header is taken from the authority of an absolute-form
   target, when it has a host, and kept as is otherwise. *)
let test_normalize_host_header () =
  let check ?meth ?host ~expected target =
    let req = mk ?meth target in
    let req =
      match host with
      | None -> req
      | Some h -> {req with headers = Cohttp.Header.init_with "host" h}
    in
    let got = Cohttp.Header.get (normalize req).headers "host" in
    if got <> expected then
      Format.kasprintf
        failwith
        "normalize_request_target %S: got Host %a, expected %a"
        target
        (Format.pp_print_option Format.pp_print_string)
        got
        (Format.pp_print_option Format.pp_print_string)
        expected
  in
  check ~host:"client" "http://h/a" ~expected:(Some "h") ;
  check "http://h/a" ~expected:(Some "h") ;
  check ~host:"client" "HTTP://H:8732/a" ~expected:(Some "h:8732") ;
  check ~host:"client" "http://[::1]:8732/a" ~expected:(Some "[::1]:8732") ;
  check ~host:"client" "https://u:p@h:1/" ~expected:(Some "h:1") ;
  (* The host is percent-encoded again: no CR or LF can reach the header. *)
  check
    ~host:"client"
    "http://a%0d%0aX-Evil:%201/x"
    ~expected:(Some "a%0D%0Ax-evil") ;
  (* No host in the target: the [Host] header is kept. *)
  check ~host:"client" "http:///a" ~expected:(Some "client") ;
  check ~host:"client" "http:../a" ~expected:(Some "client") ;
  (* Origin-form and authority-form: unchanged. *)
  check ~host:"client" "/a" ~expected:(Some "client") ;
  check ~meth:`CONNECT ~host:"client" "h:8732" ~expected:(Some "client")

let gen_request =
  let open QCheck2.Gen in
  let prefix =
    oneofl
      [
        "";
        "/";
        "//";
        "http:";
        "http:/";
        "http:///";
        "http://h";
        "http://h/";
        "HTTP://h:8732/";
        "https://u:p@h/";
        "http://[::1]:8732/";
        "file:";
        "foo:";
        "a+b.c-d:";
      ]
  in
  let segment =
    oneofl
      [
        "";
        "a";
        "b";
        ".";
        "..";
        "%2e";
        "%2E";
        "%2e%2e";
        ".%2e";
        "%252e";
        "%2f";
        "%5c";
        "\\";
        ";";
        ":";
        "@";
        "+";
        "~";
      ]
  in
  let suffix =
    oneofl [""; "?"; "?a=b"; "?a=b%20c&d"; "?u=http://h/../x"; "#f"]
  in
  let meth = oneofl [`GET; `POST; `HEAD; `OPTIONS; `PUT; `CONNECT] in
  let* meth in
  let* prefix in
  let* segments = list_size (int_range 0 6) segment in
  let* suffix in
  return (meth, prefix ^ String.concat "/" segments ^ suffix)

let print_request (_, resource) = resource

let test_property ~name property =
  QCheck2.Test.make ~name ~count:20_000 ~print:print_request gen_request
  @@ fun (meth, resource) ->
  QCheck2.assume (is_valid_target ~meth resource) ;
  property (mk ~meth resource)

let tests =
  [
    (* Requests that cohttp does not handle as absolute-form are returned
       as is, so that their handling is unchanged. *)
    test_property ~name:"identity on non absolute-form targets" (fun req ->
        is_absolute_form req || normalize req == req);
    test_property ~name:"idempotence" (fun req ->
        let req = normalize req in
        (normalize req).resource = req.resource);
    test_property ~name:"normalized targets are origin-form" (fun req ->
        (not (is_absolute_form req))
        ||
        let req = normalize req in
        String.length req.resource > 0 && req.resource.[0] = '/');
    (* The path dispatched for a normalized request is the same as the one
       dispatched once it is forwarded as an origin-form request. *)
    test_property ~name:"same path once forwarded" (fun req ->
        req.meth = `CONNECT
        ||
        let req = normalize req in
        dispatched_path req = forwarded_path req);
  ]

(* The CORS host check gives the same decision for an origin-form request
   and for its absolute-form equivalent, whatever the case of the host:
   the host of an absolute-form target is lowercased when parsed. *)
let test_cors_host_case_equivalence () =
  let check ~allowed_origin ~host =
    let cors =
      Resto_cohttp.Cors.
        {allowed_headers = []; allowed_origins = [allowed_origin]}
    in
    let normalized_headers target =
      let req = mk target in
      let req = {req with headers = Cohttp.Header.init_with "host" host} in
      (normalize req).headers
    in
    let origin_headers = normalized_headers "/foo" in
    if not (Resto_cohttp.Cors.check_host origin_headers cors) then
      Format.kasprintf
        failwith
        "Origin-form request failed the CORS host check: Host=%s, allowed \
         origin=%s"
        host
        allowed_origin ;
    let absolute_headers = normalized_headers ("http://" ^ host ^ "/foo") in
    if not (Resto_cohttp.Cors.check_host absolute_headers cors) then
      Format.kasprintf
        failwith
        "Absolute-form request failed the equivalent CORS host check: Host=%a, \
         allowed origin=%s"
        (Format.pp_print_option Format.pp_print_string)
        (Cohttp.Header.get absolute_headers "host")
        allowed_origin
  in
  check ~allowed_origin:"Example.COM" ~host:"Example.COM" ;
  check ~allowed_origin:"example.com" ~host:"Example.COM" ;
  check ~allowed_origin:"Example.COM" ~host:"example.com" ;
  check ~allowed_origin:"Example.COM:8732" ~host:"Example.COM:8732"

let () =
  test_normalize_request_target () ;
  test_normalize_host_header () ;
  test_cors_host_case_equivalence () ;
  List.iter (QCheck2.Test.check_exn ~rand:(Random.State.make [|42|])) tests
