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

open Lwt.Infix
module Encoding = Resto_json.Encoding
module Service = Resto.MakeService (Encoding)
module Directory = Resto_directory.Make (Encoding)
module Media_type = Resto_cohttp.Media_type.Make (Encoding)

let json =
  let destruct enc body =
    try
      let json = Ezjsonm.value_from_string body in
      Ok (Json_encoding.destruct enc json)
    with exc -> Error (Printexc.to_string exc)
  in
  {
    Media_type.name = Cohttp.Accept.MediaType ("application", "json");
    q = Some 1000;
    pp =
      (fun _enc ppf raw ->
        let json = Ezjsonm.from_string raw in
        Format.fprintf ppf "%s" (Ezjsonm.to_string json));
    construct =
      (fun enc v ->
        Ezjsonm.value_to_string @@ Json_repr.Ezjsonm.repr
        @@ Json_encoding.construct enc v);
    construct_seq =
      (fun enc v ->
        let item =
          Ezjsonm.value_to_string @@ Json_repr.Ezjsonm.repr
          @@ Json_encoding.construct enc v
        in
        Seq.return (Bytes.of_string item, 0, String.length item));
    destruct;
    destruct_many =
      (fun enc body ->
        match destruct enc body with Ok v -> ([v], "") | Error _ -> ([], body));
  }

let media_types = [json]

module Logger : Resto_cohttp_server.Server.LOGGING = struct
  let log_debug fmt = Format.kasprintf (Format.printf "[DEBUG]: %s\n%!") fmt

  let log_info fmt = Format.kasprintf (Printf.printf "[INFO]: %s\n%!") fmt

  let log_notice fmt = Format.kasprintf (Printf.printf "[NOTICE]: %s\n%!") fmt

  let log_warn fmt = Format.kasprintf (Printf.printf "[WARN]: %s\n%!") fmt

  let log_error fmt = Format.kasprintf (Printf.eprintf "[ERROR]: %s\n%!") fmt

  let lwt_log_debug fmt =
    Format.kasprintf Lwt_fmt.(fprintf stdout "[DEBUG]: %s\n%!") fmt

  let lwt_log_info fmt =
    Format.kasprintf Lwt_fmt.(fprintf stdout "[INFO]: %s\n%!") fmt

  let lwt_log_notice fmt =
    Format.kasprintf Lwt_fmt.(fprintf stdout "[NOTICE]: %s\n%!") fmt

  let lwt_log_warn fmt =
    Format.kasprintf Lwt_fmt.(fprintf stdout "[WARN]: %s\n%!") fmt

  let lwt_log_error fmt =
    Format.kasprintf Lwt_fmt.(fprintf stderr "[ERROR]: %s\n%!") fmt
end

let foo_bar =
  let open Resto.Path in
  List.fold_left add_suffix root ["foo"; "bar"]

let get_foo_bar =
  Service.get_service
    ~query:Resto.Query.empty
    ~output:Encoding.unit
    ~error:Encoding.unit
    foo_bar

let foo_blah =
  let open Resto.Path in
  List.fold_left add_suffix root ["foo"; "blah"]

let get_foo_blah =
  Service.get_service
    ~query:Resto.Query.empty
    ~output:Encoding.unit
    ~error:Encoding.unit
    foo_blah

let bwraf =
  let open Resto.Path in
  List.fold_left add_suffix root ["blue"; "white"; "red"; "and"; "fuchsia"]

let post_bwraf =
  Service.post_service
    ~query:Resto.Query.empty
    ~input:Encoding.unit
    ~output:Encoding.unit
    ~error:Encoding.unit
    bwraf

let directory =
  let open Directory in
  let dir = empty in
  let dir = register0 dir get_foo_bar (fun () () -> Lwt.return @@ `Ok ()) in
  let dir = register0 dir get_foo_blah (fun () () -> Lwt.return @@ `Ok ()) in
  let dir = register0 dir post_bwraf (fun () () -> Lwt.return @@ `Ok ()) in
  dir

let port = 8000

let uri = "http://localhost:8000"

let child expect_foo_bar expect_foo_blah expect_bwraf =
  let module Client =
    Resto_cohttp_client.Client.Make
      (Encoding)
      (Resto_cohttp_client.Client.OfCohttp (Cohttp_lwt_unix.Client))
  in
  let logger = Client.full_logger Format.err_formatter in
  let base = Uri.of_string uri in
  let open Lwt.Infix in
  Client.call_service media_types ~base ~logger get_foo_bar () () ()
  >>= fun (_, _, r) ->
  assert (r = expect_foo_bar) ;
  Client.call_service media_types ~base ~logger get_foo_blah () () ()
  >>= fun (_, _, r) ->
  assert (r = expect_foo_blah) ;
  Client.call_service media_types ~base ~logger post_bwraf () () ()
  >>= fun (_, _, r) ->
  assert (r = expect_bwraf) ;
  Stdlib.exit 0

let parent pid =
  Lwt_unix.waitpid [] pid >>= function
  | _, WEXITED 0 -> Lwt.return ()
  | _ -> assert false

module Server = Resto_cohttp_server.Server.Make (Encoding) (Logger)

(* Sends a raw HTTP request with the given request target, so that
   absolute-form targets are sent verbatim, and returns the status code. *)
let raw_request_status ?(port = port) ?(meth = "GET") target =
  let body = if meth = "POST" then "{}" else "" in
  let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  Lwt_io.with_connection addr (fun (ic, oc) ->
      Lwt_io.write
        oc
        (Printf.sprintf
           "%s %s HTTP/1.1\r\n\
            Host: localhost\r\n\
            Content-Type: application/json\r\n\
            Content-Length: %d\r\n\
            Connection: close\r\n\
            \r\n\
            %s"
           meth
           target
           (String.length body)
           body)
      >>= fun () ->
      Lwt_io.read_line ic >>= fun status_line ->
      match String.split_on_char ' ' status_line with
      | _ :: code :: _ -> Lwt.return (int_of_string code)
      | _ -> Lwt.fail_with ("Unexpected status line: " ^ status_line))

let check_raw_status ?port ?meth target expected =
  raw_request_status ?port ?meth target >>= fun status ->
  if status <> expected then
    Format.kasprintf
      failwith
      "Request %S: got status %d, expected %d"
      target
      status
      expected ;
  Lwt.return_unit

(* A signal setup with both soft and hard exits to test both behaviours *)
let main () =
  (* set up and start the server *)
  let open Lwt.Infix in
  let server = Server.init_server ~media_types directory in
  Server.launch server (`TCP (`Port port)) >>= fun () ->
  (* first test *)
  Server.set_acl server
  @@ Resto_acl.Acl.Allow_all
       {except = [{meth = Any; path = Exact [Literal "foo"; Wildcard]}]} ;
  (match Lwt_unix.fork () with
  | 0 ->
      Lwt_unix.sleep 1.0 >>= fun () ->
      child (`Unauthorized None) (`Unauthorized None) (`Ok (Some ()))
  | pid -> parent pid)
  >>= fun () ->
  (* second test *)
  Server.set_acl server
  @@ Resto_acl.Acl.Deny_all
       {except = [{meth = Any; path = FollowedByAnySuffix [Literal "foo"]}]} ;
  (match Lwt_unix.fork () with
  | 0 ->
      Lwt_unix.sleep 1.0 >>= fun () ->
      child (`Ok (Some ())) (`Ok (Some ())) (`Unauthorized None)
  | pid -> parent pid)
  >>= fun () ->
  (* third test *)
  Server.set_acl server
  @@ Resto_acl.Acl.Deny_all
       {
         except =
           [
             {meth = Exact `GET; path = FollowedByAnySuffix [Literal "foo"]};
             {meth = Exact `DELETE; path = FollowedByAnySuffix []};
           ];
       } ;
  (match Lwt_unix.fork () with
  | 0 ->
      Lwt_unix.sleep 1.0 >>= fun () ->
      child (`Ok (Some ())) (`Ok (Some ())) (`Unauthorized None)
  | pid -> parent pid)
  >>= fun () ->
  (* fourth test *)
  Server.set_acl server
  @@ Resto_acl.Acl.Deny_all
       {
         except =
           [
             {
               meth = Exact `POST;
               path =
                 FollowedByAnySuffix
                   [
                     Literal "blue";
                     Literal "white";
                     Wildcard;
                     Wildcard;
                     Literal "fuchsia";
                   ];
             };
           ];
       } ;
  (match Lwt_unix.fork () with
  | 0 ->
      Lwt_unix.sleep 1.0 >>= fun () ->
      child (`Unauthorized None) (`Unauthorized None) (`Ok (Some ()))
  | pid -> parent pid)
  >>= fun () ->
  (* fifth test: absolute-form request targets are handled as their
     origin-form equivalent. *)
  Server.set_acl server
  @@ Resto_acl.Acl.Allow_all
       {except = [{meth = Any; path = Exact [Literal "foo"; Wildcard]}]} ;
  check_raw_status "/foo/bar" 401 >>= fun () ->
  check_raw_status "/blue/../foo/bar" 401 >>= fun () ->
  check_raw_status "http://localhost/foo/bar" 401 >>= fun () ->
  check_raw_status "http://localhost/blue/../foo/bar" 401 >>= fun () ->
  check_raw_status "http://localhost/blue/%2e%2e/foo/bar" 401 >>= fun () ->
  check_raw_status "http://localhost/../foo/bar" 401 >>= fun () ->
  check_raw_status "http:../foo/bar" 401 >>= fun () ->
  check_raw_status "http:blue/../../foo/bar" 401 >>= fun () ->
  check_raw_status "HTTP:%2e%2e/foo/bar" 401 >>= fun () ->
  check_raw_status
    ~meth:"POST"
    "http://localhost/foo/../blue/white/red/and/fuchsia"
    200
  >>= fun () ->
  Server.set_acl server
  @@ Resto_acl.Acl.Deny_all
       {except = [{meth = Any; path = FollowedByAnySuffix [Literal "foo"]}]} ;
  check_raw_status "http://localhost/foo/bar" 200 >>= fun () ->
  check_raw_status "http://localhost/blue/../foo/bar" 200 >>= fun () ->
  check_raw_status
    ~meth:"POST"
    "http://localhost/foo/../blue/white/red/and/fuchsia"
    401
  >>= fun () ->
  (* sixth test: a custom callback given to [launch] gets the normalized
     request, so that layers running before [resto_callback] (here, an
     extra endpoint looked up by path) see the same path as resto. *)
  let extra_port = port + 1 in
  let extra_server = Server.init_server ~media_types directory in
  let callback conn req body =
    match Uri.path (Cohttp.Request.uri req) with
    | "/extra" ->
        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"" ()
        >>= fun response -> Lwt.return (`Response response)
    | _ -> Server.resto_callback extra_server conn req body
  in
  Server.launch extra_server ~callback (`TCP (`Port extra_port)) >>= fun () ->
  check_raw_status ~port:extra_port "/blue/../extra" 200 >>= fun () ->
  check_raw_status ~port:extra_port "http://localhost/extra" 200 >>= fun () ->
  check_raw_status ~port:extra_port "http://localhost/blue/../extra" 200
  >>= fun () ->
  check_raw_status ~port:extra_port "http:../extra" 200 >>= fun () ->
  check_raw_status ~port:extra_port "http://localhost/blue/../foo/bar" 200
  >>= fun () ->
  Server.shutdown extra_server >>= fun () ->
  (* end of tests *)
  Lwt.return ()

let () =
  Lwt_main.run @@ main () ;
  Stdlib.exit 0
