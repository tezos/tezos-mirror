(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

let () = Printexc.record_backtrace true

open Lwt.Infix
module Encoding = Resto_json.Encoding
module Service = Resto.MakeService (Encoding)
module Directory = Resto_directory.Make (Encoding)
module Media_type = Resto_cohttp.Media_type.Make (Encoding)

(* This tests that the server and client of resto manage to communicate even
   when the output is chunked. *)

let seqing chunk_size s =
  let b = Bytes.unsafe_of_string s in
  let rec aux offset () =
    if offset + chunk_size > Bytes.length b then
      Seq.Cons ((b, offset, Bytes.length b - offset), Seq.empty)
    else Seq.Cons ((b, offset, chunk_size), aux (offset + chunk_size))
  in
  aux 0

let json chunk_size =
  let construct enc v =
    Ezjsonm.value_to_string @@ Json_repr.Ezjsonm.repr
    @@ Json_encoding.construct enc v
  in
  let destruct =
   fun enc body ->
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
        try
          let json = Ezjsonm.value_from_string raw in
          Format.fprintf ppf "%s" (Ezjsonm.value_to_string json)
        with _ -> Format.fprintf ppf "Invalid JSON: %S" raw);
    construct;
    construct_seq =
      (fun enc v ->
        let item = construct enc v in
        seqing chunk_size item);
    destruct;
    destruct_many =
      (fun enc body ->
        match String.split_on_char '\n' body |> List.rev with
        | [] -> ([], "")
        | last :: rfirsts ->
            let firsts =
              List.filter_map
                (fun s ->
                  match destruct enc s with
                  | Error e ->
                      Format.eprintf
                        "Ignoring unparsable value in JSON stream: %s\n\
                         Value: %s@."
                        e
                        s ;
                      None
                  | Ok v -> Some v)
                (List.rev rfirsts)
            in
            (firsts, last));
  }

let media_types chunk_size = [json chunk_size]

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

let kv_list_encoding =
  let open Json_encoding in
  list (tup2 string int32)

let foo_bar =
  let open Resto.Path in
  List.fold_left add_suffix root ["foo"; "bar"]

let get_foo_bar =
  Service.get_service
    ~query:Resto.Query.empty
    ~output:kv_list_encoding
    ~error:Encoding.unit
    foo_bar

let val_foo_bar = []

let foo_blah =
  let open Resto.Path in
  List.fold_left add_suffix root ["foo"; "blah"]

let get_foo_blah =
  Service.get_service
    ~query:Resto.Query.empty
    ~output:kv_list_encoding
    ~error:Encoding.unit
    foo_blah

let gen_foo_blah i = (string_of_int i, Int32.of_int i)

let val_foo_blah = List.init 10 gen_foo_blah

let bwraf =
  let open Resto.Path in
  List.fold_left add_suffix root ["blue"; "white"; "red"; "and"; "fuchsia"]

let post_bwraf =
  Service.post_service
    ~query:Resto.Query.empty
    ~input:Encoding.unit
    ~output:kv_list_encoding
    ~error:Encoding.unit
    bwraf

let stream_bwraf =
  Service.post_service
    ~query:Resto.Query.empty
    ~input:Json_encoding.int
    ~output:Json_encoding.(tup2 string int32)
    ~error:Encoding.unit
    Resto.Path.(root / "stream")

let val_bwraf top = List.init top (fun i -> (string_of_int i, Int32.of_int i))

let gen max =
  let cpt = ref (-1) in
  fun () ->
    incr cpt ;
    if !cpt >= max then None else try Some (gen_foo_blah !cpt) with _ -> None

let directory =
  let open Directory in
  let dir = empty in
  let dir =
    register0 dir get_foo_bar (fun () () -> Lwt.return @@ `OkChunk val_foo_bar)
  in
  let dir =
    register0 dir get_foo_blah (fun () () ->
        Lwt.return @@ `OkChunk val_foo_blah)
  in
  let dir =
    register0 dir post_bwraf (fun () () -> Lwt.return @@ `OkChunk (val_bwraf 7))
  in
  let dir =
    register0 dir stream_bwraf (fun () max ->
        let gen = gen max in
        let next () = Lwt.return (gen ()) in
        Resto_directory.Answer.return_stream {next; shutdown = (fun () -> ())})
  in
  dir

let split_chunks s chunk_size =
  let len = String.length s in
  let rec aux acc ofs =
    if ofs + chunk_size > len then
      let rem = len - ofs in
      if rem = 0 then (List.rev acc, None)
      else (List.rev acc, Some (String.sub s ofs rem))
    else
      let chunk = String.sub s ofs chunk_size in
      aux (chunk :: acc) (ofs + chunk_size)
  in
  aux [] 0

(* This HTTP server merges or splits chunks for the /stream endpoint based on the
   chunk_size parameter. *)
let cohttp_server chunk_size =
  let callback _conn (req : Cohttp.Request.t) (body : Cohttp_lwt.Body.t) =
    match req.resource with
    | "/stream" ->
        Cohttp_lwt.Body.to_string body >>= fun body ->
        let max =
          body |> Ezjsonm.value_from_string |> Json_encoding.(destruct int)
        in
        let buf = Buffer.create chunk_size in
        let next = gen max in
        let stream, push = Lwt_stream.create () in
        let rec gen () =
          match next () with
          | None ->
              if Buffer.length buf > 0 then push (Some (Buffer.contents buf)) ;
              push None ;
              Lwt.return_unit
          | Some v ->
              Lwt.pause () >>= fun () ->
              let s =
                Json_encoding.construct (Service.output_encoding stream_bwraf) v
                |> Ezjsonm.value_to_string
              in
              Buffer.add_string buf s ;
              Buffer.add_char buf '\n' ;
              let chunks, rest =
                split_chunks (Buffer.contents buf) chunk_size
              in
              List.iter (fun s -> push (Some s)) chunks ;
              Buffer.reset buf ;
              Option.iter (Buffer.add_string buf) rest ;
              gen ()
        in
        Lwt.dont_wait gen ignore ;
        Lwt.return
          (`Response
             ( Cohttp.Response.make
                 ~status:`OK
                 ~encoding:Cohttp.Transfer.Chunked
                 (),
               Cohttp_lwt.Body.of_stream stream ))
    | _ ->
        Lwt.return
          (`Response
             (Cohttp.Response.make ~status:`Not_found (), Cohttp_lwt.Body.empty))
  in
  Cohttp_lwt_unix.Server.make_response_action ~callback

let run_stream_cohttp_server chunk_size port =
  cohttp_server chunk_size ()
  |> Cohttp_lwt_unix.Server.create
       ~mode:(`TCP (`Port port))
       ~on_exn:(fun e ->
         Format.eprintf "server exn: %s@." (Printexc.to_string e))

(* TODO port resto tests to Tezt *)
let fresh_port () =
  let dummy_socket = Unix.(socket PF_INET SOCK_STREAM 0) in
  (* allows rebinding to a port that is in TIME_WAIT *)
  Unix.setsockopt dummy_socket Unix.SO_REUSEADDR true ;
  Fun.protect ~finally:(fun () -> Unix.close dummy_socket) @@ fun () ->
  Unix.bind dummy_socket Unix.(ADDR_INET (inet_addr_loopback, 0)) ;
  let addr = Unix.getsockname dummy_socket in
  match addr with ADDR_INET (_, port) -> port | _ -> assert false

let uri port = "http://localhost:" ^ string_of_int port

let is_ok_result r v =
  match r with `Ok (Some w) -> assert (v = w) | _ -> assert false

module Client =
  Resto_cohttp_client.Client.Make
    (Encoding)
    (Resto_cohttp_client.Client.OfCohttp (Cohttp_lwt_unix.Client))

let child port =
  let logger = Client.full_logger Format.err_formatter in
  let base = port |> uri |> Uri.of_string in
  let media_types = media_types 1 in
  Client.call_service media_types ~base ~logger get_foo_bar () () ()
  >>= fun (_, _, r) ->
  is_ok_result r val_foo_bar ;
  Client.call_service media_types ~base ~logger get_foo_blah () () ()
  >>= fun (_, _, r) ->
  is_ok_result r val_foo_blah ;
  Client.call_service media_types ~base ~logger post_bwraf () () ()
  >>= fun (_, _, r) ->
  is_ok_result r (val_bwraf 7) ;
  Stdlib.exit 0

let child_stream port =
  let logger = Client.full_logger Format.err_formatter in
  let base = port |> uri |> Uri.of_string in
  let media_types = media_types 1 in
  let stream, push = Lwt_stream.create () in
  let max = 15 in
  Client.call_streamed_service
    media_types
    ~base
    ~logger
    stream_bwraf
    ()
    ()
    max
    ~on_close:(fun () ->
      Format.eprintf "| STREAM closed@." ;
      push None)
    ~on_chunk:(fun ((k, v) as c) ->
      Format.eprintf "| RECEIVED chunk: [%S, %ld]@." k v ;
      push (Some c))
  >>= fun (_, _, r) ->
  let close = match r with `Ok (Some f) -> f | _ -> assert false in
  Lwt_stream.to_list stream >>= fun read ->
  if read <> val_bwraf max then (
    Format.eprintf "Read:@." ;
    List.iter (fun (k, v) -> Format.eprintf "- [%S, %ld]@." k v) read ;
    assert false) ;
  close () ;
  Stdlib.exit 0

module Server = Resto_cohttp_server.Server.Make (Encoding) (Logger)

let parent pid port chunk_size =
  let media_types = media_types chunk_size in
  Server.init_and_launch ~media_types directory (`TCP (`Port port))
  >>= fun () ->
  Lwt_unix.waitpid [] pid >>= function
  | _, WEXITED 0 -> Lwt.return ()
  | _ -> assert false

let test_one_size chunk_size =
  Printf.printf "Testing chunking with size %d\n%!" chunk_size ;
  let port = fresh_port () in
  match Lwt_unix.fork () with
  | 0 ->
      Lwt_unix.sleep 2.0 (* leave time for the server to start *) >>= fun () ->
      child port
  | pid -> parent pid port chunk_size

let test_stream chunk_size =
  Printf.printf "Testing chunking stream with size %d\n%!" chunk_size ;
  let port = fresh_port () in
  match Lwt_unix.fork () with
  | 0 ->
      Lwt_unix.sleep 2.0 (* leave time for the server to start *) >>= fun () ->
      child_stream port
  | pid ->
      let server = run_stream_cohttp_server chunk_size port in
      let child_stopped =
        Lwt_unix.waitpid [] pid >>= function
        | _, WEXITED 0 -> Lwt.return ()
        | _ -> assert false
      in
      Lwt.pick [server; child_stopped]

let main () =
  (* test smaller and smaller, more and more numerous chunks *)
  let open Lwt.Infix in
  test_one_size (16 * 1024) >>= fun () ->
  test_one_size 64 >>= fun () ->
  test_one_size 1 >>= fun () ->
  test_stream (16 * 1024) >>= fun () ->
  test_stream 64 >>= fun () ->
  test_stream 1 >>= fun () -> Lwt.return_unit

let () =
  Lwt_main.run @@ main () ;
  Stdlib.exit 0
