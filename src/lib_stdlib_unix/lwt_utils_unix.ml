(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Error_monad

let () =
  register_error_kind
    `Temporary
    ~id:"unix_error"
    ~title:"Unix error"
    ~description:"An unhandled unix exception"
    ~pp:Format.pp_print_string
    Data_encoding.(obj1 (req "msg" string))
    (function
      | Exn (Unix.Unix_error (err, fn, _)) ->
          Some ("Unix error in " ^ fn ^ ": " ^ Unix.error_message err)
      | _ -> None)
    (fun msg -> Exn (Failure msg))

let default_net_timeout = ref (Ptime.Span.of_int_s 8)

let end_of_file_if_zero nb_read =
  if nb_read = 0 then Lwt.fail End_of_file else Lwt.return_unit

let read_bytes_with_timeout ?(timeout = !default_net_timeout) ?file_offset
    ?(pos = 0) ?len fd buf =
  let buflen = Bytes.length buf in
  let len = match len with None -> buflen - pos | Some l -> l in
  if pos < 0 || pos + len > buflen then invalid_arg "read_bytes" ;
  let rec inner nb_read pos len =
    if len = 0 then Lwt.return_unit
    else
      let open Lwt_syntax in
      let reader =
        match file_offset with
        | None -> Lwt_unix.read
        | Some fo -> Lwt_unix.pread ~file_offset:(fo + nb_read)
      in
      let* nb_read' =
        Lwt_unix.with_timeout (Ptime.Span.to_float_s timeout) (fun () ->
            reader fd buf pos len)
      in
      let* () = end_of_file_if_zero nb_read' in
      inner (nb_read + nb_read') (pos + nb_read') (len - nb_read')
  in
  inner 0 pos len

let read_bytes ?file_offset ?(pos = 0) ?len fd buf =
  let buflen = Bytes.length buf in
  let len = match len with None -> buflen - pos | Some l -> l in
  if pos < 0 || pos + len > buflen then invalid_arg "read_bytes" ;
  let rec inner nb_read pos len =
    if len = 0 then Lwt.return_unit
    else
      let open Lwt_syntax in
      let reader =
        match file_offset with
        | None -> Lwt_unix.read
        | Some fo -> Lwt_unix.pread ~file_offset:(fo + nb_read)
      in
      let* nb_read' = reader fd buf pos len in
      let* () = end_of_file_if_zero nb_read' in
      inner (nb_read + nb_read') (pos + nb_read') (len - nb_read')
  in
  inner 0 pos len

let write_bytes ?file_offset ?(pos = 0) ?len descr buf =
  let buflen = Bytes.length buf in
  let len = match len with None -> buflen - pos | Some l -> l in
  if pos < 0 || pos + len > buflen then invalid_arg "write_bytes" ;
  let rec inner nb_written pos len =
    if len = 0 then Lwt.return_unit
    else
      let open Lwt_syntax in
      let writer =
        match file_offset with
        | None -> Lwt_unix.write
        | Some fo -> Lwt_unix.pwrite ~file_offset:(fo + nb_written)
      in
      let* nb_written' = writer descr buf pos len in
      let* () = end_of_file_if_zero nb_written' in
      inner (nb_written + nb_written') (pos + nb_written') (len - nb_written')
  in
  inner 0 pos len

let write_string ?(pos = 0) ?len descr buf =
  let len = match len with None -> String.length buf - pos | Some l -> l in
  let rec inner pos len =
    if len = 0 then Lwt.return_unit
    else
      let open Lwt_syntax in
      let* nb_written = Lwt_unix.write_string descr buf pos len in
      let* () = end_of_file_if_zero nb_written in
      inner (pos + nb_written) (len - nb_written)
  in
  inner pos len

let is_directory file_name =
  let open Lwt_syntax in
  let+ s = Lwt_unix.stat file_name in
  s.st_kind = S_DIR

let dir_exists path =
  Lwt.try_bind
    (fun () -> Lwt_unix.stat path)
    (fun {st_kind; _} -> Lwt.return (st_kind = S_DIR))
    (function Unix.Unix_error _ -> Lwt.return_false | e -> Lwt.reraise e)

let remove_dir dir =
  let open Lwt_syntax in
  let rec remove dir =
    let files = Lwt_unix.files_of_directory dir in
    let* () =
      Lwt_stream.iter_s
        (fun file ->
          if file = "." || file = ".." then Lwt.return_unit
          else
            let file = Filename.concat dir file in
            if Sys.is_directory file then remove file else Lwt_unix.unlink file)
        files
    in
    Lwt_unix.rmdir dir
  in
  let* dir_exists = dir_exists dir in
  if dir_exists then remove dir else Lwt.return_unit

let rec create_dir ?(perm = 0o755) dir =
  let open Lwt_syntax in
  let* dir_exists = Lwt_unix.file_exists dir in
  if not dir_exists then
    let* () = create_dir (Filename.dirname dir) in
    Lwt.catch
      (fun () -> Lwt_unix.mkdir dir perm)
      (function
        | Unix.Unix_error (Unix.EEXIST, _, _) ->
            (* This is the case where the directory has been created
               by another Lwt.t, after the call to Lwt_unix.file_exists. *)
            Lwt.return_unit
        | e -> Lwt.reraise e)
  else
    let* {st_kind; _} = Lwt_unix.stat dir in
    match st_kind with
    | S_DIR -> Lwt.return_unit
    | _ -> Stdlib.failwith "Not a directory"

let safe_close fd =
  let open Lwt_result_syntax in
  Lwt.catch
    (fun () ->
      let*! () = Lwt_unix.close fd in
      return_unit)
    (fun exc -> tzfail (Exn exc))

let create_file ?(close_on_exec = true) ?(perm = 0o644) name content =
  let open Lwt_syntax in
  let flags =
    let open Unix in
    let flags = [O_TRUNC; O_CREAT; O_WRONLY] in
    if close_on_exec then O_CLOEXEC :: flags else flags
  in
  let* fd = Lwt_unix.openfile name flags perm in
  Lwt.try_bind
    (fun () -> write_string fd ~pos:0 ~len:(String.length content) content)
    (fun v ->
      let* ru = safe_close fd in
      let () =
        Result.iter_error
          (fun trace ->
            Format.eprintf "Uncaught error: %a\n%!" pp_print_trace trace)
          ru
      in
      Lwt.return v)
    (fun exc ->
      let* ru = safe_close fd in
      let () =
        Result.iter_error
          (fun trace ->
            Format.eprintf "Uncaught error: %a\n%!" pp_print_trace trace)
          ru
      in
      raise exc)

let read_file fn = Lwt_io.with_file fn ~mode:Input (fun ch -> Lwt_io.read ch)

let copy_file ~src ~dst =
  let open Lwt_syntax in
  Lwt_io.with_file ~mode:Output dst (fun dst_ch ->
      Lwt_io.with_file src ~mode:Input (fun src_ch ->
          let buff = Bytes.create 4096 in
          let rec loop () =
            let* n = Lwt_io.read_into src_ch buff 0 4096 in
            match n with
            | 0 -> Lwt.return_unit
            | n ->
                let* () = Lwt_io.write_from_exactly dst_ch buff 0 n in
                loop ()
          in
          loop ()))

let copy_dir ?(perm = 0o755) src dst =
  let open Lwt_syntax in
  let rec copy_dir dir dst_dir =
    let* () = create_dir ~perm dst in
    let files = Lwt_unix.files_of_directory dir in
    Lwt_stream.iter_p
      (fun file ->
        if file = Filename.current_dir_name || file = Filename.parent_dir_name
        then Lwt.return_unit
        else
          let basename = file in
          let file = Filename.concat dir file in
          if Sys.is_directory file then
            let new_dir = Filename.concat dst_dir basename in
            let* () = create_dir ~perm new_dir in
            copy_dir file new_dir
          else copy_file ~src:file ~dst:(Filename.concat dst_dir basename))
      files
  in
  let* src_dir_exists = dir_exists src in
  if src_dir_exists then copy_dir src dst
  else Lwt.fail (Unix.Unix_error (Unix.ENOTDIR, "", "copy_dir"))

let of_sockaddr = function
  | Unix.ADDR_UNIX _ -> None
  | Unix.ADDR_INET (addr, port) -> (
      match Ipaddr_unix.of_inet_addr addr with
      | V4 addr -> Some (Ipaddr.v6_of_v4 addr, port)
      | V6 addr -> Some (addr, port))

let getaddrinfo ~passive ~node ~service =
  let open Lwt_unix in
  let open Lwt_syntax in
  let* addr =
    getaddrinfo
      node
      service
      (AI_SOCKTYPE SOCK_STREAM :: (if passive then [AI_PASSIVE] else []))
  in
  let points = List.filter_map (fun {ai_addr; _} -> of_sockaddr ai_addr) addr in
  Lwt.return points

let getpass () =
  let open Unix in
  (* Turn echoing off and fail if we can't. *)
  let tio = tcgetattr stdin in
  let old_echo = tio.c_echo in
  let old_echonl = tio.c_echonl in
  tio.c_echo <- false ;
  tio.c_echonl <- true ;
  tcsetattr stdin TCSAFLUSH tio ;
  (* Read the passwd. *)
  let passwd = read_line () in
  (* Restore terminal. *)
  tio.c_echo <- old_echo ;
  tio.c_echonl <- old_echonl ;
  tcsetattr stdin TCSAFLUSH tio ;
  passwd

module Json = struct
  let to_root = function
    | `O ctns -> `O ctns
    | `A ctns -> `A ctns
    | `Null -> `O []
    | oth -> `A [oth]

  let write_file file json =
    let json = to_root json in
    protect (fun () ->
        Lwt_io.with_file ~mode:Output file (fun chan ->
            let open Lwt_result_syntax in
            let str = Data_encoding.Json.to_string ~minify:false json in
            let*! () = Lwt_io.write chan str in
            return_unit))

  let read_file file =
    protect (fun () ->
        Lwt_io.with_file ~mode:Input file (fun chan ->
            let open Lwt_result_syntax in
            let*! str = Lwt_io.read chan in
            return (Ezjsonm.from_string str :> Data_encoding.json)))
end

let with_tempdir name f =
  let open Lwt_syntax in
  let base_dir = Filename.temp_file name "" in
  let* () = Lwt_unix.unlink base_dir in
  let* () = Lwt_unix.mkdir base_dir 0o700 in
  Lwt.finalize (fun () -> f base_dir) (fun () -> remove_dir base_dir)

let rec retry ?(log = fun _ -> Lwt.return_unit) ?(n = 5) ?(sleep = 1.) f =
  let open Lwt_syntax in
  let* rr = f () in
  match rr with
  | Ok _ as r -> Lwt.return r
  | Error error as r ->
      if n > 0 then
        let* () = log error in
        let* () = Lwt_unix.sleep sleep in
        retry ~log ~n:(n - 1) ~sleep f
      else Lwt.return r

type 'action io_error = {
  action : 'action;
  unix_code : Unix.error;
  caller : string;
  arg : string;
}

type error += Io_error of [`Close | `Open | `Rename | `Unlink | `Lock] io_error

let tzfail_of_io_error
    (e : [< `Close | `Open | `Rename | `Unlink | `Lock] io_error) =
  Error [Io_error e]

let () =
  register_error_kind
    `Permanent
    ~id:"io_error"
    ~title:"IO error"
    ~description:"IO error"
    ~pp:(fun ppf (_action, unix_code, caller, arg) ->
      Format.fprintf
        ppf
        "IO error in %s(%s): %s)"
        caller
        arg
        (Unix.error_message unix_code))
    Data_encoding.(
      obj4
        (req
           "action"
           (string_enum
              [
                ("close", `Close);
                ("open", `Open);
                ("Rename", `Rename);
                ("Unlink", `Unlink);
                ("lock", `Lock);
              ]))
        (req "unix_code" Unix_error.encoding)
        (req "caller" string)
        (req "arg" string))
    (function
      | Io_error {action; unix_code; caller; arg} ->
          Some (action, unix_code, caller, arg)
      | _ -> None)
    (fun (action, unix_code, caller, arg) ->
      Io_error {action; unix_code; caller; arg})

let with_open_file ~flags ?(perm = 0o640) filename task =
  let open Lwt_syntax in
  let* rfd =
    Lwt.catch
      (fun () ->
        let* r = Lwt_unix.openfile filename flags perm in
        Lwt.return (Ok r))
      (function
        | Unix.Unix_error (unix_code, caller, arg) ->
            Lwt.return (Error {action = `Open; unix_code; caller; arg})
        | exn -> Lwt.reraise exn)
  in
  match rfd with
  | Error _ as r -> Lwt.return r
  | Ok fd ->
      let* res = task fd in
      Lwt.catch
        (fun () ->
          let* () = Lwt_unix.close fd in
          Lwt.return (Ok res))
        (function
          | Unix.Unix_error (unix_code, caller, arg) ->
              Lwt.return (Error {action = `Close; unix_code; caller; arg})
          | exn -> Lwt.reraise exn)

let with_open_out ?(overwrite = true) file task =
  let flags =
    let open Unix in
    if overwrite then [O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC]
    else [O_WRONLY; O_CREAT; O_CLOEXEC]
  in
  with_open_file ~flags file task

let with_open_in file task =
  with_open_file ~flags:[O_RDONLY; O_CLOEXEC] file task

(* This is to avoid file corruption *)
let with_atomic_open_out ?(overwrite = true) filename
    ?(temp_dir = Filename.dirname filename) f =
  let open Lwt_result_syntax in
  let temp_file =
    Filename.temp_file ~temp_dir (Filename.basename filename) ".tmp"
  in
  let* res = with_open_out ~overwrite temp_file f in
  Lwt.catch
    (fun () ->
      let*! () = Lwt_unix.rename temp_file filename in
      return res)
    (function
      | Unix.Unix_error (unix_code, caller, arg) ->
          Lwt.return (Error {action = `Rename; unix_code; caller; arg})
      | exn -> Lwt.reraise exn)
