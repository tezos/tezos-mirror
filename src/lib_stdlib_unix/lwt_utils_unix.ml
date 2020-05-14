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
      | _ ->
          None)
    (fun msg -> Exn (Failure msg))

let default_net_timeout = ref (Ptime.Span.of_int_s 8)

let read_bytes_with_timeout ?(timeout = !default_net_timeout) ?file_offset
    ?(pos = 0) ?len fd buf =
  let buflen = Bytes.length buf in
  let len = match len with None -> buflen - pos | Some l -> l in
  if pos < 0 || pos + len > buflen then invalid_arg "read_bytes" ;
  let rec inner nb_read pos len =
    if len = 0 then Lwt.return_unit
    else
      let reader =
        match file_offset with
        | None ->
            Lwt_unix.read
        | Some fo ->
            Lwt_unix.pread ~file_offset:(fo + nb_read)
      in
      Lwt_unix.with_timeout (Ptime.Span.to_float_s timeout) (fun () ->
          reader fd buf pos len)
      >>= function
      | 0 ->
          Lwt.fail End_of_file
          (* other endpoint cleanly closed its connection *)
      | nb_read' ->
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
      let reader =
        match file_offset with
        | None ->
            Lwt_unix.read
        | Some fo ->
            Lwt_unix.pread ~file_offset:(fo + nb_read)
      in
      reader fd buf pos len
      >>= function
      | 0 ->
          Lwt.fail End_of_file
      | nb_read' ->
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
      let writer =
        match file_offset with
        | None ->
            Lwt_unix.write
        | Some fo ->
            Lwt_unix.pwrite ~file_offset:(fo + nb_written)
      in
      writer descr buf pos len
      >>= function
      | 0 ->
          Lwt.fail End_of_file
          (* other endpoint cleanly closed its connection *)
      | nb_written' ->
          inner
            (nb_written + nb_written')
            (pos + nb_written')
            (len - nb_written')
  in
  inner 0 pos len

let write_string ?(pos = 0) ?len descr buf =
  let len = match len with None -> String.length buf - pos | Some l -> l in
  let rec inner pos len =
    if len = 0 then Lwt.return_unit
    else
      Lwt_unix.write_string descr buf pos len
      >>= function
      | 0 ->
          Lwt.fail End_of_file
          (* other endpoint cleanly closed its connection *)
      | nb_written ->
          inner (pos + nb_written) (len - nb_written)
  in
  inner pos len

let remove_dir dir =
  let rec remove dir =
    let files = Lwt_unix.files_of_directory dir in
    Lwt_stream.iter_s
      (fun file ->
        if file = "." || file = ".." then Lwt.return_unit
        else
          let file = Filename.concat dir file in
          if Sys.is_directory file then remove file else Lwt_unix.unlink file)
      files
    >>= fun () -> Lwt_unix.rmdir dir
  in
  if Sys.file_exists dir && Sys.is_directory dir then remove dir
  else Lwt.return_unit

let rec create_dir ?(perm = 0o755) dir =
  Lwt_unix.file_exists dir
  >>= function
  | false ->
      create_dir (Filename.dirname dir)
      >>= fun () ->
      Lwt.catch
        (fun () -> Lwt_unix.mkdir dir perm)
        (function
          | Unix.Unix_error (Unix.EEXIST, _, _) ->
              (* This is the case where the directory has been created
                 by another Lwt.t, after the call to Lwt_unix.file_exists. *)
              Lwt.return_unit
          | e ->
              Lwt.fail e)
  | true -> (
      Lwt_unix.stat dir
      >>= function
      | {st_kind = S_DIR; _} ->
          Lwt.return_unit
      | _ ->
          Stdlib.failwith "Not a directory" )

let safe_close fd =
  Lwt.catch
    (fun () -> Lwt_unix.close fd >>= fun () -> return_unit)
    (fun exc -> fail (Exn exc))

let create_file ?(close_on_exec = true) ?(perm = 0o644) name content =
  let flags =
    let open Unix in
    let flags = [O_TRUNC; O_CREAT; O_WRONLY] in
    if close_on_exec then O_CLOEXEC :: flags else flags
  in
  Lwt_unix.openfile name flags perm
  >>= fun fd ->
  Lwt.try_bind
    (fun () -> write_string fd ~pos:0 ~len:(String.length content) content)
    (fun v ->
      safe_close fd
      >>= function
      | Error trace ->
          Format.eprintf "Uncaught error: %a\n%!" pp_print_error trace ;
          Lwt.return v
      | Ok () ->
          Lwt.return v)
    (fun exc ->
      safe_close fd
      >>= function
      | Error trace ->
          Format.eprintf "Uncaught error: %a\n%!" pp_print_error trace ;
          raise exc
      | Ok () ->
          raise exc)

let read_file fn = Lwt_io.with_file fn ~mode:Input (fun ch -> Lwt_io.read ch)

let copy_file ~src ~dst =
  Lwt_io.with_file ~mode:Output dst (fun dst_ch ->
      Lwt_io.with_file src ~mode:Input (fun src_ch ->
          let buff = Bytes.create 4096 in
          let rec loop () =
            Lwt_io.read_into src_ch buff 0 4096
            >>= function
            | 0 ->
                Lwt.return_unit
            | n ->
                Lwt_io.write_from_exactly dst_ch buff 0 n >>= fun () -> loop ()
          in
          loop ()))

let copy_dir ?(perm = 0o755) src dst =
  let rec copy_dir dir dst_dir =
    create_dir ~perm dst
    >>= fun () ->
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
            create_dir ~perm new_dir >>= fun () -> copy_dir file new_dir
          else copy_file ~src:file ~dst:(Filename.concat dst_dir basename))
      files
  in
  if Sys.file_exists src && Sys.is_directory src then copy_dir src dst
  else Lwt.fail (Unix.Unix_error (Unix.ENOTDIR, "", "copy_dir"))

let of_sockaddr = function
  | Unix.ADDR_UNIX _ ->
      None
  | Unix.ADDR_INET (addr, port) -> (
    match Ipaddr_unix.of_inet_addr addr with
    | V4 addr ->
        Some (Ipaddr.v6_of_v4 addr, port)
    | V6 addr ->
        Some (addr, port) )

let getaddrinfo ~passive ~node ~service =
  let open Lwt_unix in
  getaddrinfo
    node
    service
    (AI_SOCKTYPE SOCK_STREAM :: (if passive then [AI_PASSIVE] else []))
  >>= fun addr ->
  let points =
    List.filter_map (fun {ai_addr; _} -> of_sockaddr ai_addr) addr
  in
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
    | `O ctns ->
        `O ctns
    | `A ctns ->
        `A ctns
    | `Null ->
        `O []
    | oth ->
        `A [oth]

  let write_file file json =
    let json = to_root json in
    protect (fun () ->
        Lwt_io.with_file ~mode:Output file (fun chan ->
            let str = Data_encoding.Json.to_string ~minify:false json in
            Lwt_io.write chan str >|= ok))

  let read_file file =
    protect (fun () ->
        Lwt_io.with_file ~mode:Input file (fun chan ->
            Lwt_io.read chan
            >>= fun str ->
            return (Ezjsonm.from_string str :> Data_encoding.json)))
end

let with_tempdir name f =
  let base_dir = Filename.temp_file name "" in
  Lwt_unix.unlink base_dir
  >>= fun () ->
  Lwt_unix.mkdir base_dir 0o700
  >>= fun () ->
  Lwt.finalize (fun () -> f base_dir) (fun () -> remove_dir base_dir)

let rec retry ?(log = fun _ -> Lwt.return_unit) ?(n = 5) ?(sleep = 1.) f =
  f ()
  >>= function
  | Ok r ->
      Lwt.return_ok r
  | Error error as x ->
      if n > 0 then
        log error
        >>= fun () ->
        Lwt_unix.sleep sleep >>= fun () -> retry ~log ~n:(n - 1) ~sleep f
      else Lwt.return x

type 'action io_error = {
  action : 'action;
  unix_code : Unix.error;
  caller : string;
  arg : string;
}

let with_open_file ~flags ?(perm = 0o640) filename task =
  Lwt.catch
    (fun () ->
      Lwt_unix.openfile filename flags perm >>= fun x -> Lwt.return (Ok x))
    (function
      | Unix.Unix_error (unix_code, caller, arg) ->
          Lwt.return (Error {action = `Open; unix_code; caller; arg})
      | exn ->
          raise exn)
  >>= function
  | Error _ as x ->
      Lwt.return x
  | Ok fd ->
      task fd
      >>= fun res ->
      Lwt.catch
        (fun () -> Lwt_unix.close fd >>= fun () -> return res)
        (function
          | Unix.Unix_error (unix_code, caller, arg) ->
              Lwt.return (Error {action = `Close; unix_code; caller; arg})
          | exn ->
              raise exn)

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
let with_atomic_open_out ?(overwrite = true) ?temp_dir filename f =
  let temp_file =
    Filename.temp_file ?temp_dir (Filename.basename filename) ".tmp"
  in
  with_open_out ~overwrite temp_file f
  >>=? fun res ->
  Lwt.catch
    (fun () ->
      Lwt_unix.rename temp_file filename >>= fun () -> Lwt.return (Ok res))
    (function
      | Unix.Unix_error (unix_code, caller, arg) ->
          Lwt.return (Error {action = `Rename; unix_code; caller; arg})
      | exn ->
          raise exn)
