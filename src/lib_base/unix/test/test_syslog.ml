(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Base, Unix
   Invocation:   dune exec src/lib_base/unix/test/main.exe
   Subject:      Check syslog primitives
*)

open Tezt
open Tezt_core.Base

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Syslog: check formatting function"
    ~tags:["unix"; "syslog"]
  @@ fun () ->
  let prefix_without_tag_len =
    (* e.g. : "<117>Apr 13 14:00:50 " *)
    21
  in
  let check_format tag msg expected =
    let formatted =
      Syslog.format_message ~tag ~facility:Console ~with_pid:false Notice msg
    in
    let formatted =
      String.sub formatted prefix_without_tag_len (String.length formatted - 21)
    in
    Check.(
      (tag ^ ": " ^ expected = formatted)
        string
        ~error_msg:"Expected %L, got %R"
        ~__LOC__)
  in
  check_format
    "test1"
    (String.make 1024 'a')
    (String.make (1024 - 31) 'a' ^ "...") ;
  check_format
    "test2"
    (String.make (1024 - 28) 'b')
    (String.make (1024 - 28) 'b') ;
  check_format "test3" "hello hello" "hello hello" ;
  check_format "test4" "" "" ;
  unit

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Syslog: send message through socket"
    ~tags:["unix"; "syslog"]
  @@ fun () ->
  let pid = Unix.getpid () in
  let path = Temp.file @@ Format.asprintf "test-syslog-%d.sock" pid in
  let main_socket = Lwt_unix.(socket PF_UNIX SOCK_RAW 0) in
  let* () = Lwt_unix.bind main_socket (ADDR_UNIX path) in
  let* logger = Syslog.create ~path ~tag:"test" Console in
  let msg = "hello user" in
  let expected_msg = "test: hello user" in
  let len = String.length expected_msg + 21 in
  let* () = Syslog.syslog logger Notice msg in
  let buf = Bytes.create len in
  let* () =
    Lwt.finalize
      (fun () -> Lwt_utils_unix.read_bytes ~len main_socket buf)
      (fun () -> Syslog.close logger)
  in
  let res = String.sub (Bytes.to_string buf) 21 (len - 21) in
  Check.((expected_msg = res) string ~error_msg:"Expected %L, got %R" ~__LOC__) ;
  unit
