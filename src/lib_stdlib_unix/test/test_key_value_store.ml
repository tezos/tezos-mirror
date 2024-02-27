(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
   Component:    Key-value store
   Invocation:   dune exec src/lib_stdlib_unix/test/main.exe \
                  -- --file test_key_value_store.ml
   Subject:      Test the key-value store
*)

open Error_monad

let value_size = 1

let test_lockfile_unique_process () =
  let open Lwt_syntax in
  let dir_path =
    Format.asprintf "key-value-store-test-key-1-%d" (Unix.getpid ())
    |> Filename.concat (Filename.get_temp_dir_name ())
  in
  let root_dir = dir_path in
  let* r1 = Key_value_store.init ~lru_size:10 ~root_dir in
  let kws1 =
    match r1 with
    | Ok kws1 -> kws1
    | Error errs ->
        Test.fail "Unable to initialize key value store: %a" pp_print_trace errs
  in
  let* r2 = Key_value_store.init ~lru_size:10 ~root_dir in
  let kws2 =
    match r2 with
    | Ok kws2 -> kws2
    | Error errs ->
        Test.fail
          "Unable to initialize key value store2 : %a"
          pp_print_trace
          errs
  in
  let* _ = Key_value_store.close kws1 in
  let* _ = Key_value_store.close kws2 in
  Lwt.return_unit

let test_lockfile_unique_process_double_close () =
  let open Lwt_syntax in
  let dir_path =
    Format.asprintf "key-value-store-test-key-2-%d" (Unix.getpid ())
    |> Filename.concat (Filename.get_temp_dir_name ())
  in
  let root_dir = dir_path in
  let* r = Key_value_store.init ~lru_size:10 ~root_dir in
  match r with
  | Ok kvs -> (
      let* r =
        let open Lwt_result_syntax in
        let* () = Key_value_store.close kvs in
        let* () = Key_value_store.close kvs in
        return_unit
      in
      match r with
      | Ok () -> Lwt.return_unit
      | Error errs ->
          Test.fail "Close is not idempotent: %a" pp_print_trace errs)
  | Error _ -> Test.fail "Failed to initialize key value store"

let child ppid count root_dir chan_p2c_in chan_c2p_out =
  let open Lwt_result_syntax in
  Format.eprintf
    "Child[%d-%d]: trying to initialize store %s\n%!"
    ppid
    count
    root_dir ;
  let*! r = Key_value_store.init ~lru_size:10 ~root_dir in
  let* () =
    match r with
    | Ok kws -> (
        let*! () = Lwt_io.write_line chan_c2p_out "Initialized" in
        let*! string = Lwt_io.read_line chan_p2c_in in
        match string with
        | "Bye" ->
            Format.eprintf
              "Child[%d-%d]: got bye, releasing store\n%!"
              ppid
              count ;
            let* () = Key_value_store.close kws in
            return_unit
        | msg ->
            let msg =
              String.escaped
              @@ Format.asprintf "Error: Unexpected message: %s" msg
            in
            let*! () = Lwt_io.write_line chan_c2p_out msg in
            return_unit)
    | Error errs ->
        let msg =
          String.escaped
          @@ Format.asprintf
               "Child[%d-%d] : Cannot initialize store: %a"
               ppid
               count
               pp_print_trace
               errs
        in
        let*! () = Lwt_io.write_line chan_c2p_out msg in
        return_unit
  in
  return_unit

let count = ref 0

let test_lockfile_multiple_process () =
  let ppid = Unix.getpid () in
  let root_dir = Format.asprintf "key-value-store-test-key-%d-3" ppid in
  Lwt_utils_unix.with_tempdir root_dir @@ fun root_dir ->
  incr count ;
  let chan_c2p_in, chan_c2p_out = Lwt_unix.pipe ~cloexec:true () in
  let chan_p2c_in, chan_p2c_out = Lwt_unix.pipe ~cloexec:true () in
  let chan_c2p_in = Lwt_io.of_fd ~mode:Input chan_c2p_in in
  let chan_p2c_in = Lwt_io.of_fd ~mode:Input chan_p2c_in in
  let chan_c2p_out = Lwt_io.of_fd ~mode:Output chan_c2p_out in
  let chan_p2c_out = Lwt_io.of_fd ~mode:Output chan_p2c_out in
  let open Lwt_syntax in
  let* () =
    match Lwt_unix.fork () with
    | -1 -> Test.fail "Failed to fork"
    | 0 ->
        let* _ = child ppid !count root_dir chan_p2c_in chan_c2p_out in
        Format.eprintf "Child[%d-%d] Exit\n%!" ppid !count ;
        exit 0
    | _cpid -> (
        Format.eprintf
          "Parent[%d-%d]: waiting for child to be ready\n%!"
          ppid
          !count ;
        let* string = Lwt_io.read_line chan_c2p_in in
        Format.eprintf "Parent[%d-%d]: got '%s'\n%!" ppid !count string ;
        match string with
        | "Initialized" -> (
            Format.eprintf "Parent[%d-%d]: initializing...\n%!" ppid !count ;
            let* r1 = Key_value_store.init ~lru_size:10 ~root_dir in
            match r1 with
            | Ok kws ->
                let* () = Lwt_io.write_line chan_p2c_out "Bye" in
                let* _ = Key_value_store.close kws in
                let* _ = Lwt_unix.waitpid [] _cpid in
                Test.fail
                  "Unexpected success: should not be able to open the store"
            | Error [Lwt_utils_unix.Io_error {action = `Lock; _}] ->
                let* () = Lwt_io.write_line chan_p2c_out "Bye" in
                let* _ = Lwt_unix.waitpid [] _cpid in
                Lwt.return_unit
            | Error errs ->
                let* () = Lwt_io.write_line chan_p2c_out "Bye" in
                let* _ = Lwt_unix.waitpid [] _cpid in
                Test.fail
                  "Unexpected failure: should fail with \
                   Could_not_acquire_lockfile, got %a"
                  pp_print_trace
                  errs)
        | msg ->
            let* () = Lwt_io.write_line chan_p2c_out "Bye" in
            let* _ = Lwt_unix.waitpid [] _cpid in
            Test.fail "Unexpected message from child: %s" msg)
  in
  let* () = Lwt_io.close chan_c2p_in in
  let* () = Lwt_io.close chan_c2p_out in
  let* () = Lwt_io.close chan_p2c_in in
  let* () = Lwt_io.close chan_p2c_out in
  Lwt.return_unit

let () =
  ( Tezt_core.Test.register
      ~__FILE__
      ~title:"test_kws_lockfile_unique_process"
      ~tags:["kws"]
  @@ fun () -> test_lockfile_unique_process () ) ;
  ( Tezt_core.Test.register
      ~__FILE__
      ~title:"test_kws_lockfile_unique_process_double_close"
      ~tags:["kws"]
  @@ fun () -> test_lockfile_unique_process_double_close () ) ;
  Tezt_core.Test.register
    ~__FILE__
    ~title:"test_kws_lockfile_multiple_process"
    ~tags:["kws"]
  @@ fun () -> test_lockfile_multiple_process ()
