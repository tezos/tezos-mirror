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

(** Testing
    -------
    Component:    Persistence library
    Invocation:   dune build @runtest_persistence
    Subject:      Unit tests of the Persistence library
*)

open Tezos_mockup
open Tezos_stdlib_unix
open Tezos_test_services

let base_dir_class_testable =
  Alcotest.(testable Persistence.pp_base_dir_class ( = ))

let check_base_dir s bd1 bd2 = Alcotest.check base_dir_class_testable s bd1 bd2

(** [classify_base_dir] a non existing directory *)
let test_classify_does_not_exist =
  Test_services.tztest "Classify a non existing directory" `Quick (fun () ->
      Lwt_utils_unix.with_tempdir "test_persistence" (fun base_dir ->
          Persistence.classify_base_dir
            (Filename.concat base_dir "non_existing_directory")
          >|=? check_base_dir
                 "A non existing directory"
                 Base_dir_does_not_exist))

(** [classify_base_dir] a file *)
let test_classify_is_file =
  Test_services.tztest "Classify a file" `Quick (fun () ->
      let tmp_file = Filename.temp_file "" "" in
      Persistence.classify_base_dir tmp_file
      >|=? check_base_dir "A file" Base_dir_is_file)

(** [classify_base_dir] a mockup directory *)
let test_classify_is_mockup =
  Test_services.tztest "Classify a mockup directory" `Quick (fun () ->
      Lwt_utils_unix.with_tempdir "test_persistence" (fun dirname ->
          let mockup_directory = (Files.get_mockup_directory ~dirname :> string)
          and mockup_file_name = Files.Context.get ~dirname in
          Lwt_unix.mkdir mockup_directory 0o700
          >>= fun () ->
          let () = close_out (open_out (mockup_file_name :> string)) in
          Persistence.classify_base_dir dirname
          >|=? check_base_dir "A mockup directory" Base_dir_is_mockup))

(** [classify_base_dir] a non empty directory *)
let test_classify_is_nonempty =
  Test_services.tztest "Classify a non empty directory" `Quick (fun () ->
      Lwt_utils_unix.with_tempdir "test_persistence" (fun temp_dir ->
          let _ = Filename.temp_file ~temp_dir "" "" in
          Persistence.classify_base_dir temp_dir
          >|=? check_base_dir "A non empty directory" Base_dir_is_nonempty))

(** [classify_base_dir] an empty directory *)
let test_classify_is_empty =
  Test_services.tztest "Classify an empty directory" `Quick (fun () ->
      Lwt_utils_unix.with_tempdir "test_persistence" (fun base_dir ->
          Persistence.classify_base_dir base_dir
          >|=? check_base_dir "An empty directory" Base_dir_is_empty))

let () =
  Alcotest_lwt.run
    "tezos-mockup"
    [ ( "persistence",
        [ test_classify_does_not_exist;
          test_classify_is_file;
          test_classify_is_mockup;
          test_classify_is_nonempty;
          test_classify_is_empty ] ) ]
  |> Lwt_main.run
