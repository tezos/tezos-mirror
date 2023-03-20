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
    Invocation:   dune exec src/lib_mockup/test/main.exe
    Subject:      Unit tests of the Persistence library
*)

open Tezos_mockup
open Tezos_stdlib_unix
open Tezos_mockup_registration

let base_dir_class_testable =
  Alcotest.(testable Persistence.pp_base_dir_class ( = ))

let check_base_dir s bd1 bd2 = Alcotest.check base_dir_class_testable s bd1 bd2

(** [classify_base_dir] a non existing directory *)
let test_classify_does_not_exist =
  let open Lwt_result_syntax in
  Tztest.tztest "Classify a non existing directory" `Quick (fun () ->
      Lwt_utils_unix.with_tempdir "test_persistence" (fun base_dir ->
          let+ bd =
            Persistence.classify_base_dir
              (Filename.concat base_dir "non_existing_directory")
          in
          check_base_dir "A non existing directory" Base_dir_does_not_exist bd))

(** [classify_base_dir] a file *)
let test_classify_is_file =
  let open Lwt_result_syntax in
  Tztest.tztest "Classify a file" `Quick (fun () ->
      let tmp_file = Filename.temp_file "" "" in
      let+ bd = Persistence.classify_base_dir tmp_file in
      check_base_dir "A file" Base_dir_is_file bd)

(** [classify_base_dir] a mockup directory *)
let test_classify_is_mockup =
  let open Lwt_result_syntax in
  Tztest.tztest "Classify a mockup directory" `Quick (fun () ->
      Lwt_utils_unix.with_tempdir "test_persistence" (fun dirname ->
          let mockup_directory = (Files.get_mockup_directory ~dirname :> string)
          and mockup_file_name = Files.Context.get ~dirname in
          let*! () = Lwt_unix.mkdir mockup_directory 0o700 in
          let () = close_out (open_out (mockup_file_name :> string)) in
          let+ bd = Persistence.classify_base_dir dirname in
          check_base_dir "A mockup directory" Base_dir_is_mockup bd))

(** [classify_base_dir] a non empty directory *)
let test_classify_is_nonempty =
  let open Lwt_result_syntax in
  Tztest.tztest "Classify a non empty directory" `Quick (fun () ->
      Lwt_utils_unix.with_tempdir "test_persistence" (fun temp_dir ->
          let _ = Filename.temp_file ~temp_dir "" "" in
          let+ bd = Persistence.classify_base_dir temp_dir in
          check_base_dir "A non empty directory" Base_dir_is_nonempty bd))

(** [classify_base_dir] an empty directory *)
let test_classify_is_empty =
  let open Lwt_result_syntax in
  Tztest.tztest "Classify an empty directory" `Quick (fun () ->
      Lwt_utils_unix.with_tempdir "test_persistence" (fun base_dir ->
          let+ bd = Persistence.classify_base_dir base_dir in
          check_base_dir "An empty directory" Base_dir_is_empty bd))

module Mock_protocol : Registration.PROTOCOL = struct
  open Tezos_protocol_environment.Internal_for_tests
  include Environment_protocol_T_test.Mock_all_unit

  let hash = Protocol_hash.hash_string [""]
end

module Mock_mockup : Registration.MOCKUP = struct
  type parameters = unit

  type protocol_constants = unit

  let parameters_encoding = Data_encoding.unit

  let default_parameters = ()

  let protocol_constants_encoding = Data_encoding.unit

  let default_protocol_constants _ = assert false

  let default_bootstrap_accounts _ = assert false

  let protocol_hash = Mock_protocol.hash

  module Protocol = Mock_protocol
  module Block_services =
    Tezos_shell_services.Block_services.Make (Mock_protocol) (Mock_protocol)

  let directory = Tezos_rpc.Directory.empty

  let init ~cctxt:_ ~parameters:_ ~constants_overrides_json:_
      ~bootstrap_accounts_json:_ =
    assert false

  let migrate _ = assert false
end

let mock_mockup_module (protocol_hash' : Protocol_hash.t) :
    (module Registration.MOCKUP) =
  (module struct
    include Mock_mockup

    let protocol_hash = protocol_hash'
  end)

let mock_printer () =
  let rev_logs : string list ref = ref [] in
  object
    inherit
      Tezos_client_base.Client_context.simple_printer
        (fun _channel log ->
          rev_logs := log :: !rev_logs ;
          Lwt.return_unit)

    method get_logs = List.rev !rev_logs
  end

(** [get_registered_mockup] fails when no environment was registered. *)
let test_get_registered_mockup_no_env =
  let open Lwt_result_syntax in
  Tztest.tztest
    "get_registered_mockup fails when no environment was registered"
    `Quick
    (fun () ->
      let module Registration = Registration.Internal_for_tests.Make () in
      let module Persistence = Persistence.Internal_for_tests.Make (Registration) in
      let*! r = Persistence.get_registered_mockup None (mock_printer ()) in
      match r with
      | Ok _ -> Alcotest.fail "Should have failed"
      | Error ([_] as errors) ->
          let actual =
            Format.asprintf "%a" pp_print_top_error_of_trace errors
          in
          return
          @@ Alcotest.check'
               Alcotest.string
               ~msg:"The error message must be correct"
               ~expected:
                 "Default protocol Alpha (no requested protocol) not found in \
                  available mockup environments. Available protocol hashes: []"
               ~actual
      | Error _ -> Alcotest.fail "There should be exactly 1 error")

(** [get_registered_mockup] fails if the requested protocol is not found. *)
let test_get_registered_mockup_not_found =
  let open Lwt_result_syntax in
  Tztest.tztest
    "get_registered_mockup fails if the requested protocol is not found"
    `Quick
    (fun () ->
      let module Registration = Registration.Internal_for_tests.Make () in
      let module Persistence = Persistence.Internal_for_tests.Make (Registration) in
      let proto_hash_1 = Protocol_hash.hash_string ["mock1"] in
      let proto_hash_2 = Protocol_hash.hash_string ["mock2"] in
      let proto_hash_3 = Protocol_hash.hash_string ["mock3"] in
      Registration.register_mockup_environment (mock_mockup_module proto_hash_1) ;
      Registration.register_mockup_environment (mock_mockup_module proto_hash_2) ;
      let*! r =
        Persistence.get_registered_mockup (Some proto_hash_3) (mock_printer ())
      in
      match r with
      | Ok _ -> Alcotest.fail "Should have failed"
      | Error ([_] as errors) ->
          let actual =
            Format.asprintf "%a" pp_print_top_error_of_trace errors
          in
          let expected =
            Format.asprintf
              "Requested protocol with hash %a not found in available mockup \
               environments. Available protocol hashes: [%a, %a]"
              Protocol_hash.pp
              proto_hash_3
              Protocol_hash.pp
              proto_hash_2
              Protocol_hash.pp
              proto_hash_1
          in
          return
          @@ Alcotest.check'
               Alcotest.string
               ~msg:"The error message must be correct"
               ~expected
               ~actual
      | Error _ -> Alcotest.fail "There should be exactly 1 error")

(** [get_registered_mockup] returns Alpha if none is specified. *)
let test_get_registered_mockup_take_alpha =
  let open Lwt_result_syntax in
  Tztest.tztest
    "get_registered_mockup returns Alpha if none is specified"
    `Quick
    (fun () ->
      let module Registration = Registration.Internal_for_tests.Make () in
      let module Persistence = Persistence.Internal_for_tests.Make (Registration) in
      let printer = mock_printer () in
      let proto_hash_1 = Protocol_hash.hash_string ["mock1"] in
      let proto_hash_alpha =
        Protocol_hash.of_b58check_exn
          "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
      in
      let proto_hash_3 = Protocol_hash.hash_string ["mock3"] in
      Registration.register_mockup_environment (mock_mockup_module proto_hash_1) ;
      Registration.register_mockup_environment
        (mock_mockup_module proto_hash_alpha) ;
      Registration.register_mockup_environment (mock_mockup_module proto_hash_3) ;
      let+ (module Result) = Persistence.get_registered_mockup None printer in
      Alcotest.check'
        (Alcotest.testable Protocol_hash.pp Protocol_hash.equal)
        ~msg:"The Alpha protocol is returned"
        ~expected:proto_hash_alpha
        ~actual:Result.protocol_hash ;
      Alcotest.(
        check'
          (list string)
          ~msg:"Log must be correct"
          ~expected:["No protocol specified: using Alpha as default protocol."]
          ~actual:printer#get_logs))

(** [get_registered_mockup] returns the requested protocol. *)
let test_get_registered_mockup_take_requested =
  let open Lwt_result_syntax in
  Tztest.tztest
    "get_registered_mockup returns the requested protocol"
    `Quick
    (fun () ->
      let module Registration = Registration.Internal_for_tests.Make () in
      let module Persistence = Persistence.Internal_for_tests.Make (Registration) in
      let proto_hash_1 = Protocol_hash.hash_string ["mock1"] in
      let proto_hash_2 = Protocol_hash.hash_string ["mock2"] in
      Registration.register_mockup_environment (mock_mockup_module proto_hash_1) ;
      Registration.register_mockup_environment (mock_mockup_module proto_hash_2) ;
      let+ (module Result) =
        Persistence.get_registered_mockup (Some proto_hash_1) (mock_printer ())
      in
      Alcotest.check'
        (Alcotest.testable Protocol_hash.pp Protocol_hash.equal)
        ~msg:"The requested protocol is returned"
        ~expected:proto_hash_1
        ~actual:Result.protocol_hash)

let () =
  Alcotest_lwt.run
    "tezos-mockup"
    [
      ( "persistence",
        [
          test_classify_does_not_exist;
          test_classify_is_file;
          test_classify_is_mockup;
          test_classify_is_nonempty;
          test_classify_is_empty;
          test_get_registered_mockup_no_env;
          test_get_registered_mockup_not_found;
          test_get_registered_mockup_take_alpha;
          test_get_registered_mockup_take_requested;
        ] );
    ]
  |> Lwt_main.run
