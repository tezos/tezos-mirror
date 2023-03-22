(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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
    Component:    Shell
    Invocation:   dune exec src/lib_shell/test/main.exe
    Subject:      Unit tests for protocol_validator. Currently only tests that
                  events are emitted.
*)

open Shell_test_helpers

(** A [Alcotest_protocol_validator] with protocol
    validator-specific testables and helpers *)
module Alcotest_protocol_validator = struct
  module type HASHEQ = sig
    type t

    val eq : t -> t -> bool

    val pp : Format.formatter -> t -> unit
  end

  (* type hasheq = *)
  (* let from_hasheq (module ) *)

  let registered_protocol : Registered_protocol.t Alcotest.testable =
    let open Registered_protocol in
    let eq (p1 : t) (p2 : t) : bool =
      let (module P1) = p1 in
      let (module P2) = p2 in
      Protocol_hash.equal P1.hash P2.hash
    in
    let pp fmt (p : t) =
      let (module P) = p in
      Protocol_hash.pp fmt P.hash
    in
    Alcotest.testable pp eq
end

let section = Some (Internal_event.Section.make_sanitized ["node"; "validator"])

let filter = Some section

(** [wrap f _switch] wraps a test function [f] by setting up a Mock_sink if
    necessary, initializing a mock p2p network, an empty chain state and a
    validator. It passes the validator to the test function [f]. *)
let wrap f _switch () =
  Tztest.with_empty_mock_sink (fun _ ->
      Lwt_utils_unix.with_tempdir "tezos_test_" (fun test_dir ->
          let open Lwt_syntax in
          let* store = init_chain test_dir in
          let* r = init_mock_p2p Distributed_db_version.Name.zero in
          match r with
          | Ok p2p ->
              (* Create state *)
              let db = Distributed_db.create store p2p in
              (* Set working dir for protocol compiler *)
              Updater.init (Filename.concat test_dir "build") ;
              (* Start validator *)
              let vl = Protocol_validator.create db in
              f vl _switch ()
          | Error error ->
              Format.printf "Could not get p2p:\n   %a\n" pp_print_trace error ;
              Format.print_flush () ;
              Lwt.return_unit))

(** Start tests *)

(** Requesting the validation of a protocol emits a
    pushing_validation_request event. *)
let test_pushing_validator_protocol vl _switch () =
  (* Let's validate a phony protocol *)
  let open Lwt_syntax in
  let pt = Protocol.{expected_env = V0; components = []} in
  let* res = Protocol_validator.validate vl Protocol_hash.zero pt in
  Alcotest.(
    check (Tztestable.tzresults Alcotest_protocol_validator.registered_protocol))
    "Compilation should fail."
    res
    (Error
       [
         Validation_errors.Invalid_protocol
           {hash = Protocol_hash.zero; error = Compilation_failed};
       ]) ;
  Mock_sink.(
    assert_has_event
      "Should have a pushing_validation_request event"
      ?filter
      Pattern.
        {
          level = Some Internal_event.Debug;
          section = Some section;
          name = "pushing_protocol_validation";
        }) ;
  Lwt.return_unit

(** [previously_validated_protocol] tests that requesting the
    validation of a protocol that is already validated (e.g. the
    genesis protocol) emits a previously_validated_protocol event. *)
let test_previously_validated_protocol vl _switch () =
  (* Let's request the re-validation of the genesis protocol *)
  let open Lwt_syntax in
  let phony_pt = Protocol.{expected_env = V0; components = []} in
  let* res = Protocol_validator.validate vl genesis_protocol_hash phony_pt in
  Alcotest.check
    (Tztestable.tzresults Alcotest_protocol_validator.registered_protocol)
    "Compilation should work."
    (Ok genesis_protocol)
    res ;
  Mock_sink.(
    assert_has_event
      "Should have a previously_validated_protocol event"
      ?filter
      Pattern.
        {
          level = Some Internal_event.Debug;
          section = Some section;
          name = "previously_validated_protocol";
        }) ;
  Lwt.return_unit

(** [fetching_protocol] tests that requesting the fetch of a protocol
    emits a fetching_protocol event. *)
let test_fetching_protocol vl _switch () =
  (* Let's fetch a phony protocol, and timeout immediately *)
  let open Lwt_syntax in
  let* _ =
    Protocol_validator.fetch_and_compile_protocol
      ~peer:P2p_peer.Id.zero
      ~timeout:Ptime.Span.zero
      vl
      Protocol_hash.zero
  in
  Mock_sink.(
    assert_has_event
      "Should have a fetching_protocol event"
      ?filter
      Pattern.
        {
          level = Some Internal_event.Notice;
          section = Some section;
          name = "fetching_protocol";
        }) ;
  Lwt.return_unit

let tests =
  [
    Alcotest_lwt.test_case
      "pushing_validator_protocol"
      `Quick
      (wrap test_pushing_validator_protocol);
    Alcotest_lwt.test_case
      "previously_validated_protocol"
      `Quick
      (wrap test_previously_validated_protocol);
    Alcotest_lwt.test_case
      "fetching_protocol"
      `Quick
      (wrap test_fetching_protocol);
  ]
