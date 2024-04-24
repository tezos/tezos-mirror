(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2020-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    _______

    Component: Store
    Invocation: dune exec src/lib_store/unix/test/main.exe \
                -- --file test_protocol_store.ml
    Subject: Store tests ( protocol )
*)

open Test_utils

let test_protocol_store _ store =
  let open Lwt_syntax in
  let protocols_to_register =
    [
      ( Tezos_embedded_protocol_alpha.Registerer.Registered.hash,
        Tezos_embedded_protocol_alpha.Registerer.Source.sources );
      ( Tezos_embedded_protocol_genesis.Registerer.Registered.hash,
        Tezos_embedded_protocol_genesis.Registerer.Source.sources );
    ]
  in
  let non_stored_protocol_hash =
    Tezos_embedded_protocol_demo_noops.Registerer.Registered.hash
  in
  let* () =
    List.iter_s
      (fun (h, p) ->
        let* _ = Store.Protocol.store store h p in
        Lwt.return_unit)
      protocols_to_register
  in
  assert (
    List.for_all
      (fun (h, _p) -> Store.Protocol.mem store h)
      protocols_to_register) ;
  assert (not (Store.Protocol.mem store non_stored_protocol_hash)) ;
  let* () =
    List.iter_s
      (fun (h, p) ->
        let* o = Store.Protocol.read store h in
        match o with
        | None -> Lwt.fail Alcotest.Test_error
        | Some p' ->
            Alcotest.check (module Protocol) "protocol equality" p p' ;
            Lwt.return_unit)
      protocols_to_register
  in
  let* o = Store.Protocol.read store non_stored_protocol_hash in
  match o with None -> return_ok_unit | Some _ -> Lwt.fail Alcotest.Test_error

let tests =
  let test_cases =
    List.map wrap_test [("generic protocol storing", test_protocol_store)]
  in
  ("protocol store", test_cases)

let () = Lwt_main.run (Alcotest_lwt.run ~__FILE__ "tezos-store" [tests])
