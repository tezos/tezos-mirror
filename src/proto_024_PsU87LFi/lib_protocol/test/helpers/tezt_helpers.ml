(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Test registration *)

(** Registers a test. The protocol's name is added to the title and
    tags. File-specific title prefix and tags can also be specified. *)
let register_test ~__FILE__ ?(file_tags = []) ~title ?(additional_tags = [])
    ?(slow = false) f =
  let tags =
    let tags = file_tags @ additional_tags in
    if slow then Tezos_test_helpers.Tag.slow :: tags else tags
  in
  Tezt_tezos.Protocol.register_test
    ~__FILE__
    ~title
    ~tags
    ~uses:(fun _ -> [])
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    (fun _protocol -> f ())
    [T024]

(** Same as [register_test], but for a test function returning [unit
    tzresult Lwt.t]. If the result is an error, the test fails. *)
let register_test_es ~__FILE__ ?file_tags ~title ?additional_tags ?slow f =
  register_test ~__FILE__ ?file_tags ~title ?additional_tags ?slow @@ fun () ->
  let* r = f () in
  match r with
  | Ok () -> Lwt.return_unit
  | Error err ->
      let* () = Tezos_base_unix.Internal_event_unix.close () in
      Test.fail "@\n%a@." pp_print_trace err
