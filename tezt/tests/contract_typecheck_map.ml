(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Michelson / Typechecking
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_typecheck_map.ml
   Subject:      Typechecking tests for the map type.
*)

let team = Tag.layer1

let tags = [team; "client"; "contract"; "michelson"; "typechecking"]

let test_normalize_legacy_flag =
  Protocol.register_test
    ~__FILE__
    ~title:"Test normalize with legacy flag"
    ~tags:[team; "client"; "normalize"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let data = "{Elt %a 0 1}" in
  let typ = "map nat nat" in
  let* () =
    let* _ = Client.typecheck_data client ~legacy:true ~data ~typ in
    unit
  in
  let* () =
    Client.spawn_typecheck_data client ~legacy:false ~data ~typ
    |> Process.check_error ~msg:(rex "unexpected annotation.")
  in
  unit

let register ~protocols = test_normalize_legacy_flag protocols
