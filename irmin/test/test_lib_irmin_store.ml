(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Irmin
    Invocation:   dune exec irmin/test/main.exe -- --file test_lib_irmin_store.ml
    Subject:      This file tests simple assertions about the store
*)

include Test_utils

let block2_actions =
  [
    Add ([ "a"; "b" ], Bytes.of_string "ab");
    Add ([ "a"; "c" ], Bytes.of_string "ac");
    Add ([ "version" ], Bytes.of_string "0.0");
  ]

let block3a_actions =
  [ Remove [ "a"; "b" ]; Add ([ "a"; "d" ], Bytes.of_string "ad") ]

let block3b_actions =
  [ Remove [ "a"; "c" ]; Add ([ "a"; "d" ], Bytes.of_string "ad") ]

type t = {
  index : Context.index;
  block2 : Context_hash.t;
  block3a : Context_hash.t;
  block3b : Context_hash.t;
}

let wrap_context_init f _ =
  let base_dir = Tezt.Temp.dir "tezos_test_" in
  let root = Filename.concat base_dir "context" in
  let* index = Context.init root in
  let*!! genesis =
    Context.commit_genesis index ~chain_id ~time:genesis_time
      ~protocol:genesis_protocol
  in
  let* block2 = create_block index genesis block2_actions in
  let* block3a = create_block index block2 block3a_actions in
  let* block3b = create_block index block2 block3b_actions in
  let* r = f { index; block2; block3a; block3b } in
  let* () = Context.close index in
  return r

let c = function None -> None | Some s -> Some (Bytes.to_string s)

(** Checkout the context applied until [block2]. It is asserted that
    the following key-values are present:
    - (["version"], ["0.0"])
    - (["a"; "b"], ["ab"])
    - (["a; "c""], ["ac"]) *)
let test_simple { index; block2; _ } =
  let* o = Context.checkout index block2 in
  match o with
  | None -> Assert.fail_msg "checkout block2"
  | Some ctxt ->
      let* version = Context.find ctxt [ "version" ] in
      let* tree = Context.find_tree ctxt [] in
      (match tree with
      | None -> assert false
      | Some tree -> Log.info "%a" Context.Tree.pp tree);
      Assert.String.Option.equal ~loc:__LOC__ (c version) (Some "0.0");
      let* ab = Context.find ctxt [ "a"; "b" ] in
      Assert.String.Option.equal (Some "ab") (c ab);
      let* ac = Context.find ctxt [ "a"; "c" ] in
      Assert.String.Option.equal ~loc:__LOC__ (Some "ac") (c ac);
      Lwt.return_unit

let tests : (string * (t -> unit Lwt.t)) list =
  let test name f = (Printf.sprintf "irmin_disk:%s" name, f) in
  [ test "simple" test_simple ]

let register_test title f =
  Tezt.Test.register ~__FILE__
    ~tags:[ Tag.layer1; "irmin"; "store"; Tag.flaky ]
    ~title
  @@ f

let register () =
  List.iter (fun (s, f) -> register_test s (wrap_context_init f)) tests
