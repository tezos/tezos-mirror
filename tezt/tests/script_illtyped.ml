(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Michelson / Contracts
   Invocation:   dune exec tezt/tests/main.exe -- --file script_illtyped.ml
   Subject:      Test Michelson script typechecking failures
*)

let test_deprecated_typecheck script ~legacy =
  Protocol.register_test
    ~__FILE__
    ~title:
      (sf
         "Test Deprecated Typecheck %s - %s"
         (if legacy then "in Legacy" else "Breaks")
         (Michelson_script.name_s script))
    ~tags:["client"; "script"; "michelson"; "typechecking"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let ill_typed_scripts =
    [
      "legacy/create_contract";
      "legacy/create_contract_flags";
      "legacy/create_contract_rootname";
    ]
  in
  let script_path = Michelson_script.path script in
  let expected_error =
    if List.mem (Michelson_script.name_s script) ill_typed_scripts then
      sf "script %S is ill-typed" script_path
    else "Use of deprecated instruction"
  in
  Client.spawn_typecheck_script ~scripts:[script_path] ~legacy client
  |> Process.check_error ~msg:(rex expected_error)

let test_ill_typecheck script error_pattern =
  Protocol.register_test
    ~__FILE__
    ~title:(sf "Test Ill Typecheck - %s" script)
    ~tags:["client"; "script"; "michelson"; "typechecking"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let script_path =
    Michelson_script.(find ["ill_typed"; script] protocol |> path)
  in
  Client.spawn_typecheck_script ~scripts:[script_path] client
  |> Process.check_error ~msg:error_pattern

let test_legacy_typecheck protocols =
  [("timelock", Protocol.(Until_protocol (number Nairobi)))]
  |> List.iter @@ fun (script, supports) ->
     ( Protocol.register_test
         ~__FILE__
         ~supports
         ~title:
           (sf
              "Test deprecated instructions typecheck conditionally - %s"
              script)
         ~tags:["client"; "script"; "michelson"; "typechecking"]
         ~uses_node:false
     @@ fun protocol ->
       let* client = Client.init_mockup ~protocol () in
       let script_path =
         Michelson_script.(find ["ill_typed"; script] protocol |> path)
       in
       let* () =
         Client.spawn_typecheck_script
           ~scripts:[script_path]
           ~legacy:false
           client
         |> Process.check_error ~msg:(rex "Use of deprecated instruction")
       in
       Client.typecheck_script ~scripts:[script_path] ~legacy:true client )
       protocols

let register ~protocols =
  protocols
  |> List.iter (fun protocol ->
         Michelson_script.find_all_legacy protocol
         |> List.iter (fun michelson_script ->
                test_deprecated_typecheck
                  michelson_script
                  [protocol]
                  ~legacy:false ;
                test_deprecated_typecheck
                  michelson_script
                  [protocol]
                  ~legacy:true)) ;
  [
    (* Even though the interpreter uses a nonempty stack internally,
       the typechecker should not be able to observe it. *)
    ( "stack_bottom_unfailwithable",
      rex "wrong stack type for instruction FAILWITH" );
    ("stack_bottom_unrightable", rex "wrong stack type for instruction RIGHT");
    ("stack_bottom_unleftable", rex "wrong stack type for instruction LEFT");
    ("stack_bottom_ungetable", rex "wrong stack type for instruction GET");
    ("stack_bottom_unpairable", rex "wrong stack type for instruction UNPAIR");
    ("stack_bottom_undug2able", rex "wrong stack type for instruction DUG");
    ("stack_bottom_undugable", rex "wrong stack type for instruction DUG");
    ("stack_bottom_undig2able", rex "wrong stack type for instruction DIG");
    ("stack_bottom_undigable", rex "wrong stack type for instruction DIG");
    ("stack_bottom_undip2able", rex "wrong stack type for instruction DUP");
    ("stack_bottom_undipable", rex "wrong stack type for instruction DUP");
    ("stack_bottom_undup2able", rex "wrong stack type for instruction DUP");
    ("stack_bottom_undropable", rex "wrong stack type for instruction DROP");
    ("stack_bottom_unpopable", rex "wrong stack type for instruction DUP");
    ( "stack_bottom_unpopable_in_lambda",
      rex "wrong stack type for instruction DUP" );
    (* operations cannot be PACKed *)
    ( "pack_operation",
      rex "operation type forbidden in parameter, storage and constants" );
    (* big_maps cannot be PACKed *)
    ("pack_big_map", rex "big_map or sapling_state type not expected here");
    ("invalid_self_entrypoint", rex "Contract has no entrypoint named D");
    ("contract_annotation_default", rex "unexpected_default_entrypoint");
    (* Missing field *)
    ("missing_only_storage_field", rex "Missing contract field: storage");
    ("missing_only_code_field", rex "Missing contract field: code");
    ("missing_only_parameter_field", rex "Missing contract field: parameter");
    ( "missing_parameter_and_storage_fields",
      rex "Missing contract field: parameter" );
    (* Duplicated field *)
    ("multiple_parameter_field", rex "duplicate contract field: parameter");
    ("multiple_code_field", rex "duplicate contract field: code");
    ("multiple_storage_field", rex "duplicate contract field: storage");
    (* The first duplicated field is reported, storage in this case *)
    ("multiple_storage_and_code_fields", rex "duplicate contract field: storage");
    (* error message for set update on non-comparable type *)
    ( "set_update_non_comparable",
      rex "Type nat\\s+is not compatible with type list operation" );
    (* error message for the arity of the chain_id type *)
    ( "chain_id_arity",
      rex "primitive chain_id expects 0 arguments but is given 1" );
    (* error message for DIP over the limit *)
    ("big_dip", rex "expected a positive 10-bit integer");
    (* error message for DROP over the limit *)
    ("big_drop", rex "expected a positive 10-bit integer");
    (* error message for attempting to push a value of type never *)
    ("never_literal", rex "type never has no inhabitant.");
    (* COMB, UNCOMB, and DUP cannot take 0 as argument *)
    ("comb0", rex "PAIR expects an argument of at least 2");
    ("comb1", rex "PAIR expects an argument of at least 2");
    ("uncomb0", rex "UNPAIR expects an argument of at least 2");
    ("uncomb1", rex "UNPAIR expects an argument of at least 2");
    ("dup0", rex "DUP n expects an argument of at least 1");
    ( "push_big_map_with_id_with_parens",
      rex "big_map or sapling_state type not expected here" );
    ( "push_big_map_with_id_without_parens",
      rex "primitive PUSH expects 2 arguments but is given 4" );
    (* sapling_state is not packable *)
    ("pack_sapling_state", rex "big_map or sapling_state type not expected here");
    (* sapling_state is not packable *)
    ( "unpack_sapling_state",
      rex "big_map or sapling_state type not expected here" );
    (* Ticket duplication attempt *)
    ( "ticket_dup",
      rex "ticket nat cannot be used here because it is not duplicable" );
    (* error message for ticket unpack *)
    ("ticket_unpack", rex "Ticket in unauthorized position");
    (* error message for attempting to use APPLY to capture a ticket *)
    ("ticket_apply", rex "Ticket in unauthorized position");
    (* error message for attempting to wrap a ticket in a ticket *)
    ( "ticket_in_ticket",
      rex "comparable type expected.Type ticket unit is not comparable" );
    (* error message for DIP { FAILWITH } *)
    ("dip_failwith", rex "The FAIL instruction must appear in a tail position.");
    (* error message for MAP { FAILWITH } *)
    ( "map_failwith",
      rex "The proper type of the return list cannot be inferred." );
  ]
  |> List.iter (fun (script, error_pattern) ->
         test_ill_typecheck script error_pattern protocols) ;
  test_legacy_typecheck protocols
