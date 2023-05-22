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
   Component:    Michelson / Typechecking
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_typecheck_contract.ml
   Subject:      Typechecking tests for the address and (contract _) types.
*)

let tags = ["client"; "contract"; "michelson"; "typechecking"]

(** An address followed by an entrypoint typechecks at type address if 
  and only if the entrypoint is not "default". *)
let check_address client address =
  let address_a = sf {|"%s%%a"|} address in
  let* address_opt =
    Client.normalize_data
      client
      ~data:(sf {|"%s"|} address)
      ~typ:"address"
      ~mode:Optimized
    |> Lwt.map String.trim
  in
  let* address_opt_a =
    Client.normalize_data client ~data:address_a ~typ:"address" ~mode:Optimized
    |> Lwt.map String.trim
  in
  let* () =
    Client.typecheck_data client ~data:(sf {|"%s"|} address) ~typ:"address"
  in
  let* () = Client.typecheck_data client ~data:address_a ~typ:"address" in
  let* () = Client.typecheck_data client ~data:address_opt ~typ:"address" in
  let* () = Client.typecheck_data client ~data:address_opt_a ~typ:"address" in
  let unexpected_default_error = "unexpected_default_entrypoint" in
  let not_an_address_error = "not an expression of type address" in
  let* () =
    Client.spawn_typecheck_data
      client
      ~data:(sf {|"%s%%default"|} address)
      ~typ:"address"
    |> Process.check_error ~msg:(rex unexpected_default_error)
  in
  let* () =
    (* 64656661756c74 is "default" in hexa *)
    Client.spawn_typecheck_data
      client
      ~data:(sf {|%s64656661756c74|} address_opt)
      ~typ:"address"
    |> Process.check_error ~msg:(rex not_an_address_error)
  in
  unit

(** Helper to check that an address followed by an entrypoint typechecks
    at type (contract typ) using both readable and optimized
    representations. *)
let check_contract_ok client address entrypoint typ =
  let address_readable =
    match entrypoint with
    | Some entrypoint -> sf {|"%s%%%s"|} address entrypoint
    | None -> sf {|"%s"|} address
  in
  let* address_opt =
    Client.normalize_data
      client
      ~data:address_readable
      ~typ:"address"
      ~mode:Optimized
    |> Lwt.map String.trim
  in
  let* () =
    Client.typecheck_data
      client
      ~data:address_readable
      ~typ:(sf {|contract (%s)|} typ)
  in
  let* () =
    Client.typecheck_data
      client
      ~data:address_opt
      ~typ:(sf {|contract (%s)|} typ)
  in
  let* (_run_script_result : Client.run_script_result) =
    Client.run_script
      client
      ~prg:
        (sf
           {|
parameter unit;
storage address;
code {
        CDR;
        CONTRACT (%s);
        ASSERT_SOME;
        ADDRESS;
        NIL operation;
        PAIR }|}
           typ)
      ~storage:address_readable
      ~input:"Unit"
  in
  unit

(** Helper to check that an address followed by an entrypoint does not
    typecheck at type (contract typ) using both readable and optimised
    representations. *)
let check_contract_ko client address entrypoint typ expected_error =
  let address_readable =
    match entrypoint with
    | None -> sf {|"%s"|} address
    | Some entrypoint -> sf {|"%s%%%s"|} address entrypoint
  in
  let* address_opt =
    Client.normalize_data
      client
      ~data:address_readable
      ~typ:"address"
      ~mode:Optimized
    |> Lwt.map String.trim
  in
  let* () =
    Client.spawn_typecheck_data
      client
      ~data:address_readable
      ~typ:(sf "contract (%s)" typ)
    |> Process.check_error ~msg:(rex expected_error)
  in
  let* () =
    Client.spawn_typecheck_data
      client
      ~data:address_opt
      ~typ:(sf "contract (%s)" typ)
    |> Process.check_error ~msg:(rex expected_error)
  in
  let* (_run_script_result : Client.run_script_result) =
    Client.run_script
      client
      ~prg:
        (sf
           {|
parameter unit;
storage address;
code {
        CDR;
        DUP;
        CONTRACT (%s);
        ASSERT_NONE;
        NIL operation;
        PAIR }|}
           typ)
      ~storage:address_readable
      ~input:"Unit"
  in
  unit

(** An acceptable handle to an implicit account has two possibilities.
    - it is of type address if the entrypoint is not "default",
    - it is of type (contract <ty>) if the entrypoint is empty and ty is unit or ticket. *)
let test_implicit =
  Protocol.register_test ~__FILE__ ~title:"Test Implicit" ~tags
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let tz1 = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" in
  let* () = check_address client tz1 in
  let* () = check_contract_ok client tz1 None "unit" in
  let* () = check_contract_ok client tz1 None "ticket string" in
  let no_entrypoint_error = "Contract has no entrypoint named a" in
  let type_mismatch_error =
    match protocol with
    | Alpha ->
        "is not acceptable as a handle to an implicit account, whose \
         parameters type can only be unit or ticket <ty>"
    | Mumbai | Nairobi -> "Type nat is not compatible with type unit."
  in
  let* () =
    check_contract_ko client tz1 (Some "a") "unit" no_entrypoint_error
  in
  let* () = check_contract_ko client tz1 (Some "a") "nat" no_entrypoint_error in
  let* () = check_contract_ko client tz1 None "nat" type_mismatch_error in
  unit

(** The address of an inexistent originated account followed by some
    entrypoint typechecks:
    - at type address if the entrypoint is not "default",
    - at no (contract _) type. *)
let test_originated_inexistent =
  Protocol.register_test ~__FILE__ ~title:"Test Originated Inexistent" ~tags
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let kt1 = "KT1RvwLgpxVv9ANCKsDb5vBgTaZRG1W4bKWP" in
  let* () = check_address client kt1 in
  let invalid_contract_error = "invalid contract." in
  let* () = check_contract_ko client kt1 None "unit" invalid_contract_error in
  let* () =
    check_contract_ko client kt1 (Some "a") "unit" invalid_contract_error
  in
  let* () = check_contract_ko client kt1 None "nat" invalid_contract_error in
  let* () =
    check_contract_ko client kt1 (Some "a") "nat" invalid_contract_error
  in
  unit

(** The address of an existent originated account that does not specify
    a default entrypoint followed by some entrypoint typechecks:
    - at type address if the entrypoint is not "default",
    - at type (contract <ty>) if
      - the entrypoint is empty and <ty> is the root type
      - the entrypoint is non-empty, one of the declared entrypoints, and
        <ty> is the type associated to that entrypoint. *)
let test_originated_no_default =
  Protocol.register_test ~__FILE__ ~title:"Test originated no default" ~tags
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, kt1 =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~burn_cap:Tez.one
      client
      ["entrypoints"; "simple_entrypoints"]
      protocol
  in
  let root_type = {|or (unit %A) (or (string %B) (nat %C))|} in
  let a_type = "unit" in
  let b_type = "string" in
  let* () = check_address client kt1 in
  let* () = check_contract_ok client kt1 None root_type in
  let* () = check_contract_ok client kt1 (Some "A") a_type in
  let* () = check_contract_ok client kt1 (Some "B") b_type in
  let no_entrypoint_error = "Contract has no entrypoint named a" in
  let* () =
    check_contract_ko client kt1 (Some "a") a_type no_entrypoint_error
  in
  unit

(** The address of an existent orignated account that specifies a
    default entrypoint followed by some entrypoint typechecks:
    - at tpye address if the entrypoint is not "default",
    - at type (contract <ty>) if
      - the entrypoint is empty and <ty> is the type of the default
        entrypoint
      - the entrypoint is non-empty, one of the declared entrypoints,
        and <ty> is the type associated to that entrypoint. *)
let test_originated_with_default =
  Protocol.register_test ~__FILE__ ~title:"Test originated with default" ~tags
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let initial_storage = {|Pair "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" "" 0|} in
  let* _alias, kt1 =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:initial_storage
      ~burn_cap:Tez.one
      client
      ["entrypoints"; "delegatable_target"]
      protocol
  in
  let root_type =
    {|or (or (key_hash %set_delegate) (unit %remove_delegate)) (or %default string nat)|}
  in
  let default_type = "or string nat" in
  let* () = check_address client kt1 in
  let* () = check_contract_ok client kt1 None default_type in
  let* () = check_contract_ok client kt1 (Some "set_delegate") "key_hash" in
  let no_entrypoint_error = "Contract has no entrypoint named a" in
  let* () =
    check_contract_ko client kt1 (Some "a") root_type no_entrypoint_error
  in
  let type_mismatch_error = "is not compatible with type" in
  let* () = check_contract_ko client kt1 None root_type type_mismatch_error in
  unit

let register ~protocols =
  test_implicit protocols ;
  test_originated_inexistent protocols ;
  test_originated_no_default protocols ;
  test_originated_with_default protocols
