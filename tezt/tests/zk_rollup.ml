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
   Component:    ZK Rollup
   Invocation:   dune exec tezt/tests/main.exe -- --file zk_rollup.ml
   Subject:      Tests for the ZK Rollup client and protocol
*)

open Tezt_tezos

(* Number of operations in each private batch *)
let batch_size = 2

module Operator = Tezos_alpha_test_helpers.Dummy_zk_rollup.Operator (struct
  let batch_size = batch_size
end)

module Zk_rollup = Tezos_protocol_alpha.Protocol.Alpha_context.Zk_rollup

module MakeHelpers () = struct
  (* Subdirectory for auxiliary files *)
  let epoxy_dir = Temp.dir "epoxy"

  let with_epoxy_dir file_name = epoxy_dir ^ "/" ^ file_name

  let public_parameters_file = with_epoxy_dir "public_parameters"

  let () =
    let contents =
      Data_encoding.Binary.(
        to_string_exn
          Tezos_protocol_alpha.Environment.Plonk.public_parameters_encoding
          Operator.public_parameters)
    in
    write_file public_parameters_file ~contents

  let one_true_op_file = with_epoxy_dir "true_op"

  let one_false_op_file = with_epoxy_dir "false_op"

  let write_op_files true_op false_op =
    let to_string op =
      Data_encoding.(
        Binary.to_string_exn
          (list
          @@ tup2
               Zk_rollup.Operation.encoding
               (option Zk_rollup.Ticket.encoding)))
        [(op, None)]
    in
    write_file one_true_op_file ~contents:(to_string true_op) ;
    write_file one_false_op_file ~contents:(to_string false_op)

  let () =
    let contents =
      Data_encoding.(
        Binary.to_string_exn
          (list
          @@ tup2
               Zk_rollup.Operation.encoding
               (option Zk_rollup.Ticket.encoding)))
        [(Operator.Internal_for_tests.false_op, None)]
    in
    write_file one_false_op_file ~contents

  let init_state_file = with_epoxy_dir "init_state"

  let () =
    let contents =
      Data_encoding.Binary.(to_string_exn Zk_rollup.State.encoding)
        Operator.init_state
    in
    write_file init_state_file ~contents

  let of_plonk_smap s = Zk_rollup.Account.SMap.of_seq @@ Plonk.SMap.to_seq s

  let circuits_info_file = with_epoxy_dir "circuits_info"

  let () =
    let contents =
      Data_encoding.Binary.(
        to_string_exn
          Zk_rollup.Account.circuits_info_encoding
          (of_plonk_smap Operator.circuits))
    in
    write_file circuits_info_file ~contents

  let update_file = with_epoxy_dir "update"

  let write_update update =
    let contents =
      Data_encoding.Binary.(to_string_exn Zk_rollup.Update.encoding) update
    in
    write_file update_file ~contents

  let init protocol additional_bootstrap_account_count =
    let args =
      [
        (["zk_rollup_enable"], `Bool true);
        (["zk_rollup_min_pending_to_process"], `Int 3);
      ]
    in
    let* parameter_file =
      Protocol.write_parameter_file ~base:(Either.right (protocol, None)) args
    in
    let* node, client =
      Client.init_with_protocol
        ~nodes_args:[Synchronisation_threshold 0; Connections 0]
        ~additional_bootstrap_account_count
        `Client
        ~protocol
        ~parameter_file
        ()
    in
    Log.info "Activated protocol." ;
    let* () = Client.bake_for client in
    return (client, node)

  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4290
     Don't use raw RPCs, replace by actual ZKRU RPCs
  *)

  let rpc_pending_list client ~zk_rollup =
    let* client_output =
      Client.spawn_command
        client
        (["--wait"; "none"]
        @ [
            "rpc";
            "get";
            "/chains/main/blocks/head/context/raw/json/zk_rollup/" ^ zk_rollup
            ^ "/pending_operations";
          ])
      |> Process.check_and_read_stdout
    in
    Log.info "Pending list: %s" client_output ;
    Lwt.return_unit

  let rpc_pending_list_length client ~zk_rollup =
    let* client_output =
      Client.spawn_command
        client
        (["--wait"; "none"]
        @ [
            "rpc";
            "get";
            "/chains/main/blocks/head/context/raw/json/zk_rollup/" ^ zk_rollup
            ^ "/pending_list";
          ])
      |> Process.check_and_read_stdout
    in
    let acc = JSON.parse ~origin:"RPC response" client_output in
    let length =
      Option.value ~default:0
      @@ int_of_string_opt JSON.(encode (acc |-> "length"))
    in
    return length

  let rpc_state client ~zk_rollup =
    let* client_output =
      Client.spawn_command
        client
        (["--wait"; "none"]
        @ [
            "rpc";
            "get";
            "/chains/main/blocks/head/context/raw/json/zk_rollup/" ^ zk_rollup;
          ])
      |> Process.check_and_read_stdout
    in
    let acc = JSON.parse ~origin:"RPC response" client_output in
    let state = JSON.(encode (acc |-> "account" |-> "dynamic" |-> "state")) in
    Log.info "Rollup state: %s" state ;
    return state
end

let originate ?expect_failure ?(gas_cap = 10_000) ?(burn_cap = 10_000)
    ?(storage_limit = 10_000) client ~src ~alias ~public_parameters_file
    ~init_state_file ~circuits_info_file ~nb_ops =
  let* zk_rollup_opt =
    Client.Zk_rollup.originate
      ?expect_failure
      client
      ~src
      ~alias
      ~public_parameters_file
      ~init_state_file
      ~circuits_info_file
      ~nb_ops
      ~gas_cap
      ~burn_cap
      ~storage_limit
  in
  let* () = Client.bake_for_and_wait client in
  return zk_rollup_opt

let publish ?expect_failure ?(burn_cap = 10_000) ?(gas_cap = 10_000) client ~src
    ~zk_rollup ~ops_file =
  let* () =
    Client.Zk_rollup.publish
      ?expect_failure
      client
      ~src
      ~zk_rollup
      ~ops_file
      ~gas_cap
      ~burn_cap
  in
  let* () = Client.bake_for_and_wait client in
  Lwt.return_unit

let update ?expect_failure ?(gas_cap = 10_000) ?(burn_cap = 10_000) client ~src
    ~zk_rollup ~update_file =
  let* () =
    Client.Zk_rollup.update
      ?expect_failure
      client
      ~src
      ~zk_rollup
      ~update_file
      ~gas_cap
      ~burn_cap
  in
  let* () = Client.bake_for_and_wait client in
  Lwt.return_unit

(* This test will:
   - Originate a new zk rollup
   - Publish 5 operations
   - Submit an update that processes the first 4 of the published operations
*)
let successful_test =
  Protocol.register_test ~__FILE__ ~title:"successful_test" ~tags:["zk_rollup"]
  @@ fun protocol ->
  let open MakeHelpers () in
  let* c, _node = init protocol 1 in
  let* bootstrap1 = Client.show_address ~alias:"bootstrap1" c in
  let alias = "epx" in
  let* zk_rollup_opt =
    originate
      c
      ~src:bootstrap1.public_key_hash
      ~alias
      ~public_parameters_file
      ~init_state_file
      ~circuits_info_file
      ~nb_ops:1
  in
  let zk_rollup = Option.get zk_rollup_opt in
  let* pl_length = rpc_pending_list_length c ~zk_rollup in
  assert (pl_length = 0) ;
  let zkr = Zk_rollup.Address.of_b58check_exn zk_rollup in
  let true_op = Operator.{Internal_for_tests.true_op with rollup_id = zkr} in
  let false_op = Operator.{Internal_for_tests.false_op with rollup_id = zkr} in
  let () = write_op_files true_op false_op in
  let* () =
    publish
      c
      ~src:bootstrap1.public_key_hash
      ~zk_rollup
      ~ops_file:one_false_op_file
  in
  let* () =
    publish
      c
      ~src:bootstrap1.public_key_hash
      ~zk_rollup
      ~ops_file:one_true_op_file
  in
  let* () =
    publish
      c
      ~src:bootstrap1.public_key_hash
      ~zk_rollup
      ~ops_file:one_true_op_file
  in

  let* () =
    publish
      c
      ~src:bootstrap1.public_key_hash
      ~zk_rollup
      ~ops_file:one_false_op_file
  in

  let* () =
    publish
      c
      ~src:bootstrap1.public_key_hash
      ~zk_rollup
      ~ops_file:one_false_op_file
  in
  let* pl_length = rpc_pending_list_length c ~zk_rollup in
  assert (pl_length = 5) ;
  let* initial_state = rpc_state c ~zk_rollup in
  let* () = rpc_pending_list c ~zk_rollup in
  let _new_state, upd =
    Operator.(
      craft_update
        init_state
        ~zk_rollup:zkr
        ~private_ops:[]
        [false_op; true_op; true_op; false_op])
  in
  let () = write_update upd in
  let* () = update c ~src:bootstrap1.public_key_hash ~zk_rollup ~update_file in
  let* final_state = rpc_state c ~zk_rollup in
  assert (final_state <> initial_state) ;
  let* () = rpc_pending_list c ~zk_rollup in
  let* pl_length = rpc_pending_list_length c ~zk_rollup in
  assert (pl_length = 1) ;
  unit

let failing_origination =
  Protocol.register_test
    ~__FILE__
    ~title:"failing_origination"
    ~tags:["zk_rollup"]
  @@ fun protocol ->
  let open MakeHelpers () in
  let* c, _node = init protocol 1 in
  let* bootstrap1 = Client.show_address ~alias:"bootstrap1" c in
  let alias = "epx" in
  let* _zk_rollup =
    originate
      ~expect_failure:true
      c
      ~src:bootstrap1.public_key_hash
      ~alias
      ~public_parameters_file:update_file
        (* Invalid file for public_paramaters *)
      ~init_state_file
      ~circuits_info_file
      ~nb_ops:1
  in
  unit

let failing_publish =
  Protocol.register_test ~__FILE__ ~title:"failing_publish" ~tags:["zk_rollup"]
  @@ fun protocol ->
  let open MakeHelpers () in
  let* c, _node = init protocol 1 in
  let* bootstrap1 = Client.show_address ~alias:"bootstrap1" c in
  (* invalid zk_rollup address *)
  let zk_rollup = "" in
  let* () =
    publish
      ~expect_failure:true
      c
      ~src:bootstrap1.public_key_hash
      ~zk_rollup
      ~ops_file:one_false_op_file
  in
  unit

let failing_update =
  Protocol.register_test ~__FILE__ ~title:"failing_update" ~tags:["zk_rollup"]
  @@ fun protocol ->
  let open MakeHelpers () in
  let* c, _node = init protocol 1 in
  let* bootstrap1 = Client.show_address ~alias:"bootstrap1" c in
  (* invalid zk_rollup address *)
  let zk_rollup = "" in
  let* () =
    update
      ~expect_failure:true
      c
      ~src:bootstrap1.public_key_hash
      ~zk_rollup
      ~update_file
  in
  unit

let register ~protocols =
  successful_test protocols ;
  failing_origination protocols ;
  failing_publish protocols ;
  failing_update protocols
