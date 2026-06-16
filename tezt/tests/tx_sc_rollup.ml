(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2024 TriliTech <contact@trili.tech>                    *)
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
   Component:    Smart Optimistic Rollups: TX Kernel
   Requirements: make build-kernels
   Invocation:   dune exec tezt/tests/main.exe -- --file tx_sc_rollup.ml

   Tests in this file originate tx kernel rollup
   from tx_kernel.wasm file using reveal_installer and DAC.
*)

open Base
open Sc_rollup_helpers

let send_message ?(src = Constant.bootstrap2.alias) client msg =
  let* () = Client.Sc_rollup.send_message ~hooks ~src ~msg client in
  Client.bake_for_and_wait client

let assert_state_changed ?block sc_rollup_node prev_state_hash =
  let* state_hash =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_state_hash ?block ()
  in
  Check.(state_hash <> prev_state_hash)
    Check.string
    ~error_msg:"State hash has not changed (%L <> %R)" ;
  Lwt.return_unit

let assert_ticks_advanced ?block sc_rollup_node prev_ticks =
  let* ticks =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_total_ticks ?block ()
  in
  Check.(ticks > prev_ticks)
    Check.int
    ~error_msg:"Tick counter did not advance (%L > %R)" ;
  Lwt.return_unit

let rec bake_until cond client sc_rollup_node =
  let* stop = cond client in
  if stop then unit
  else
    let* () = Client.bake_for_and_wait client in
    let* current_level = Client.level client in
    let* _ =
      Sc_rollup_node.wait_for_level ~timeout:30. sc_rollup_node current_level
    in
    bake_until cond client sc_rollup_node

let setup_classic ~kind ~commitment_period ~challenge_window protocol =
  let* node, client = setup_l1 ~commitment_period ~challenge_window protocol in
  let bootstrap1_key = Constant.bootstrap1.alias in
  let sc_rollup_node =
    Sc_rollup_node.create
      Operator
      node
      ~base_dir:(Client.base_dir client)
      ~kind
      ~default_operator:bootstrap1_key
  in
  let* {boot_sector; _} =
    prepare_installer_kernel
      ~preimages_dir:
        (Filename.concat (Sc_rollup_node.data_dir sc_rollup_node) "wasm_2_0_0")
      Constant.WASM.tx_kernel
  in
  (* Initialise the sc rollup *)
  let* sc_rollup_address =
    Client.Sc_rollup.originate
      ~burn_cap:Tez.(of_int 9999999)
      ~alias:"rollup"
      ~src:bootstrap1_key
      ~kind:"wasm_2_0_0"
      ~boot_sector
      ~parameters_ty:"pair string (ticket string)"
      client
  in
  let* () = Client.bake_for_and_wait client in
  return (client, sc_rollup_node, sc_rollup_address, [])

let setup_bootstrap ~kind ~commitment_period ~challenge_window protocol =
  let sc_rollup_address = "sr163Lv22CdE8QagCwf48PWDTquk6isQwv57" in
  let* {
         bootstrap_smart_rollup = bootstrap_tx_kernel;
         smart_rollup_node_data_dir;
         smart_rollup_node_extra_args;
       } =
    setup_bootstrap_smart_rollup
      ~name:"tx_kernel"
      ~address:sc_rollup_address
      ~parameters_ty:"pair string (ticket string)"
      ~installee:Constant.WASM.tx_kernel
      ()
  in
  let bootstrap1_key = Constant.bootstrap1.alias in
  let* node, client =
    setup_l1
      ~bootstrap_smart_rollups:[bootstrap_tx_kernel]
      ~commitment_period
      ~challenge_window
      protocol
  in
  let sc_rollup_node =
    Sc_rollup_node.create
      Operator
      node
      ~data_dir:smart_rollup_node_data_dir
      ~base_dir:(Client.base_dir client)
      ~kind
      ~default_operator:bootstrap1_key
  in
  let* () = Client.bake_for_and_wait client in
  return
    (client, sc_rollup_node, sc_rollup_address, smart_rollup_node_extra_args)

let tx_kernel_e2e setup protocol =
  let open Tezt_tx_kernel in
  let commitment_period = 10 and challenge_window = 10 in
  let* client, sc_rollup_node, sc_rollup_address, node_args =
    setup ~commitment_period ~challenge_window protocol
  in

  (* Run the rollup node, ensure origination succeeds. *)
  let* () = Sc_rollup_node.run sc_rollup_node sc_rollup_address node_args in
  let* _level = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in
  (* Originate a contract that will mint and transfer tickets to the tx kernel. *)
  let* mint_and_deposit_contract =
    Tezt_tx_kernel.Contracts.prepare_mint_and_deposit_contract client protocol
  in
  let* _level = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in

  (* gen two tz1 accounts *)
  let pkh1, pk1, sk1 = Tezos_crypto.Signature.Ed25519.generate_key () in
  let pkh2, pk2, sk2 = Tezos_crypto.Signature.Ed25519.generate_key () in
  let ticket_content = "Hello, Ticket!" in
  let ticketer =
    Tezos_protocol_alpha.Protocol.Contract_hash.to_b58check
      mint_and_deposit_contract
  in

  (* Deposit *)
  let* () =
    Tezt_tx_kernel.Contracts.deposit_string_tickets
      ~hooks
      client
      ~mint_and_deposit_contract:ticketer
      ~sc_rollup_address
      ~destination_l2_addr:
        (Tezos_crypto.Signature.Ed25519.Public_key_hash.to_b58check pkh1)
      ~ticket_content
      ~amount:450
  in
  let* _level = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in

  (* Construct transfer *)
  let sc_rollup_hash =
    Tezos_crypto.Hashed.Smart_rollup_address.of_b58check_exn sc_rollup_address
  in
  let transfer_message =
    Transaction_batch.(
      empty
      |> add_transfer
           ~counter:0
           ~signer:(Public_key pk1)
           ~signer_secret_key:sk1
           ~destination:pkh2
           ~ticketer
           ~ticket_content
           ~amount:60
      |> add_transfer
           ~counter:0
           ~signer:(Public_key pk2)
           ~signer_secret_key:sk2
           ~destination:pkh1
           ~ticketer
           ~ticket_content
           ~amount:10
      |> make_encoded_batch ~wrap_with:(`External_message_frame sc_rollup_hash)
      |> hex_encode)
  in

  (* Send transfers *)
  let* prev_state_hash =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_state_hash ()
  in
  let* () = send_message client (sf "hex:[%S]" transfer_message) in

  let* _ = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in
  let* () = assert_state_changed sc_rollup_node prev_state_hash in

  (* After that pkh1 has 400 tickets, pkh2 has 50 tickets *)

  (**** Withdrawal part ****)
  (* originate ticket receiver contract *)
  let* receive_tickets_contract =
    Tezt_tx_kernel.Contracts.prepare_receive_withdrawn_tickets_contract
      client
      protocol
  in
  let* _level = Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node in
  (* pk withdraws part of his tickets, pk2 withdraws all of his tickets *)
  let withdraw_message =
    Transaction_batch.(
      empty
      |> add_withdraw
           ~counter:1
           ~signer:(Public_key pk1)
           ~signer_secret_key:sk1
           ~destination:receive_tickets_contract
           ~entrypoint:"receive_tickets"
           ~ticketer
           ~ticket_content
           ~amount:220
      |> add_withdraw
           ~counter:1
           ~signer:(Public_key pk2)
           ~signer_secret_key:sk2
           ~destination:receive_tickets_contract
           ~entrypoint:"receive_tickets"
           ~ticketer
           ~ticket_content
           ~amount:40
      |> make_encoded_batch ~wrap_with:(`External_message_frame sc_rollup_hash)
      |> hex_encode)
  in
  (* Send withdrawal *)
  let* prev_state_hash =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_state_hash ()
  in
  let* prev_ticks =
    Sc_rollup_node.RPC.call ~rpc_hooks sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_total_ticks ()
  in
  let* () = send_message client (sf "hex:[%S]" withdraw_message) in
  let* withdrawal_level =
    Sc_rollup_node.wait_sync ~timeout:30. sc_rollup_node
  in

  let* _, last_lcc_level =
    Sc_rollup_helpers.last_cemented_commitment_hash_with_level
      ~sc_rollup:sc_rollup_address
      client
  in
  let next_lcc_level = last_lcc_level + commitment_period in

  (* Bake until the next commitment is cemented, this commitment
     will include the withdrawal. *)
  let* () =
    bake_until
      (fun client ->
        let* _, lcc_level =
          Sc_rollup_helpers.last_cemented_commitment_hash_with_level
            ~sc_rollup:sc_rollup_address
            client
        in
        return (lcc_level = next_lcc_level))
      client
      sc_rollup_node
  in

  let block = string_of_int next_lcc_level in
  let* () = assert_state_changed ~block sc_rollup_node prev_state_hash in
  let* () = assert_ticks_advanced ~block sc_rollup_node prev_ticks in

  (* EXECUTE withdrawal *)
  let* outbox =
    Sc_rollup_node.RPC.call sc_rollup_node
    @@ Sc_rollup_rpc.get_global_block_outbox ~outbox_level:withdrawal_level ()
  in
  Log.info "Outbox is %s" @@ JSON.encode outbox ;
  let execute_outbox_proof ~message_index =
    let outbox_level = withdrawal_level in
    let* proof =
      Sc_rollup_node.RPC.call sc_rollup_node
      @@ Sc_rollup_rpc.outbox_proof_simple ~message_index ~outbox_level ()
    in
    match proof with
    | Some {commitment_hash; proof} ->
        let*! () =
          Client.Sc_rollup.execute_outbox_message
            ~hooks
            ~burn_cap:(Tez.of_int 10)
            ~rollup:sc_rollup_address
            ~src:Constant.bootstrap1.alias
            ~commitment_hash
            ~proof
            client
        in
        Client.bake_for_and_wait client
    | _ -> failwith "Unexpected error during proof generation"
  in
  let* () = execute_outbox_proof ~message_index:0 in
  let* () = execute_outbox_proof ~message_index:1 in
  unit

let register_test ?supports ?(regression = false) ~__FILE__ ~tags ?uses ~title f
    =
  let tags = "tx_sc_rollup" :: tags in
  if regression then
    Protocol.register_regression_test ?supports ~__FILE__ ~title ~tags ?uses f
  else Protocol.register_test ?supports ~__FILE__ ~title ~tags ?uses f

let test_tx_kernel_e2e =
  register_test
    ~regression:true
    ~__FILE__
    ~tags:["wasm"; "kernel"; "wasm_2_0_0"; "kernel_e2e"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.smart_rollup_installer;
        Constant.WASM.tx_kernel;
      ])
    ~title:(Printf.sprintf "wasm_2_0_0 - tx kernel should run e2e (kernel_e2e)")
    (tx_kernel_e2e (setup_classic ~kind:"wasm_2_0_0"))

let test_bootstrapped_tx_kernel_e2e =
  register_test
    ~supports:(Protocol.From_protocol 018)
    ~__FILE__
    ~tags:["wasm"; "kernel"; "wasm_2_0_0"; "kernel_e2e"; "bootstrap"]
    ~uses:(fun _protocol ->
      [
        Constant.octez_smart_rollup_node;
        Constant.smart_rollup_installer;
        Constant.WASM.tx_kernel;
      ])
    ~title:
      (Printf.sprintf
         "wasm_2_0_0 - bootstrapped tx kernel should run e2e (kernel_e2e)")
    (tx_kernel_e2e (setup_bootstrap ~kind:"wasm_2_0_0"))

let register ~protocols =
  test_tx_kernel_e2e protocols ;
  test_bootstrapped_tx_kernel_e2e protocols
