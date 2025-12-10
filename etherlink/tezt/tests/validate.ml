(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024-2025 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

open Rpc.Syntax
open Test_helpers

type transaction_type = Legacy | Eip1559 | Eip2930

let tag = function
  | Legacy -> "Legacy"
  | Eip1559 -> "Eip1559"
  | Eip2930 -> "Eip2930"

let register ?maximum_gas_per_transaction ?set_account_code ?da_fee_per_byte
    ?(kernels = [Kernel.Latest; Mainnet]) ?minimum_base_fee_per_gas ~title ~tags
    f tx_types =
  List.iter
    (fun kernel ->
      List.iter
        (fun tx_type ->
          let tag_tx = tag tx_type in
          let tag_kernel, use_kernel = Kernel.to_uses_and_tags kernel in
          let title = Format.sprintf "%s: %s (%s)" tag_tx title tag_kernel in
          Test.register
            ~__FILE__
            ~title
            ~tags:(String.lowercase_ascii tag_tx :: tag_kernel :: tags)
            ~uses_admin_client:false
            ~uses_client:false
            ~uses_node:false
            ~uses:
              [
                Constant.octez_evm_node;
                use_kernel;
                Constant.smart_rollup_installer;
              ]
          @@ fun () ->
          let patch_config =
            Evm_node.patch_config_with_experimental_feature ()
          in
          let* sequencer =
            init_sequencer_sandbox
              ?maximum_gas_per_transaction
              ?set_account_code
              ?da_fee_per_byte
              ?minimum_base_fee_per_gas
              ~kernel:use_kernel
              ~patch_config
              ()
          in
          f kernel sequencer tx_type)
        tx_types)
    kernels

let send_transaction_and_fail_upon_sequencer_validation ~raw_tx sequencer
    ~expected_error ~error_msg =
  let wait_for_add = Evm_node.wait_for_tx_queue_add_transaction sequencer in
  let wait_for_error =
    Evm_node.wait_for_block_producer_rejected_transaction sequencer
  in
  let*@ _ = Rpc.send_raw_transaction ~raw_tx sequencer in
  let* _ = wait_for_add in
  let*@ _ = produce_block sequencer in
  let* error = wait_for_error in
  Check.(error =~ rex expected_error) ~error_msg ;
  unit

let send_transaction_and_wait_confirmation ~raw_tx sequencer =
  let wait_for_add = Evm_node.wait_for_tx_queue_add_transaction sequencer in
  let* hash =
    let*@ hash = Rpc.send_raw_transaction ~raw_tx sequencer in
    return hash
  and* _ = wait_for_add in
  let wait_for_confirmed =
    Evm_node.wait_for_tx_queue_transaction_confirmed ~hash sequencer
  in
  let* () =
    let*@ _ = produce_block sequencer in
    unit
  and* _hash = wait_for_confirmed in
  return hash

let test_validate_compressed_sig =
  register ~title:"Validate compressed signature" ~tags:["signature"; "caller"]
  @@ fun _kernel sequencer _tx_type ->
  let account =
    Eth_account.
      {
        address = "0xA257edC8ad1D8f8f463aC0D947cc381000b3c863";
        private_key =
          "0xb80e5dd2ba9281e482589973600609bb0f10f6a075e6c733e4472d4dd2df238a";
      }
  in
  (* Gives funds to the new account. *)
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:account.address
         ~value:(Wei.of_eth_int 10)
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in
  let* base_fee_per_gas = Rpc.get_gas_price sequencer in
  let base_fee_per_gas = Int32.to_int base_fee_per_gas in
  (* We have noticed that this transaction produces a compressed signature.
     We add this test as a non-regression test to make sure we handle
     compressed signature. *)
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:account.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:base_fee_per_gas
      ~gas:100_000
      ~address:account.address
      ~value:Wei.zero
      ()
  in
  let*@ transaction_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@! transaction_object =
    Rpc.get_transaction_by_hash ~transaction_hash sequencer
  in
  (* 64 because, 31 bytes in hexa (so 62), and 2 extra bytes for "0x". *)
  Check.((String.length transaction_object.s = 64) int)
    ~error_msg:"Signature.S was supposed to be compressed" ;
  Check.(
    (String.lowercase_ascii transaction_object.from
    = String.lowercase_ascii account.address)
      string)
    ~error_msg:"Expected caller to be %R but got %L" ;

  unit

let test_validate_recover_caller =
  register ~title:"Validate recovers caller" ~tags:["caller"]
  @@ fun _kernel sequencer tx_type ->
  let source = Eth_account.bootstrap_accounts.(0) in
  let make_raw_tx ~legacy =
    Cast.craft_tx
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~legacy
      ~gas:30_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  let* raw_tx =
    match tx_type with
    | Legacy -> make_raw_tx ~legacy:true
    | Eip2930 ->
        return
          "01f86f82053980843b9aca00825b0494b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a764000080c080a04cda4f1de731980d8f9bb4653f5bdf82cbacb342af15f512e925cb5e7bf44b9ba020874590087c9c2e481f5beb3abb96a9e2ce2712e064f9443760921d7e90d9eb"
    | Eip1559 -> make_raw_tx ~legacy:false
  in
  let*@ transaction_hash = Rpc.send_raw_transaction ~raw_tx sequencer in
  let*@! transaction_object =
    Rpc.get_transaction_by_hash ~transaction_hash sequencer
  in
  Check.(
    (String.lowercase_ascii transaction_object.from
    = String.lowercase_ascii source.address)
      string)
    ~error_msg:"Expected caller to be %R but got %L" ;
  unit

let test_validate_chain_id =
  register ~title:"Validate chain id" ~tags:["chain_id"]
  @@ fun _kernel sequencer tx_type ->
  let source = Eth_account.bootstrap_accounts.(0) in
  let make_tx_chain_id ~chain_id ~legacy =
    Cast.craft_tx
      ~legacy
      ~source_private_key:source.private_key
      ~chain_id
      ~nonce:0
      ~gas_price:1_000_000_000
      ~gas:30_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  let* invalid_chain_id =
    match tx_type with
    | Legacy -> make_tx_chain_id ~chain_id:1000 ~legacy:true
    | Eip2930 ->
        return
          "0x01f8678205dc80843b9aca008261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c080a063db2a9f77795acaf9fa62465addfd5d8e77bf564868c6762e16f3ae0c084d20a02a57aaaff92cfdde10dfa1016127ff2a94df0bf620615b50e660e76e6044a844"
    | Eip1559 -> make_tx_chain_id ~chain_id:1000 ~legacy:false
  in
  let*@? err = Rpc.send_raw_transaction ~raw_tx:invalid_chain_id sequencer in
  Check.((err.message = "Invalid chain id") string)
    ~error_msg:"the transaction has an invalid chain id, it should fail" ;

  let* valid_chain_id =
    match tx_type with
    | Legacy -> make_tx_chain_id ~chain_id:1337 ~legacy:true
    | Eip2930 ->
        return
          "0x01f86782053980843b9aca008261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c001a05ba2acb79e66aaadd076b1ee6fdf09f5cb95541e2b24504209f89908b50a84dea0492d8cf5c29b5ec44b4730eebfb62ddf5ce486d5b8729c50a8002d6219f76412"
    | Eip1559 -> make_tx_chain_id ~chain_id:1337 ~legacy:false
  in
  let* _hash =
    send_transaction_and_wait_confirmation ~raw_tx:valid_chain_id sequencer
  in

  unit

let test_validate_nonce =
  register ~title:"Validate nonce" ~tags:["nonce"]
  @@ fun _kernel sequencer tx_type ->
  (* Send one transaction so the nonce is 1. *)
  let source = Eth_account.bootstrap_accounts.(0) in
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:source.private_key
         ~to_public_key:"0xE7f682c226d7269C7247b878B3F94c7a8d31FEf5"
         ~value:Wei.zero
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in
  let make_tx_nonce ~nonce ~legacy =
    Cast.craft_tx
      ~legacy
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce
      ~gas_price:1_000_000_000
      ~gas:30_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  (* Nonce 10 is in the future, that's valid. *)
  let* nonce_10 =
    match tx_type with
    | Legacy -> make_tx_nonce ~nonce:10 ~legacy:true
    | Eip2930 ->
        return
          "0x01f8678205390a843b9aca008261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c080a06c58350ee42312a79bfc488992cf3aaa56eecad645797b00ffb708f9ea540e88a0438a70e53e970b53f8ff6444132053296c3da19e877b6216b6c7c5834f52d7da"
    | Eip1559 -> make_tx_nonce ~nonce:10 ~legacy:false
  in
  let*@ _ok = Rpc.send_raw_transaction ~raw_tx:nonce_10 sequencer in
  (* Nonce 1 is expected nonce, that's valid. *)
  let* nonce_1 =
    match tx_type with
    | Legacy -> make_tx_nonce ~nonce:1 ~legacy:true
    | Eip2930 ->
        return
          "0x01f86782053901843b9aca008261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c080a0e4ea03b849ac0e339adde760cf024ffbc5e4e70d601ca3d597151f45ef0fe8daa057b1bdd92c0c549d38f01373935b1705f4b8310885efd8b2e23a4f4ac5da1b78"
    | Eip1559 -> make_tx_nonce ~nonce:1 ~legacy:false
  in
  let*@ _ok = Rpc.send_raw_transaction ~raw_tx:nonce_1 sequencer in
  (* Nonce 0 is refused. *)
  let* nonce_0 =
    match tx_type with
    | Legacy -> make_tx_nonce ~nonce:0 ~legacy:true
    | Eip2930 ->
        return
          "0x01f86782053980843b9aca008261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c001a05ba2acb79e66aaadd076b1ee6fdf09f5cb95541e2b24504209f89908b50a84dea0492d8cf5c29b5ec44b4730eebfb62ddf5ce486d5b8729c50a8002d6219f76412"
    | Eip1559 -> make_tx_nonce ~nonce:0 ~legacy:false
  in
  let*@? err = Rpc.send_raw_transaction ~raw_tx:nonce_0 sequencer in
  Check.((err.message = "Nonce too low") string)
    ~error_msg:"the transaction has an invalid nonce, it should fail" ;

  unit

let test_validate_max_fee_per_gas =
  register ~title:"Validate max fee per gas" ~tags:["max_fee_per_gas"]
  @@ fun _kernel sequencer tx_type ->
  let source = Eth_account.bootstrap_accounts.(0) in

  let* base_fee_per_gas = Rpc.get_gas_price sequencer in
  let base_fee_per_gas = Int32.to_int base_fee_per_gas in

  let make_tx_gas_price ~gas_price ~legacy =
    Cast.craft_tx
      ~legacy
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price
      ~gas:30_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  let* gas_price_below =
    match tx_type with
    | Legacy -> make_tx_gas_price ~gas_price:(base_fee_per_gas - 1) ~legacy:true
    | Eip2930 ->
        return
          "0x01f863820539800a8261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c001a0978d80876079de8efc0d3623ba127c19d3f74743bfbd8bffc461302c1b3ede23a063a107e1d83060ccbb38bbb02baccf01b4eebdf0cfefd0b21b9eafd7756fba3b"
    | Eip1559 ->
        make_tx_gas_price ~gas_price:(base_fee_per_gas - 1) ~legacy:false
  in
  let* () =
    send_transaction_and_fail_upon_sequencer_validation
      ~raw_tx:gas_price_below
      sequencer
      ~expected_error:"Max gas fee too low"
      ~error_msg:"the transaction has gas price too low, it should fail"
  in

  let* gas_price_enough =
    match tx_type with
    | Legacy -> make_tx_gas_price ~gas_price:base_fee_per_gas ~legacy:true
    | Eip2930 ->
        return
          "0x01f86782053980843b9aca008261a8945d66ec78664f4a0b0929a41270316a6cd4d8bd4b8080c001a05ba2acb79e66aaadd076b1ee6fdf09f5cb95541e2b24504209f89908b50a84dea0492d8cf5c29b5ec44b4730eebfb62ddf5ce486d5b8729c50a8002d6219f76412"
    | Eip1559 -> make_tx_gas_price ~gas_price:base_fee_per_gas ~legacy:false
  in
  let* _hash =
    send_transaction_and_wait_confirmation ~raw_tx:gas_price_enough sequencer
  in
  unit

let test_validate_pay_for_fees =
  register ~title:"Validate pay for fees" ~tags:["pay_for_fees"]
  @@ fun _kernel sequencer tx_type ->
  let empty_account =
    Eth_account.
      {
        address = "0xA257edC8ad1D8f8f463aC0D947cc381000b3c863";
        private_key =
          "0xb80e5dd2ba9281e482589973600609bb0f10f6a075e6c733e4472d4dd2df238a";
      }
  in

  let* base_fee_per_gas = Rpc.get_gas_price sequencer in
  let base_fee_per_gas = Int32.to_int base_fee_per_gas in

  let make_tx_pay_fees ~nonce ~legacy ~gas_price () =
    Cast.craft_tx
      ~source_private_key:empty_account.private_key
      ~chain_id:1337
      ~nonce
      ~legacy
      ~gas_price
      ~gas:100_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  (* The account has no funds, any transaction will be rejected. *)
  let* insufficient_funds =
    match tx_type with
    | Legacy ->
        make_tx_pay_fees ~nonce:0 ~legacy:true ~gas_price:base_fee_per_gas ()
    | Eip2930 ->
        Test.fail
          ~__LOC__
          "Eip2930 are hardcoded transaction, we can't use value like \
           base_fee_per_gas"
    | Eip1559 ->
        make_tx_pay_fees ~nonce:0 ~legacy:false ~gas_price:base_fee_per_gas ()
  in
  let* () =
    send_transaction_and_fail_upon_sequencer_validation
      ~raw_tx:insufficient_funds
      sequencer
      ~expected_error:"Cannot prepay transaction."
      ~error_msg:"the account has no funds, it should fail"
  in

  (* We transfer enough funds to pay for `100_000 (gas unit) * gas price`. *)
  let* _ =
    send_transaction_to_sequencer
      (Eth_cli.transaction_send
         ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
         ~to_public_key:empty_account.address
         ~value:(Wei.of_string (string_of_int (100_000 * base_fee_per_gas)))
         ~endpoint:(Evm_node.endpoint sequencer))
      sequencer
  in

  (* Now the account can pay for at least the base_fee_per_gas. *)
  let* enough_funds =
    match tx_type with
    | Legacy ->
        make_tx_pay_fees ~nonce:0 ~legacy:true ~gas_price:base_fee_per_gas ()
    | Eip2930 ->
        Test.fail
          ~__LOC__
          "Eip2930 are hardcoded transaction, we can't use value like \
           base_fee_per_gas"
    | Eip1559 ->
        make_tx_pay_fees ~nonce:0 ~legacy:false ~gas_price:base_fee_per_gas ()
  in
  let* _hash =
    send_transaction_and_wait_confirmation ~raw_tx:enough_funds sequencer
  in
  (* But it's the gas price provided by the user that's necessary, not
     the base fee per gas. If the user sets an higher gas price, it needs
     to be able to pay for it. *)
  let* insufficient_funds =
    match tx_type with
    | Legacy ->
        make_tx_pay_fees
          ~nonce:1
          ~legacy:true
          ~gas_price:(base_fee_per_gas + 1)
          ()
    | Eip2930 ->
        Test.fail
          ~__LOC__
          "Eip2930 are hardcoded transaction, we can't use value like \
           base_fee_per_gas"
    | Eip1559 ->
        make_tx_pay_fees
          ~nonce:1
          ~legacy:false
          ~gas_price:(base_fee_per_gas + 1)
          ()
  in
  let* () =
    send_transaction_and_fail_upon_sequencer_validation
      ~raw_tx:insufficient_funds
      sequencer
      ~expected_error:"Cannot prepay transaction."
      ~error_msg:"Transaction rejected, failed with %L, expected %R."
  in
  unit

let test_validate_pay_for_fees_max_fee_per_gas =
  register
    ~da_fee_per_byte:(Wei.of_string "4_000_00_000")
    ~minimum_base_fee_per_gas:(Wei.of_string "2_000")
    ~title:"DA fees computation with max fee per gas"
    ~tags:["pay_for_fees"; "da_fees"]
  @@ fun _kernel sequencer tx_type ->
  (* This is a regression test for the bug where the DA fees are computed using
     the `transaction.max_fee_per_gas` instead of the `base_fee_per_gas`
     (or `gas_price`).
     The problem occured because the account is supposed to be able to
     pay for `max_fee_per_gas`, but the DA fees computation should use the real
     `gas_price`.

     The test uses a very small base_fee_per_gas, so the gas limit will be
     enormous as it requires many units to cover for the da fees. However,
     if the validation computes the DA fees using a different gas price
     (a much bigger one), it will consider too many gas unit for the execution
     gas. *)
  let estimateGas =
    [("to", `String "0xd77420f73b4612a7a99dba8c2afd30a1886b0344")]
  in
  let*@ gas = Rpc.estimate_gas estimateGas sequencer in
  let* raw_tx =
    Cast.craft_tx
      ~source_private_key:Eth_account.bootstrap_accounts.(0).private_key
      ~chain_id:1337
      ~nonce:0
      ~legacy:(tx_type = Legacy)
      ~gas_price:1_000_000_000
      ~gas:(Int64.to_int gas)
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  (* As the transaction has the maximum gas possible 30M. If the DA
     fees are wrongly calculated it would consider that the execution
     gas limit is more than 30M, and result in a failure. *)
  let* _hash = send_transaction_and_wait_confirmation ~raw_tx sequencer in
  unit

let test_validate_gas_limit_above_the_maximum =
  register
    ~da_fee_per_byte:(Wei.of_string "4_000_000_000_000")
    ~title:"Validate gas limit above the maximum"
    ~tags:["gas_limit"]
  @@ fun _ sequencer tx_type ->
  let source = Eth_account.bootstrap_accounts.(0) in
  let make_tx_gas_limit ~legacy ~gas =
    Cast.craft_tx
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~legacy
      ~gas
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  (* 300_000_000 gas limit is above the maximum gas_limit for a transaction (even a block) *)
  let* gas_limit_above_the_maximum =
    match tx_type with
    | Legacy -> make_tx_gas_limit ~legacy:true ~gas:300_000_000
    | Eip1559 -> make_tx_gas_limit ~legacy:false ~gas:300_000_000
    | Eip2930 ->
        return
          "0x01f86982053980843b9aca008411e1a30094d77420f73b4612a7a99dba8c2afd30a1886b03448080c001a026ce5062285cd3ade072bd279e56a5ce0679cd56c8cfaf434f5d2b9a1d211c8ea06ebd07be2e0231557a0f6a3766667faa711f0675469da46e3dceb045d5558fd5"
  in
  let* tx_hash =
    send_transaction_and_wait_confirmation
      ~raw_tx:gas_limit_above_the_maximum
      sequencer
  in
  let*@ receipt_opt = Rpc.get_transaction_receipt ~tx_hash sequencer in
  match receipt_opt with
  | None ->
      Test.fail
        ~__LOC__
        "Expected a receipt for transaction hash %s but got none"
        tx_hash
  | Some receipt ->
      Check.is_true
        receipt.status
        ~__LOC__
        ~error_msg:
          "Expected status in transaction receipt to be 0x1 (success) but got \
           0x0 (failure)" ;
      Check.((receipt.cumulativeGasUsed = 621_000L) ~__LOC__ int64)
        ~error_msg:
          "Expected cumulative gas used in transaction receipt to be lower \
           than 625_000 but got %L" ;

      unit

(** This test verifies that transactions with gas consumption over the
    maximum allowed gas per transaction are properly rejected by
    confirming gas used equals the gas limit in case of
    under-approximated gas limit. *)
let test_validate_custom_gas_limit_less_than_maximum_gas_per_transaction =
  let maximum_gas_per_transaction = 21001L in
  register
    ~maximum_gas_per_transaction
    ~da_fee_per_byte:(Wei.of_string "4_000_000_000_000")
    ~title:
      "Validate custom gas limit lower than the maximum gas per transaction"
    ~tags:["gas_limit"; "maximum_gas_per_transaction"]
  @@ fun kernel sequencer tx_type ->
  if kernel = Kernel.Latest then
    (* This test isn't relevant on a kernel where REVM is activated. A transaction with a gas limit inferior to 21 000
       can not be processed. The test rely on a semantic mistake from our Sputnik implementation.
       TODO: As a follow-up [validate_gas_limit] should refuse transaction with a transaction gas limit < 21 000 to be
       aligned with other EVM-compatible chains. *)
    unit
  else (
    assert (tx_type = Legacy) ;
    let source = Eth_account.bootstrap_accounts.(0) in
    let inclusion_fees = 600_000 in
    let gas = inclusion_fees + Int64.to_int maximum_gas_per_transaction in
    let under_approximated_gas = pred gas in
    let* tx =
      Cast.craft_tx
        ~source_private_key:source.private_key
        ~chain_id:1337
        ~nonce:0
        ~gas_price:1_000_000_000
        ~legacy:true
        ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
        ~value:Wei.zero
        ~gas:under_approximated_gas
        ()
    in
    let* tx_hash =
      send_transaction_and_wait_confirmation ~raw_tx:tx sequencer
    in
    let*@ receipt_opt = Rpc.get_transaction_receipt ~tx_hash sequencer in
    match receipt_opt with
    | None ->
        Test.fail
          ~__LOC__
          "Expected a receipt for transaction hash %s but got none"
          tx_hash
    | Some receipt ->
        Check.is_true
          receipt.status
          ~__LOC__
          ~error_msg:
            "Expected status in transaction receipt to be 0x1 (success) but \
             got 0x0 (failure)" ;
        Check.(
          (receipt.cumulativeGasUsed = Int64.of_int under_approximated_gas)
            ~__LOC__
            int64)
          ~error_msg:
            "Expected cumulative gas used in transaction receipt to be the gas \
             limit provided (%R) but got %L" ;
        unit)

let test_validate_gas_limit =
  register
    ~da_fee_per_byte:(Wei.of_string "4_000_000_000_000")
    ~title:"Validate gas limit"
    ~tags:["da_fee"; "gas_limit"]
  @@ fun _kernel sequencer tx_type ->
  let source = Eth_account.bootstrap_accounts.(0) in
  let make_tx_gas_limit ~legacy ~gas =
    Cast.craft_tx
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~legacy
      ~gas
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  (* 30_000 gas limit is enough to cover execution gas limit but not the gas for da_fees *)
  let* not_enough_gas_limit =
    match tx_type with
    | Legacy -> make_tx_gas_limit ~legacy:true ~gas:100_000
    | Eip1559 -> make_tx_gas_limit ~legacy:false ~gas:100_000
    | Eip2930 ->
        return
          "0x01f86882053980843b9aca00830186a094d77420f73b4612a7a99dba8c2afd30a1886b03448080c001a015d91492a6fac0b1d507e16bffe6ba1de4544bbc9d139d32e97422d7afcfa05ea038f69526e800a06e50587006b924f1d48c732315add8bf3d6bfb79a2fd29278d"
  in
  let*@? err =
    Rpc.send_raw_transaction ~raw_tx:not_enough_gas_limit sequencer
  in
  Check.(
    err.message
    =~ rex
         "Please increase the gas limit or use eth_estimateGas to get the \
          recommended amount.")
    ~error_msg:
      "The transaction has not enough gas to pay da_fees, it should fail" ;
  (* This tx is the same as the valid_transaction in eip2930 but with some random entry for access_list *)
  let not_enough_access_list_tx =
    "0x01f902bd82053980843b9aca00830f424094d77420f73b4612a7a99dba8c2afd30a1886b03448080f90253f89b9402704ed8b5a8e817f354d59432e115e0d8053394f884a00000000000000000000000000000000000000000000000000000000000000001a00000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000000000002a0fe4d1297c5434445a55041cf44037c0799556cc55064da684dc6eed1a5dccabff8bc944585fe77225b41b697c938b018e2ac67ac5a20c0f8a5a00000000000000000000000000000000000000000000000000000000000000079a00000000000000000000000000000000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000000000004a00000000000000000000000000000000000000000000000000000000000000001a0a00e9f45e9f0c328446d13a90db1b8ff531c4946ba6a4294a1ec03159cc44b19f87a94c02aaa39b223fe8d0a0e5c4f27ead9083c756cc2f863a09c7d93c4e4b5ea55e1466a741eef69e5430c31615a1970eebbd883a9864ed2dca09ede93be0d8fc6a5eb9cf1c7345a85b7519d8487a727aef0c2f00ab966aa7716a01ea3275ac863f4decf8615eb5ddf70a19af62b291bfd8b6e6747fceb19ae4484f87a942260fac5e5542a773aa44fbcfedf7c193bc2c599f863a0dc276a4f120117ad5ae6415d1c724b4f3a0e81f0ee6466e1392ca121b63123f2a00000000000000000000000000000000000000000000000000000000000000005a038137cdf9f165d9fe3ae438081fac96e39615491dfc8ca4a0e150d98de492a7d01a0af5ec7d4ba53e8ff408f1aef5f1a701b51c1bc5b9331ee7c194b5209b47ca121a07e853bc9d24fe2937843fc1feab758bc0aed3648816661e7e07ab3937650a380"
  in
  let*@? err =
    Rpc.send_raw_transaction ~raw_tx:not_enough_access_list_tx sequencer
  in
  Check.(err.message =~ rex "Not enough gas for inclusion fees")
    ~error_msg:
      "The transaction has not enough gas to pay da_fees for access_list, it \
       should fail" ;

  (* This transaction should work as it covers the gas for da_fee *)
  let* valid_transaction =
    match tx_type with
    | Legacy -> make_tx_gas_limit ~legacy:true ~gas:1_000_000
    | Eip1559 -> make_tx_gas_limit ~legacy:false ~gas:1_000_000
    | Eip2930 ->
        return
          "0x01f86882053980843b9aca00830f424094d77420f73b4612a7a99dba8c2afd30a1886b03448080c080a0d901759695e31fd26bfb4cee10022251d74bec021bceed65449705491b148ea1a0788c6b2da1784a2cda4d25afb18873c17a7ff989d31a6d67a74966e495fdd77a"
  in
  let*@ _ok = Rpc.send_raw_transaction ~raw_tx:valid_transaction sequencer in
  unit

let test_sender_is_not_contract =
  let source = Eth_account.bootstrap_accounts.(0) in
  register
    ~set_account_code:[(source.address, "6080")]
    ~title:"Sender must not be a contract"
    ~tags:["sender_contract"]
  @@ fun _kernel sequencer tx_type ->
  let make_raw_tx ~legacy =
    Cast.craft_tx
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~legacy
      ~gas:30_000
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  let* sender_is_a_contract =
    match tx_type with
    | Legacy -> make_raw_tx ~legacy:true
    | Eip2930 ->
        return
          "01f86f82053980843b9aca00825b0494b53dc01974176e5dff2298c5a94343c2585e3c54880de0b6b3a764000080c080a04cda4f1de731980d8f9bb4653f5bdf82cbacb342af15f512e925cb5e7bf44b9ba020874590087c9c2e481f5beb3abb96a9e2ce2712e064f9443760921d7e90d9eb"
    | Eip1559 -> make_raw_tx ~legacy:false
  in
  (* Just testing that the check to know if sender has code works.
     The good case where sender is not a contract is already tested
     by all the other tests *)
  let*@? err =
    Rpc.send_raw_transaction ~raw_tx:sender_is_a_contract sequencer
  in
  let error_msg =
    Format.sprintf
      "The error of this transaction should be that the sender is a contract, \
       but it's %s"
      err.message
  in
  Check.((err.message = "Sender is a contract which is not possible") string)
    ~error_msg ;

  unit

let test_base_gas_cost =
  register
    ~title:
      "Transactions with gas limit below the base gas cost (which includes \
       inclusion fees) are rejected"
    ~tags:["base_gas_cost_minimum_limit"]
  @@ fun _kernel sequencer tx_type ->
  let source = Eth_account.bootstrap_accounts.(0) in
  let make_raw_tx ~legacy =
    Cast.craft_tx
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1_000_000_000
      ~legacy
      ~gas:20_999
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~value:Wei.zero
      ()
  in
  let* raw_tx =
    match tx_type with
    | Legacy -> make_raw_tx ~legacy:true
    | Eip2930 | Eip1559 -> make_raw_tx ~legacy:false
  in
  let*@? err = Rpc.send_raw_transaction ~raw_tx sequencer in
  Check.(err.message =~ rex "The provided gas limit")
    ~error_msg:"The error message should be %R but got %L" ;
  unit

let test_validate_calldata_cost =
  register
    ~da_fee_per_byte:Wei.zero
    ~title:"Validate that gas limit validation covers calldata cost"
    ~tags:["calldata_cost"; "gas_limit"]
  @@ fun _kernel sequencer tx_type ->
  let source = Eth_account.bootstrap_accounts.(0) in
  let make_tx ~legacy =
    Cast.craft_tx
      ~source_private_key:source.private_key
      ~chain_id:1337
      ~nonce:0
      ~gas_price:1
      ~gas:100_000
      ~legacy
      ~address:"0xd77420f73b4612a7a99dba8c2afd30a1886b0344"
      ~arguments:[String.make 100_000 '1']
      ~value:Wei.zero
      ()
  in
  let* raw_tx =
    match tx_type with
    | Legacy -> make_tx ~legacy:true
    | Eip1559 | Eip2930 -> make_tx ~legacy:false
  in
  let*@? err = Rpc.send_raw_transaction ~raw_tx sequencer in
  Check.(err.message =~ rex " is insufficient to cover the transaction cost")
    ~error_msg:
      "The transaction has not enough gas to pay calldata cost, it should fail" ;
  unit

let () =
  let all_types = [Legacy; Eip1559; Eip2930] in
  test_validate_compressed_sig [Legacy] ;
  test_validate_recover_caller all_types ;
  test_validate_chain_id all_types ;
  test_validate_nonce all_types ;
  test_validate_max_fee_per_gas all_types ;
  test_validate_pay_for_fees [Legacy; Eip1559] ;
  test_validate_pay_for_fees_max_fee_per_gas [Legacy; Eip1559] ;
  test_validate_gas_limit all_types ;
  test_validate_gas_limit_above_the_maximum all_types ;
  test_validate_custom_gas_limit_less_than_maximum_gas_per_transaction [Legacy] ;
  test_sender_is_not_contract all_types ;
  test_base_gas_cost all_types ;
  test_validate_calldata_cost all_types
