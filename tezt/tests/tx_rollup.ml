(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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
   Component:    Transactional rollups
   Invocation:   dune exec tezt/tests/main.exe -- --file tx_rollup.ml
   Subject:      .
*)

(** To be attached to process whose output needs to be captured by the
    regression framework. *)
let hooks = Tezos_regression.hooks

module Rollup = Rollup.Tx_rollup

let parameter_file protocol =
  Protocol.write_parameter_file
    ~base:(Either.right (protocol, None))
    [(["tx_rollup_enable"], Some "true")]

(* This module only registers regressions tests. Those regressions
   tests should be used to ensure there is no regressions with the
   various RPCs exported by the tx_rollups. *)
module Regressions = struct
  type t = {node : Node.t; client : Client.t; rollup : string}

  let init_with_tx_rollup ~protocol =
    let* parameter_file = parameter_file protocol in
    let* (node, client) =
      Client.init_with_protocol ~parameter_file `Client ~protocol ()
    in
    (* We originate a dumb rollup to be able to generate a paths for
       tx_rollups related RPCs. *)
    let*! rollup =
      Client.Tx_rollup.originate_tx_rollup
        ~src:Constant.bootstrap1.public_key_hash
        client
    in
    let* () = Client.bake_for client in
    let* _ = Node.wait_for_level node 2 in
    return {node; client; rollup}

  let rpc_state ~protocols =
    Protocol.register_regression_test
      ~__FILE__
      ~output_file:"tx_rollup_rpc_state"
      ~title:"RPC (tx_rollups, regression) - state"
      ~tags:["tx_rollup"; "rpc"]
      ~protocols
    @@ fun protocol ->
    let* {node = _; client; rollup} = init_with_tx_rollup ~protocol in
    let*! _state = Rollup.get_state ~hooks ~rollup client in
    return ()

  let submit_batch ~batch {rollup; client; node} =
    let*! () =
      Client.Tx_rollup.submit_batch
        ~hooks
        ~content:batch
        ~rollup
        ~src:Constant.bootstrap1.public_key_hash
        client
    in
    let current_level = Node.get_level node in
    let* () = Client.bake_for client in
    let* _ = Node.wait_for_level node (current_level + 1) in
    return ()

  let rpc_inbox ~protocols =
    Protocol.register_regression_test
      ~__FILE__
      ~output_file:"tx_rollup_rpc_inbox"
      ~title:"RPC (tx_rollups, regression) - inbox"
      ~tags:["tx_rollup"; "rpc"; "inbox"]
      ~protocols
    @@ fun protocol ->
    let* ({rollup; client; node = _} as state) =
      init_with_tx_rollup ~protocol
    in
    (* The content of the batch does not matter for the regression test. *)
    let batch = "blob" in
    let* () = submit_batch ~batch state in
    let*! _state = Rollup.get_inbox ~hooks ~rollup client in
    unit

  let submit_commitment ~level ~roots ~predecessor {rollup; client; node} =
    let*! () =
      Client.Tx_rollup.submit_tx_rollup_commitment
        ~hooks
        ~level
        ~roots
        ~predecessor
        ~rollup
        ~src:Constant.bootstrap1.public_key_hash
        client
    in
    let current_level = Node.get_level node in
    let* () = Client.bake_for client in
    let* _ = Node.wait_for_level node (current_level + 1) in
    return ()

  let rpc_commitment ~protocols =
    Protocol.register_regression_test
      ~__FILE__
      ~output_file:"tx_rollup_rpc_commitment"
      ~title:"RPC (tx_rollups, regression) - commitment"
      ~tags:["tx_rollup"; "rpc"; "commitment"]
      ~protocols
    @@ fun protocol ->
    let* ({rollup; client; node} as state) = init_with_tx_rollup ~protocol in
    (* The content of the batch does not matter for the regression test. *)
    let batch = "blob" in
    let* () = submit_batch ~batch state in
    let batch_level = Node.get_level node in
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/2503

       we introduce two bakes to ensure the block is finalised. This
       should be removed once we do not rely on Tenderbake anymore. *)
    let* () = Client.bake_for client in
    let* () = Client.bake_for client in
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/2503

       At the same time we remove the dependency to Tenderbake for
       commitment, we will ensure the root is indeed the root of the
       previous inbox. I don't know yet how we will be able to do that
       yes, something is missing. *)
    let* () =
      submit_commitment
        ~level:batch_level
        ~roots:["root"]
        ~predecessor:None
        state
    in
    let offset = Node.get_level node - batch_level in
    let*! _state =
      Rollup.get_commitment ~hooks ~block:"head" ~offset ~rollup client
    in
    unit

  module Fail = struct
    let client_submit_batch_invalid_rollup_address ~protocols =
      let open Tezt_tezos in
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:"tx_rollup_client_submit_batch_invalid_rollup_address"
        ~title:"Submit a batch to an invalid rollup address should fail"
        ~tags:["tx_rollup"; "client"; "fail"]
        ~protocols
      @@ fun protocol ->
      let* parameter_file = parameter_file protocol in
      let* (_node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let invalid_address = "this is an invalid tx rollup address" in
      let*? process =
        Client.Tx_rollup.submit_batch
          ~hooks
          ~content:""
          ~rollup:invalid_address
          ~src:Constant.bootstrap1.public_key_hash
          client
      in
      let* () =
        Process.check_error
          ~exit_code:1
          ~msg:
            (rex
               ("Parameter '" ^ invalid_address
              ^ "' is an invalid tx rollup address"))
          process
      in
      unit
  end

  let register ~protocols =
    rpc_state ~protocols ;
    rpc_inbox ~protocols ;
    rpc_commitment ~protocols ;
    Fail.client_submit_batch_invalid_rollup_address ~protocols
end

(** To be attached to process whose output needs to be captured by the
    regression framework. *)
let hooks = Tezos_regression.hooks

let submit_three_batches_and_check_size ~rollup node client batches level =
  let* () =
    Lwt_list.iter_p
      (fun (content, src, _) ->
        let*! () =
          Client.Tx_rollup.submit_batch ~hooks ~content ~rollup ~src client
        in
        unit)
      batches
  in
  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node level in
  (* Check the inbox has been created, with the expected cumulated size. *)
  let expected_inbox =
    Rollup.
      {
        cumulated_size =
          List.fold_left
            (fun acc (batch, _, _) -> acc + String.length batch)
            0
            batches;
        contents = List.map (fun (_, _, batch) -> batch) batches;
      }
  in
  let*! inbox = Rollup.get_inbox ~hooks ~rollup client in
  Check.(
    ((inbox = expected_inbox)
       ~error_msg:"Unpexected inbox. Got: %L. Expected: %R.")
      Rollup.Check.inbox) ;
  return ()

let test_submit_batches_in_several_blocks ~protocols =
  Protocol.register_test
    ~__FILE__
    ~title:"Submit batches in several blocks"
    ~tags:["tx_rollup"]
    ~protocols
  @@ fun protocol ->
  let* parameter_file = parameter_file protocol in
  let* (node, client) =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let*! rollup =
    Client.Tx_rollup.originate_tx_rollup
      ~src:Constant.bootstrap1.public_key_hash
      client
  in
  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node 2 in
  (* We check the rollup exists by trying to fetch its state. Since it
     is a regression test, we can detect changes to this default
     state. *)
  let*! state = Rollup.get_state ~hooks ~rollup client in
  let expected_state =
    Rollup.{burn_per_byte = 0; inbox_ema = 0; last_inbox_level = None}
  in
  Check.(state = expected_state)
    Rollup.Check.state
    ~error_msg:"Unexpected state. Got: %L. Expected: %R." ;
  let batch = "tezos" in
  let*! () =
    Client.Tx_rollup.submit_batch
      ~hooks
      ~content:batch
      ~rollup
      ~src:Constant.bootstrap1.public_key_hash
      client
  in
  let batch1 = "tezos" in
  let batch2 = "tx_rollup" in
  let batch3 = "layer-2" in

  (* Hashes are hardcoded, but could be computed programmatically. *)
  let submission =
    [
      ( batch1,
        Constant.bootstrap1.public_key_hash,
        "M292FuoYJhEhgpQDTpKvXSagLopdXtGXRMyLyxhPTd982dqxoaE" );
      ( batch2,
        Constant.bootstrap2.public_key_hash,
        "M21VEKoBenkHCZC8WpUDtxzv4uoixG1iGUxUvMg4UGc88CRWFNF" );
      ( batch3,
        Constant.bootstrap3.public_key_hash,
        "M21tdhc2Wn76n164oJvyKW4JVZsDSDeuDsbLgp61XZWtrXjL5WA" );
    ]
  in
  (* Let’s try once and see if everything goes as expected *)
  let* () =
    submit_three_batches_and_check_size ~rollup node client submission 3
  in
  (* Let’s try to see if we can submit three more batches in the next level *)
  let* () =
    submit_three_batches_and_check_size ~rollup node client submission 3
  in
  unit

let test_submit_from_originated_source ~protocols =
  let open Tezt_tezos in
  Protocol.register_test
    ~__FILE__
    ~title:"Submit from an originated contract should fail"
    ~tags:["tx_rollup"; "client"]
    ~protocols
  @@ fun protocol ->
  let* parameter_file = parameter_file protocol in
  let* (node, client) =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  (* We begin by originating a contract *)
  let* originated_contract =
    Client.originate_contract
      ~alias:"originated_contract_simple"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg:"file:./tezt/tests/contracts/proto_alpha/str_id.tz"
      ~init:"Some \"initial storage\""
      ~burn_cap:Tez.(of_int 3)
      client
  in
  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node 2 in
  (* We originate a tx_rollup using an implicit account *)
  let*! rollup =
    Client.Tx_rollup.originate_tx_rollup
      ~src:Constant.bootstrap1.public_key_hash
      client
  in
  let* () = Client.bake_for client in
  let batch = "tezos" in
  (* Finally, we submit a batch to the tx_rollup from an originated contract *)
  let*? process =
    Client.Tx_rollup.submit_batch
      ~hooks
      ~content:batch
      ~rollup
      ~src:originated_contract
      client
  in
  let* () =
    Process.check_error
      ~exit_code:1
      ~msg:(rex "Only implicit accounts can submit transaction rollup batches")
      process
  in
  unit

let register ~protocols =
  Regressions.register ~protocols ;
  test_submit_batches_in_several_blocks ~protocols ;
  test_submit_from_originated_source ~protocols
