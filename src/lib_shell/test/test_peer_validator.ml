(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** Testing
    -------
    Component:    Peer_validator
    Invocation:   dune exec src/lib_shell/test/main.exe \
                  -- --file test_peer_validator.ml
    Subject:      Unit tests for [Peer_validator]
*)

module Assert = Assert

(** [wrap f] create a mocked [chain_store], [Chain_db] and a
    [Peer_validator] that can be used by [f]. *)
let wrap
    (f :
      Distributed_db.chain_db ->
      Block_header.t ->
      Peer_validator.t ->
      unit tzresult Lwt.t) () =
  Lwt_utils_unix.with_tempdir "tezos_test_" (fun test_dir ->
      let open Lwt_result_syntax in
      let*! store = Shell_test_helpers.init_chain test_dir in
      let* p2p =
        Shell_test_helpers.init_mock_p2p Distributed_db_version.Name.zero
      in
      let db = Distributed_db.create store p2p in
      let chain_store = Store.(main_chain_store store) in
      let callback =
        P2p_reader.
          {
            notify_branch = (fun _ _ -> ());
            notify_head = (fun _ _ _ _ -> ());
            disconnection = (fun _ -> ());
          }
      in
      let chain_db = Distributed_db.activate db chain_store callback in
      let*! genesis =
        Store.Block.read_block_opt
          chain_store
          Shell_test_helpers.genesis_block_hash
      in
      let genesis = WithExceptions.Option.get ~loc:__LOC__ genesis in
      let genesis_header = Store.Block.header genesis in
      let internal_validator =
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2833
           External validator should be used *)
        Block_validator_process.Internal chain_store
      in
      let validator_environment =
        {
          Block_validator_process.user_activated_upgrades = [];
          user_activated_protocol_overrides = [];
          operation_metadata_size_limit = Unlimited;
        }
      in
      let* block_validator_processs =
        Block_validator_process.init validator_environment internal_validator
      in
      let*! block_validator =
        Block_validator.create
          Shell_limits.default_block_validator_limits
          db
          block_validator_processs
          ~start_testchain:false
      in
      let*! pv =
        Peer_validator.create
          Shell_limits.default_peer_validator_limits
          block_validator
          chain_db
          Tezos_crypto.Crypto_box.Public_key_hash.zero
      in
      let* () =
        Lwt.finalize
          (fun () -> f chain_db genesis_header pv)
          (fun () ->
            let*! () = Peer_validator.shutdown pv in
            let*! () = Block_validator_process.close block_validator_processs in
            let*! () = Block_validator.shutdown block_validator in
            unit)
      in
      return_unit)

(* This test ensure that [validate_new_head hash header] remove all
   entries of [hash] in the db if the fitness does not increase. *)
let test_validate_new_head_on_lower_fitness () =
  let open Lwt_result_syntax in
  wrap
    (fun chain_db genesis_header pv ->
      let block_header =
        {
          Block_header.shell =
            {
              genesis_header.shell with
              (* Genesis validation_passes = 0, the block must at least have 1
                 validation passe for this test. *)
              validation_passes = 2;
            };
          protocol_data = genesis_header.protocol_data;
        }
      in
      let block_hash = Block_header.hash block_header in

      (* Craft a list of operation and manually inject it with [block_hash]
         and validation passes [0] in the db. *)
      let ops_0 = QCheck2.Gen.generate ~n:10 (Generators.operation_gen ()) in
      let*! _ =
        Distributed_db.Operations.inject chain_db (block_hash, 0) ops_0
      in
      (* Craft a list of operation and manually inject it with [block_hash]
           and validation passes [1] in the db. *)
      let ops_1 = QCheck2.Gen.generate ~n:5 (Generators.operation_gen ()) in
      let*! _ =
        Distributed_db.Operations.inject chain_db (block_hash, 1) ops_1
      in

      let*! known_0 =
        Distributed_db.Operations.known chain_db (block_hash, 0)
      in
      let*! known_1 =
        Distributed_db.Operations.known chain_db (block_hash, 1)
      in
      Assert.assert_true
        (Format.asprintf
           "The ddb should contain two entries for %a"
           Block_hash.pp
           block_hash)
        (known_0 && known_1) ;

      (* Try to validate a block with the same fitness as the previous block
         ([genesis]). *)
      let* () =
        Peer_validator.Internal_for_tests.validate_new_head
          pv
          block_hash
          block_header
      in

      let*! known_0 =
        Distributed_db.Operations.known chain_db (block_hash, 0)
      in
      let*! known_1 =
        Distributed_db.Operations.known chain_db (block_hash, 1)
      in

      Assert.assert_false
        (Format.asprintf
           "The ddb should not contain an entry for %a anymore"
           Block_hash.pp
           block_hash)
        (known_0 || known_1) ;
      return_unit)
    ()

let () =
  Alcotest_lwt.run
    ~__FILE__
    "Peer_validator"
    [
      ( "Validate_new_head",
        [
          Tztest.tztest
            "Clear the db is the fitness does not increase"
            `Quick
            test_validate_new_head_on_lower_fitness;
        ] );
    ]
  |> Lwt_main.run
