(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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
   Component:    [stresstest] client command
   Invocation:   dune exec tezt/manual_tests/main.exe -- --file stresstest_command.ml
   Subject:      Test the [stresstest] client command. Even though this
                 command is only used for testing purposes, its development
                 is both tricky and ongoing. Moreover, it can be complex to
                 debug when called in various contexts. That is why these
                 tests are nice to have. *)

type config = {
  faucet_key : Account.key;
  (* Generate accounts *)
  nb_accounts : int;
  (* Distribute tokens *)
  batch_size : int;
  batch_per_block : int;
  amount : Int64.t; (*micro tz*)
}

let test_stresstest_fund_accounts =
  Protocol.register_test
    ~__FILE__
    ~title:"stresstest fund multiple accounts"
    ~tags:["stresstest"; "fund"; "accounts"]
  @@ fun protocol ->
  let config =
    {
      faucet_key = Constant.bootstrap1;
      nb_accounts = 100;
      batch_size = 10;
      batch_per_block = 2;
      amount = 42_000_000L;
    }
  in
  let* baking_node, baking_client =
    Client.init_with_protocol
      ~nodes_args:[Synchronisation_threshold 0; Connections 1]
      `Client
      ~protocol
      ()
  in
  let* node, client =
    Client.init_with_node
      ~nodes_args:[Synchronisation_threshold 0; Connections 1]
      `Client
      ()
  in
  let* () = Client.Admin.connect_address ~peer:baking_node client in
  let* _ = Node.wait_for_level node 1 in
  let* accounts =
    Client.stresstest_gen_keys ~alias_prefix:"id_" config.nb_accounts client
  in
  let p, r = Lwt.wait () in
  (*
     Blocks to bake to ensure that all accounts were funded:
     init : 1
     reveal : batch_size / batch_per_blocks
     batches : ((nb_accounts - batch_size) / batch_size / batch_per_block )
     final bake
  *)
  let min_blocks_to_bake =
    (*fund starters*)
    1
    (*reveals*) + (config.batch_size / config.batch_per_block)
    + (config.batch_size mod config.batch_per_block)
    + (* fund batches*)
    +((config.nb_accounts - config.batch_size)
     / config.batch_size / config.batch_per_block)
    + (config.nb_accounts - config.batch_size)
      / config.batch_size mod config.batch_per_block
  in
  let () =
    Background.register
      (let rec bake_n n =
         let* () = Client.bake_for baking_client in
         Log.info
           "Block %d/%d baked@."
           (min_blocks_to_bake - n)
           min_blocks_to_bake ;
         let* () = Lwt_unix.sleep 1. in
         if n = 0 then unit else bake_n (pred n)
       in
       let* () = bake_n min_blocks_to_bake in
       Lwt.wakeup r () ;
       unit)
  in
  let initial_amount = Tez.of_mutez_int64 config.amount in
  let* () =
    Client.stresstest_fund_accounts_from_source
      ~source_key_pkh:Constant.bootstrap1.public_key_hash
      ~batch_size:config.batch_size
      ~batches_per_block:config.batch_per_block
      ~initial_amount
      client
  in
  let* () = p in
  (* Make sure that accounts are well funded, that is to say, contains
     at least the initial_amount.*)
  Lwt_list.iter_s
    (fun a ->
      let account = a.Account.public_key_hash in
      let* balance = Client.get_balance_for ~account client in
      let error_msg =
        "Account was funded with %L, but at least %R was expected."
      in
      Check.(
        Tez.(mutez_int64 balance >= mutez_int64 initial_amount) int64 ~error_msg) ;
      unit)
    accounts

let register ~protocols = test_stresstest_fund_accounts protocols
