(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_liquility_baking.ml
   Subject:      Regression testing of liquility baking contracts
*)

(* Using the lighter hook that only scrubs the clients [--base-dir] *)
let hooks =
  Tezos_regression.hooks_custom
    ~scrubbed_global_options:["--base-dir"; "-d"]
    ~replace_variables:Fun.id
    ()

let dex = "KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5"

let tok = "KT1VqarPDicMFn1ejmQqqshUkUXTCTXwmkCN"

let lqt = "KT1AafHA1C1vk959wvHWBispY9Y2f3fxBUUo"

let future = "2050-01-01T00:00:00Z"

let split_storage storage =
  String.trim storage
  |> Base.replace_string ~all:true (rex "\\s+") ~by:" "
  |> String.split_on_char ' '

let unquote s =
  match String.split_on_char '"' s with
  | [""; s; ""] -> s
  | _ -> Test.fail "[unquote] expected a double-quoted string, got: %S" s

type tok_storage = {
  tokens : int;
  allowances : int;
  admin : string;
  total_supply : int;
}

type dex_storage = {
  token_pool : int;
  xtz_pool : int;
  lqt_total : int;
  token_address : string;
  lqt_address : string;
}

let get_tok_storage ~hooks client contract =
  let* storage = Client.contract_storage ~hooks contract client in
  match split_storage storage with
  | ["Pair"; tokens; allowances; admin; total_supply] ->
      return
        {
          tokens = int_of_string tokens;
          allowances = int_of_string allowances;
          admin = unquote admin;
          total_supply = int_of_string total_supply;
        }
  | _ ->
      Test.fail "Unparseable token contract storage in %S: %S," contract storage

let get_dex_storage ~hooks client contract =
  let* storage = Client.contract_storage ~hooks contract client in
  match split_storage storage with
  | ["Pair"; token_pool; xtz_pool; lqt_total; token_address; lqt_address] ->
      return
        {
          token_pool = int_of_string token_pool;
          xtz_pool = int_of_string xtz_pool;
          lqt_total = int_of_string lqt_total;
          token_address = unquote token_address;
          lqt_address = unquote lqt_address;
        }
  | _ ->
      Test.fail "Unparseable dex contract storage in %S: %S," contract storage

let setup_mint_and_approve ~__LOC__ client =
  let* dex_address =
    Client.RPC.call client ~hooks
    @@ RPC.get_chain_block_context_liquidity_baking_cpmm_address ()
  in
  Log.info "Check DEX address" ;
  let () =
    Check.(
      (String.trim dex_address = dex)
        string
        ~__LOC__
        ~error_msg:"Expected storage %R, got %L")
  in
  Log.info "Check DEX storage" ;
  let* dex_storage = get_dex_storage client ~hooks dex in
  let () =
    Check.(
      (dex_storage.token_address = tok)
        string
        ~__LOC__
        ~error_msg:"Expected storage %R, got %L")
  in
  let () =
    Check.(
      (dex_storage.lqt_address = lqt)
        string
        ~__LOC__
        ~error_msg:"Expected storage %R, got %L")
  in
  (* mint some test TOK (1 tzBTC?) for ourselves*)
  Log.info "Call mint or burn" ;
  let arg = sf "(Pair 100000000 %S)" Constant.bootstrap1.public_key_hash in
  let* () =
    Client.call_contract
      ~hooks
      ~src:Constant.bootstrap1.alias
      ~destination:tok
      ~entrypoint:"mintOrBurn"
      ~arg
      ~burn_cap:(Tez.of_int 10)
      client
  in
  (* pre-approve big allowances in TOK for DEX *)
  Log.info "Call approve1" ;
  let arg = sf "(Pair %S 1000000000)" dex in
  let* () =
    Client.call_contract
      ~hooks
      ~src:Constant.bootstrap1.alias
      ~destination:tok
      ~entrypoint:"approve"
      ~arg
      ~burn_cap:(Tez.of_int 10)
      client
  in
  Log.info "Call approve2" ;
  let arg = sf "(Pair %S 1000000000)" dex in
  let* () =
    Client.call_contract
      ~hooks
      ~src:Constant.bootstrap2.alias
      ~destination:tok
      ~entrypoint:"approve"
      ~arg
      ~burn_cap:(Tez.of_int 10)
      client
  in
  Log.info "Call approve3" ;
  let arg = sf "(Pair %S 1000000000)" dex in
  let* () =
    Client.call_contract
      ~hooks
      ~src:Constant.bootstrap3.alias
      ~destination:tok
      ~entrypoint:"approve"
      ~arg
      ~burn_cap:(Tez.of_int 10)
      client
  in
  unit

let register_add_approve_transfer_remove =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Test add/approve/transfer/remove liquidity"
    ~tags:["client"; "michelson"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* () = setup_mint_and_approve ~__LOC__ client in
  Log.info "First add liquidity on DEX so that we have some LQT" ;
  let arg =
    sf "(Pair %S 0 1000000000 %S)" Constant.bootstrap1.public_key_hash future
  in
  let* () =
    Client.transfer
      ~hooks
      ~burn_cap:(Tez.of_int 10)
      ~amount:(Tez.of_int 9001)
      ~giver:"bootstrap1"
      ~receiver:dex
      ~arg
      ~entrypoint:"addLiquidity"
      client
  in
  Log.info "Test LQT approval" ;
  let arg = sf "(Pair %S 1000)" Constant.bootstrap2.public_key_hash in
  let* () =
    Client.call_contract
      ~hooks
      ~src:Constant.bootstrap1.alias
      ~destination:lqt
      ~entrypoint:"approve"
      ~arg
      ~burn_cap:(Tez.of_int 10)
      client
  in
  Log.info "Transfer LQT to bootstrap2 (using approval)" ;
  let arg =
    sf
      "(Pair %S %S 1000)"
      Constant.bootstrap1.public_key_hash
      Constant.bootstrap2.public_key_hash
  in
  let* () =
    Client.call_contract
      ~hooks
      ~src:Constant.bootstrap2.alias
      ~destination:lqt
      ~entrypoint:"transfer"
      ~arg
      ~burn_cap:(Tez.of_int 10)
      client
  in
  Log.info "Now remove LQT for bootstrap2" ;
  let arg =
    sf "(Pair %S 1000 0 0 %S)" Constant.bootstrap2.public_key_hash future
  in
  let* () =
    Client.call_contract
      ~hooks
      ~src:Constant.bootstrap2.alias
      ~destination:dex
      ~entrypoint:"removeLiquidity"
      ~arg
      ~burn_cap:(Tez.of_int 10)
      client
  in
  Log.info "Check LQT storage" ;
  let* _lqt_storage = get_tok_storage client ~hooks lqt in
  Log.info "Check TOK storage" ;
  let* _tok_storage = get_tok_storage client ~hooks tok in
  unit

let register_trades =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"Test trades"
    ~tags:["client"; "michelson"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* () = setup_mint_and_approve ~__LOC__ client in
  Log.info "Add liquidity" ;
  let arg =
    sf "(Pair %S 0 1000000000 %S)" Constant.bootstrap1.public_key_hash future
  in
  let* () =
    Client.transfer
      ~hooks
      ~burn_cap:(Tez.of_int 10)
      ~amount:(Tez.of_int 9001)
      ~giver:"bootstrap1"
      ~receiver:dex
      ~arg
      ~entrypoint:"addLiquidity"
      client
  in
  Log.info "Bootstrap2 buys some TOK" ;
  let arg = sf "(Pair %S 0 %S)" Constant.bootstrap2.public_key_hash future in
  let* () =
    Client.transfer
      ~hooks
      ~burn_cap:(Tez.of_int 10)
      ~amount:(Tez.of_int 9001)
      ~giver:"bootstrap2"
      ~receiver:dex
      ~arg
      ~entrypoint:"xtzToToken"
      client
  in
  Log.info "Bootstrap2 transfers TOK to bootstrap3" ;
  let arg =
    sf
      "(Pair %S %S 100)"
      Constant.bootstrap2.public_key_hash
      Constant.bootstrap3.public_key_hash
  in
  let* () =
    Client.call_contract
      ~hooks
      ~src:Constant.bootstrap2.alias
      ~destination:tok
      ~entrypoint:"transfer"
      ~arg
      ~burn_cap:(Tez.of_int 10)
      client
  in
  Log.info "Boostrap3 sells TOK" ;
  let arg =
    sf "(Pair %S 100 0 %S)" Constant.bootstrap3.public_key_hash future
  in
  let* () =
    Client.call_contract
      ~hooks
      ~src:Constant.bootstrap3.alias
      ~destination:dex
      ~entrypoint:"tokenToXtz"
      ~arg
      ~burn_cap:(Tez.of_int 10)
      client
  in
  Log.info "Check LQT storage" ;
  let* _storage_lqt = get_tok_storage client ~hooks lqt in
  Log.info "Check TOK storage" ;
  let* _tok_storage = get_tok_storage client ~hooks tok in
  unit

let register ~protocols =
  register_add_approve_transfer_remove protocols ;
  register_trades protocols
