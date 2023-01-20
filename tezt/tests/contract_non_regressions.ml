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
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_non_regressions.ml
   Subject:      Test contract-related non-regressions
*)

(* Normally "--base-dir" would appear in regression logs. However, since
   it is a different dir on every run, we need to mask it in regression
   logs so that it doesn't cause false differences. *)
let hooks = Tezos_regression.hooks

let register262 =
  Protocol.register_test
    ~__FILE__
    ~title:"Contract-related non-regressions, Issue 262"
    ~tags:["client"; "michelson"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* _alias, contract =
    Client.originate_contract_at
      ~hooks
      ~burn_cap:Tez.one
      ~amount:Tez.one
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      client
      ["non_regression"; "262_bug"]
      protocol
  in
  let* balance = Client.get_balance_for ~account:contract client in
  Check.(balance = Tez.one)
    ~__LOC__
    Tez.typ
    ~error_msg:"expected balance %R, got %L" ;
  unit

let register843 =
  Protocol.register_test
    ~__FILE__
    ~title:"Contract-related non-regressions, Issue 843"
    ~tags:["client"; "michelson"]
    ~supports:(Protocol.From_protocol 015)
  @@ fun protocol ->
  (* https://gitlab.com/tezos/tezos/-/issues/843

     This test checks that before origination the script, storage, and
     the lambdas inside the storage are all normalized. To test this
     we define them in readable mode and compare the storage size of
     the origination operations when the readable script and storage
     are used directly and when they are first normalized to optimized
     format before origination. *)
  let* client = Client.init_mockup ~protocol () in
  let addr = {|"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"|} in
  let bug843 =
    Michelson_script.(find ["non_regression"; "843_bug"] protocol |> path)
  in
  let* contract1 =
    Client.originate_contract
      ~hooks
      ~burn_cap:Tez.one
      ~alias:"contract1"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~prg:(sf "file:%s" bug843)
      ~init:(sf "Pair %s {PUSH address %s; DROP}" addr addr)
      client
  in
  let* normalized_script =
    Client.normalize_script ~script:(read_file bug843) ~mode:Optimized client
  in
  let* normalized_addr =
    Client.normalize_data ~data:addr ~typ:"address" ~mode:Optimized client
  in
  let normalized_addr = String.trim normalized_addr in
  let* contract2 =
    Client.originate_contract
      ~hooks
      ~burn_cap:Tez.one
      ~alias:"contract2"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~prg:normalized_script
      ~init:
        (sf "Pair %s {PUSH address %s; DROP}" normalized_addr normalized_addr)
      client
  in
  let* storage_size1 = Client.used_storage_space ~contract:contract1 client in
  let* storage_size2 = Client.used_storage_space ~contract:contract2 client in
  Check.(storage_size1 = storage_size2)
    ~__LOC__
    Check.string
    ~error_msg:"expected equal storage size, got %L <> %R" ;
  unit

let register protocols =
  register262 protocols ;
  register843 protocols
