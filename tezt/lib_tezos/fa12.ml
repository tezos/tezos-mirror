(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type fa12_script = {
  name : string list;
  build_storage : Account.key -> string;
  mint_entrypoint : string;
  mint_arg : Account.key -> Tez.t -> string;
}

let fa12_reference =
  {
    name = ["mini_scenarios"; "fa12_reference"];
    build_storage =
      (fun admin ->
        sf {|Pair {} (Pair "%s" (Pair False 0))|} admin.public_key_hash);
    mint_entrypoint = "mint";
    mint_arg =
      (fun owner amount ->
        sf {|(Pair "%s" %d)|} owner.public_key_hash (Tez.to_mutez amount));
  }

let lqt_fa12 =
  {
    name = ["mini_scenarios"; "lqt_fa12.mligo"];
    build_storage =
      (fun admin -> sf {|Pair {} {} "%s" 0|} admin.public_key_hash);
    mint_entrypoint = "mintOrBurn";
    mint_arg =
      (fun owner amount ->
        sf {|(Pair %d "%s")|} (Tez.to_mutez amount) owner.public_key_hash);
  }

let fa12_scripts = [fa12_reference; lqt_fa12]

let originate_fa12 ~src ~admin ~fa12_script client protocol =
  let initial_storage = fa12_script.build_storage admin in
  Client.originate_contract_at
    ~amount:Tez.zero
    ~src
    ~init:initial_storage
    ~burn_cap:(Tez.of_int 2)
    client
    fa12_script.name
    protocol

let mint ~admin ~mint ~dest ~fa12_address ~fa12_script client =
  let mint_arg = fa12_script.mint_arg dest mint in
  Client.transfer
    ~amount:Tez.zero
    ~giver:admin.Account.public_key_hash
    ~receiver:fa12_address
    ~entrypoint:fa12_script.mint_entrypoint
    ~arg:mint_arg
    ~burn_cap:Tez.one
    client
