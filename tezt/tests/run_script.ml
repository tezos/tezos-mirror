(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Client
   Invocation:   dune exec tezt/tests/main.exe -- --file run_script.ml
   Subject:      Check that run script command to tezos-client behaves correctly
 *)

(* This script checks result of some arbitrary instruction against the
   expected value. Return type and name of the instruction should be
   given by arguments [ty] and [instr] respectively.  The expected
   value should be passed to the contract as parameter. If the actual
   state matches the expected one, the contract succeeds, otherwise it
   fails with a pair consisting of the expected and the actual
   values. *)
let prg_template : ('a -> 'b -> 'c -> 'd, unit, string) format =
  {|
parameter %s ; /* type */
storage unit ;
code {
       UNPAIR ;
       DUP ;
       %s ; /* instr */
       IFCMPEQ { DROP }
               {
                 PUSH string "expected" ;
                 PAIR ;
                 %s; /* instr */
                 PUSH string "actual" ;
                 PAIR ;
                 PAIR ;
                 FAILWITH ;
               } ;
       NIL operation ;
       PAIR ;
     }
|}

let prg ty instr = Format.sprintf prg_template ty instr instr

let check_balance = prg "mutez" "BALANCE"

let check_self_address = prg "address" "SELF_ADDRESS"

let check_sender = prg "address" "SENDER"

let check_source = prg "address" "SOURCE"

let test_balance_and_self_address ~protocol () =
  let* client = Client.init_mockup ~protocol () in
  (* With no parameters, the default BALANCE is 4 000 000 ꜩ. *)
  let* _storage =
    Client.run_script
      ~prg:check_balance
      ~storage:"Unit"
      ~input:"4000000000000"
      client
  in

  (* When --balance is given, BALANCE should match the expected value. *)
  let* _storage =
    Client.run_script
      ~balance:(Tez.of_int 1)
      ~prg:check_balance
      ~storage:"Unit"
      ~input:"1000000"
      client
  in

  let* self_address =
    Client.originate_contract
      ~burn_cap:(Tez.of_int 1)
      ~alias:"test_contract"
      ~amount:(Tez.of_int 100)
      ~src:"bootstrap1"
      ~prg:check_self_address
      client
  in

  (* When --self-address is given, SELF_ADDRESS should match the given. *)
  let* _storage =
    Client.run_script
      ~self_address
      ~prg:check_self_address
      ~storage:"Unit"
      ~input:(Format.sprintf "%S" self_address)
      client
  in
  (* When --self-address is given, BALANCE should be equal to that of the
     given account. *)
  let* _storage =
    Client.run_script
      ~self_address
      ~prg:check_balance
      ~storage:"Unit"
      ~input:"100000000"
      client
  in

  (* When both --self-address and --balance are given, the BALANCE should be
     equal to the given value and SELF_ADDRESS should still match the given. *)
  let* _storage =
    Client.run_script
      ~balance:(Tez.of_int 1)
      ~self_address
      ~prg:check_self_address
      ~storage:"Unit"
      ~input:(Format.sprintf "%S" self_address)
      client
  in
  let* _storage =
    Client.run_script
      ~balance:(Tez.of_int 1)
      ~self_address
      ~prg:check_balance
      ~storage:"Unit"
      ~input:"1000000"
      client
  in
  unit

let test_source_and_sender ~protocol () =
  let* client = Client.init_mockup ~protocol () in
  let* bootstrap1 = Client.show_address ~alias:"bootstrap1" client in
  let* bootstrap2 = Client.show_address ~alias:"bootstrap2" client in

  (* When --payer is absent, --source sets *both* SENDER and SOURCE. *)
  let* _storage =
    Client.run_script
      ~source:"bootstrap1"
      ~prg:check_source
      ~storage:"Unit"
      ~input:(Format.sprintf "%S" bootstrap1.public_key_hash)
      client
  in
  let* _storage =
    Client.run_script
      ~source:"bootstrap1"
      ~prg:check_sender
      ~storage:"Unit"
      ~input:(Format.sprintf "%S" bootstrap1.public_key_hash)
      client
  in

  (* When --source is absent, --payer sets *both* SENDER and SOURCE. *)
  let* _storage =
    Client.run_script
      ~payer:"bootstrap1"
      ~prg:check_source
      ~storage:"Unit"
      ~input:(Format.sprintf "%S" bootstrap1.public_key_hash)
      client
  in
  let* _storage =
    Client.run_script
      ~payer:"bootstrap1"
      ~prg:check_sender
      ~storage:"Unit"
      ~input:(Format.sprintf "%S" bootstrap1.public_key_hash)
      client
  in

  (* When both --source and --payer are given, their values may differ. *)
  let* _storage =
    Client.run_script
      ~payer:"bootstrap1"
      ~source:"bootstrap2"
      ~prg:check_source
      ~storage:"Unit"
      ~input:(Format.sprintf "%S" bootstrap1.public_key_hash)
      client
  in
  let* _storage =
    Client.run_script
      ~payer:"bootstrap1"
      ~source:"bootstrap2"
      ~prg:check_sender
      ~storage:"Unit"
      ~input:(Format.sprintf "%S" bootstrap2.public_key_hash)
      client
  in
  unit

let make_for ~protocol () =
  List.iter
    (fun (title, f) ->
      Test.register ~__FILE__ ~title ~tags:["client"; "michelson"] f)
    [
      ( "Run script with balance and self address",
        test_balance_and_self_address ~protocol );
      ("Run script with source and sender", test_source_and_sender ~protocol);
    ]

let register ~protocols =
  List.iter
    (function
      | (Protocol.Alpha | Protocol.Jakarta) as protocol -> make_for ~protocol ()
      | Protocol.Ithaca -> ())
    (* Won't work prior to protocol J. *)
    protocols
