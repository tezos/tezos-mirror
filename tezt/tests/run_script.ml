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

(* This script checks it's own address or balance against the expected value
   given as parameter. The parameter tells what should be tested and what
   the expected value is. If the actual state matches the expected one,
   the contract succeeds, otherwise it fails with a pair consisting of the
   expected and the actual values. *)
let prg =
  {|
parameter (or (mutez %check_balance) (address %check_self_address)) ;
storage unit ;
code {
       UNPAIR ;
       IF_LEFT {
                 DUP ;
                 BALANCE ;
                 IFCMPEQ { DROP }
                         {
                           PUSH string "expected" ;
                           PAIR ;
                           BALANCE ;
                           PUSH string "actual" ;
                           PAIR ;
                           PAIR ;
                           PUSH string "Unexpected BALANCE";
                           PAIR ;
                           FAILWITH ;
                         } ;
               }
               {
                 DUP ;
                 SELF_ADDRESS ;
                 IFCMPEQ { DROP }
                         {
                           PUSH string "expected" ;
                           PAIR ;
                           SELF_ADDRESS;
                           PUSH string "actual" ;
                           PAIR ;
                           PAIR ;
                           PUSH string "Unexpected SELF_ADDRESS";
                           PAIR ;
                           FAILWITH ;
                         } ;
               };
       NIL operation ;
       PAIR ;
     }
|}

let register ~protocol () =
  Test.register ~__FILE__ ~title:"Run script" ~tags:["client"; "michelson"]
  @@ fun () ->
  let* client = Client.init_mockup ~protocol () in
  (* With no parameters, the default BALANCE is 4 000 000 ꜩ. *)
  let* _storage =
    Client.run_script ~prg ~storage:"Unit" ~input:"Left 4000000000000" client
  in

  (* When --balance is given, BALANCE should match the expected value. *)
  let* _storage =
    Client.run_script
      ~balance:(Tez.of_int 1)
      ~prg
      ~storage:"Unit"
      ~input:"Left 1000000"
      client
  in

  let* self_address =
    Client.originate_contract
      ~burn_cap:(Tez.of_int 1)
      ~alias:"test_contract"
      ~amount:(Tez.of_int 100)
      ~src:"bootstrap1"
      ~prg
      client
  in

  (* When --self-address is given, SELF_ADDRESS should match the given. *)
  let* _storage =
    Client.run_script
      ~self_address
      ~prg
      ~storage:"Unit"
      ~input:(Format.sprintf "Right %S" self_address)
      client
  in
  (* When --self-address is given, BALANCE should be equal to that of the
     given account. *)
  let* _storage =
    Client.run_script
      ~self_address
      ~prg
      ~storage:"Unit"
      ~input:"Left 100000000"
      client
  in

  (* When both --self-address and --balance are given, the BALANCE should be
     equal to the given value and SELF_ADDRESS should still match the given. *)
  let* _storage =
    Client.run_script
      ~balance:(Tez.of_int 1)
      ~self_address
      ~prg
      ~storage:"Unit"
      ~input:(Format.sprintf "Right %S" self_address)
      client
  in
  let* _storage =
    Client.run_script
      ~balance:(Tez.of_int 1)
      ~self_address
      ~prg
      ~storage:"Unit"
      ~input:"Left 1000000"
      client
  in
  unit

let register ~protocols =
  List.iter
    (function
      | Protocol.Alpha as protocol -> register ~protocol ()
      | Protocol.Hangzhou | Protocol.Ithaca -> ())
    (* Won't work prior to protocol J. *)
    protocols
