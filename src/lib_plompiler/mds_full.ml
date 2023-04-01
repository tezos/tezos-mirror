(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(* Come from https://github.com/zcash-hackworks/zcash-test-vectors/blob/master/orchard_poseidon.py *)

let v =
  [|
    [|
      "20318286217486369426965035069836800746749435906016448674636714520170817018807";
      "22192852525643738215947851702151544501993043677579072964990735260534263737314";
      "8542598184567193950383006010698963992683774969143163395442055984920670969658";
    |];
    [|
      "7392954993144188510255764707695079827771707557363197189129035318593453738368";
      "27549511735195789329533968803664746807330189702821962130064027535490056177413";
      "13129156644346446682975824711515160071389200876395558249729931114712818796177";
    |];
    [|
      "21010686366472749505451494418483718181134873748085457288199401256016616439986";
      "28561190888145054647914513058959252600378056095166705432358200159539002856405";
      "21356544248278733708505686717446976476730796312725621196632235950424803603792";
    |];
  |]
