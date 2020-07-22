(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let tezos_client = "./tezos-client"

let tezos_admin_client = "./tezos-admin-client"

let tezos_node = "./tezos-node"

type protocol = {hash : string; parameter_file : string}

let alpha =
  {
    hash = "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK";
    parameter_file = "src/proto_alpha/parameters/sandbox-parameters.json";
  }

type key = {identity : string; alias : string; secret : string}

type baker = {identity : string; alias : string}

let activator =
  {
    identity = "";
    (* FIXME: could be computed *)
    alias = "activator";
    secret =
      "unencrypted:edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6";
  }

let bootstrap1 =
  {
    identity = "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx";
    alias = "bootstrap1";
    secret =
      "unencrypted:edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh";
  }

let bootstrap2 =
  {
    identity = "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN";
    alias = "bootstrap2";
    secret =
      "unencrypted:edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo";
  }

let bootstrap3 =
  {
    identity = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU";
    alias = "bootstrap3";
    secret =
      "unencrypted:edsk4ArLQgBTLWG5FJmnGnT689VKoqhXwmDPBuGx3z4cvwU9MmrPZZ";
  }

let bootstrap4 =
  {
    identity = "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv";
    alias = "bootstrap4";
    secret =
      "unencrypted:edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3";
  }

let bootstrap5 =
  {
    identity = "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv";
    alias = "bootstrap5";
    secret =
      "unencrypted:edsk4QLrcijEffxV31gGdN2HU7UpyJjA8drFoNcmnB28n89YjPNRFm";
  }

let baker1_key =
  {
    identity = "tz1UJbFsTQfdpa1qYrqHG9Ykw834AXi5WqRR";
    alias = "baker1_key";
    secret =
      "unencrypted:edsk432L71B91i1sE8rQxPDMo2Yxo4qaYqhktvpt8yovaMpo1NUbBt";
  }

let baker1 =
  {identity = "SG1fpFaowYY8G7PfkYdKkGmsMziHKUfrHRHW"; alias = "baker1"}

let baker2_key =
  {
    identity = "tz1fWJ2oeeG6h2r61XY1wen2sV87AvCqGjAU";
    alias = "baker2_key";
    secret =
      "unencrypted:edsk3jjD4cLvE1t3SiZKCRiH9SXnJ4jHEsVpfcuRwfe7sfbnsMGX6f";
  }

let baker2 =
  {identity = "SG1TLmKJHVJxQosY6iN21AW77HsAapdupxnR"; alias = "baker2"}

let baker3_key =
  {
    identity = "tz1SERFDUknkjJTas8sUco7dskxWrUWhTc2s";
    alias = "baker3_key";
    secret =
      "unencrypted:edsk3EVDFeDsefk1UoWWhDhnBpERhAwqHbVzWnBwA4tdUL8aBhw7RC";
  }

let baker3 =
  {identity = "SG1hExdK69Z2RZkkQjKtLG6H4L4FGTZeGKHu"; alias = "baker3"}

let baker4_key =
  {
    identity = "tz1eJ9yR95LmJgVrGLvgt56T6ngCFcgjLo2S";
    alias = "baker4_key";
    secret =
      "unencrypted:edsk2rAmeBDrQ5d1FhDoTdifMvBsqSosmPSBhxL74huszZhfMFtiKb";
  }

let baker4 =
  {identity = "SG1mHgeWHGMnCUMJ8jZ1Cdh3DkWEcQ88tziJ"; alias = "baker4"}

let baker5_key =
  {
    identity = "tz1cwoRFe3BzJv9nZZBA5fk8uV6w9AVVrBF9";
    alias = "baker5_key";
    secret =
      "unencrypted:edsk4Ssc63dnYJUptVMaKfmHLJNMBYv9piYiRCiYjcH3gacKWP1z3v";
  }

let baker5 =
  {identity = "SG1jfZeHRzeWAM1T4zrwunEyUpwWc82D4tbv"; alias = "baker5"}

let all_secret_keys : key list =
  [ activator;
    bootstrap1;
    bootstrap2;
    bootstrap3;
    bootstrap4;
    bootstrap5;
    baker1_key;
    baker2_key;
    baker3_key;
    baker4_key;
    baker5_key ]

let all_bakers : baker list = [baker1; baker2; baker3; baker4; baker5]
