(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

let tezos_codec = "./tezos-codec"

type key = {identity : string; alias : string; secret : string}

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

let all_secret_keys =
  [activator; bootstrap1; bootstrap2; bootstrap3; bootstrap4; bootstrap5]

(** The default burn for an implicit account. *)
let implicit_account_burn =
  (* as per the "origination_size" constant *)
  Tez.of_mutez_int 257_000
