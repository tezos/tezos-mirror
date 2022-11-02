(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

type t = {
  b58check_prefix : string;
  prefix : string;
  hash_size : int;
  b58check_size : int;
}

let rollup_address =
  {
    b58check_prefix = "\001\128\120\031";
    prefix = "txr1";
    hash_size = 20;
    b58check_size = 37;
  }

let inbox_hash =
  {
    b58check_prefix = "\079\148\196";
    prefix = "txi";
    hash_size = 32;
    b58check_size = 53;
  }

let inbox_list_hash = inbox_hash

let message_hash =
  {
    b58check_prefix = "\079\149\030";
    prefix = "txm";
    hash_size = 32;
    b58check_size = 53;
  }

let commitment_hash =
  {
    b58check_prefix = "\079\148\017";
    prefix = "txc";
    hash_size = 32;
    b58check_size = 53;
  }

let message_result_hash =
  {
    b58check_prefix = "\018\007\206\087";
    prefix = "txmr";
    hash_size = 32;
    b58check_size = 54;
  }

let message_result_list_hash =
  {
    b58check_prefix = "\079\146\082";
    prefix = "txM";
    hash_size = 32;
    b58check_size = 53;
  }

let withdraw_list_hash =
  {
    b58check_prefix = "\079\150\072";
    prefix = "txw";
    hash_size = 32;
    b58check_size = 53;
  }

let check_encoding {prefix; b58check_size; _} encoding =
  Base58.check_encoded_prefix encoding prefix b58check_size
