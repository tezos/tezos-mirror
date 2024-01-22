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

open Tezos_protocol_alpha

let default_sender = Protocol.Contract_hash.zero

let default_source = Tezos_crypto.Signature.Public_key_hash.zero

let default_destination = Protocol.Alpha_context.Sc_rollup.Address.zero

let default_preimage_directory = "preimage"

let default_dal_pages_directory = "dal_pages"

let default_kernel_debug = false

type config = {
  sender : Protocol.Contract_hash.t;
  source : Tezos_crypto.Signature.public_key_hash;
  destination : Protocol.Alpha_context.Sc_rollup.t;
  preimage_directory : string;
  dal_pages_directory : string;
  kernel_debug : bool;
}

let config ?(sender = default_sender) ?(source = default_source)
    ?(destination = default_destination)
    ?(preimage_directory = default_preimage_directory)
    ?(dal_pages_directory = default_dal_pages_directory)
    ?(kernel_debug = default_kernel_debug) () =
  {
    sender;
    source;
    destination;
    preimage_directory;
    dal_pages_directory;
    kernel_debug;
  }
