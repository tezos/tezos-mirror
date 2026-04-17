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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/3898
   We also need  dynamic metadatas. *)

type t = {
  address : Sc_rollup_repr.Address.t;
  origination_level : Raw_level_repr.t;
}

let pp ppf {address; origination_level} =
  Format.fprintf
    ppf
    "address: %a ; origination_level: %a"
    Sc_rollup_repr.Address.pp
    address
    Raw_level_repr.pp
    origination_level

let equal {address; origination_level} metadata2 =
  Sc_rollup_repr.Address.equal address metadata2.address
  && Raw_level_repr.equal origination_level metadata2.origination_level

let encoding =
  let open Data_encoding in
  conv
    (fun {address; origination_level} -> (address, origination_level))
    (fun (address, origination_level) -> {address; origination_level})
    (obj2
       (req "address" Sc_rollup_repr.Address.encoding)
       (req "origination_level" Raw_level_repr.encoding))
