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

module SMap = Map.Make (String)

type static = {
  public_parameters : Plonk.public_parameters;
  state_length : int;
  circuits_info : bool SMap.t;
  nb_ops : int;
}

type dynamic = {state : Zk_rollup_state_repr.t}

type t = {static : static; dynamic : dynamic}

let encoding =
  let open Data_encoding in
  let static_encoding =
    let circuits_info_encoding =
      conv
        SMap.bindings
        (fun l -> SMap.of_seq @@ List.to_seq l)
        (list (tup2 string bool))
    in
    conv
      (fun {public_parameters; state_length; circuits_info; nb_ops} ->
        (public_parameters, state_length, circuits_info, nb_ops))
      (fun (public_parameters, state_length, circuits_info, nb_ops) ->
        {public_parameters; state_length; circuits_info; nb_ops})
      (obj4
         (req "public_parameters" Plonk.public_parameters_encoding)
         (req "state_length" int31)
         (req "circuits_info" circuits_info_encoding)
         (req "nb_ops" int31))
  in
  let dynamic_encoding =
    conv
      (fun {state} -> state)
      (fun state -> {state})
      (obj1 (req "state" Zk_rollup_state_repr.encoding))
  in
  conv
    (fun {static; dynamic} -> (static, dynamic))
    (fun (static, dynamic) -> {static; dynamic})
    (obj2 (req "static" static_encoding) (req "dynamic" dynamic_encoding))
