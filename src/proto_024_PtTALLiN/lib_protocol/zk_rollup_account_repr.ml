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
  circuits_info : [`Public | `Private | `Fee] SMap.t;
  nb_ops : int;
}

type dynamic = {
  state : Zk_rollup_state_repr.t;
  paid_l2_operations_storage_space : Z.t;
  used_l2_operations_storage_space : Z.t;
}

type t = {static : static; dynamic : dynamic}

let circuits_info_encoding : [`Public | `Private | `Fee] SMap.t Data_encoding.t
    =
  let open Data_encoding in
  let variant_encoding =
    let public_tag, public_encoding = (0, obj1 @@ req "public" unit) in
    let private_tag, private_encoding = (1, obj1 @@ req "private" unit) in
    let fee_tag, fee_encoding = (2, obj1 @@ req "fee" unit) in
    matching
      (function
        | `Public -> matched public_tag public_encoding ()
        | `Private -> matched private_tag private_encoding ()
        | `Fee -> matched fee_tag fee_encoding ())
      [
        case
          ~title:"Public"
          (Tag public_tag)
          public_encoding
          (function `Public -> Some () | _ -> None)
          (fun () -> `Public);
        case
          ~title:"Private"
          (Tag private_tag)
          private_encoding
          (function `Private -> Some () | _ -> None)
          (fun () -> `Private);
        case
          ~title:"Fee"
          (Tag fee_tag)
          fee_encoding
          (function `Fee -> Some () | _ -> None)
          (fun () -> `Fee);
      ]
  in
  conv_with_guard
    (fun m -> List.of_seq @@ SMap.to_seq m)
    (fun l ->
      let m = SMap.of_seq @@ List.to_seq l in
      if
        (* Check that the list has no duplicated keys *)
        Compare.List_length_with.(l <> SMap.cardinal m)
      then Error "Zk_rollup_origination: circuits_info has duplicated keys"
      else Ok m)
    (list (tup2 (string Plain) variant_encoding))

let encoding =
  let open Data_encoding in
  let static_encoding =
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
      (fun {
             state;
             paid_l2_operations_storage_space;
             used_l2_operations_storage_space;
           }
         ->
        ( state,
          paid_l2_operations_storage_space,
          used_l2_operations_storage_space ))
      (fun ( state,
             paid_l2_operations_storage_space,
             used_l2_operations_storage_space )
         ->
        {
          state;
          paid_l2_operations_storage_space;
          used_l2_operations_storage_space;
        })
      (obj3
         (req "state" Zk_rollup_state_repr.encoding)
         (req "paid_l2_operations_storage_space" n)
         (req "used_l2_operations_storage_space" n))
  in
  conv
    (fun {static; dynamic} -> (static, dynamic))
    (fun (static, dynamic) -> {static; dynamic})
    (obj2 (req "static" static_encoding) (req "dynamic" dynamic_encoding))
