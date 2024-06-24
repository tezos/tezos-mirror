(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let encoding_with_optional_last_param encoding second_param_encoding
    default_second_param =
  let open Data_encoding in
  let encoding = if is_tup encoding then encoding else tup1 encoding in
  union
    [
      case
        ~title:"with_second_param"
        (Tag 0)
        (merge_tups encoding (tup1 second_param_encoding))
        (fun (t, second_param) -> Some (t, second_param))
        (fun (t, second_param) -> (t, second_param));
      case
        ~title:"without_second_param"
        (Tag 1)
        encoding
        (fun (t, _) -> Some t)
        (fun t -> (t, default_second_param));
    ]
