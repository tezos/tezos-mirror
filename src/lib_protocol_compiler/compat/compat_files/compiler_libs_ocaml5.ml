(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let default_persistent_signature_loader =
  !Persistent_env.Persistent_signature.load

let override_persistent_signature_loader f =
  Persistent_env.Persistent_signature.load := f

let make_persistent_signature ~filename ~cmi =
  Persistent_env.Persistent_signature.
    {filename; cmi; visibility = Load_path.Visible}

let compunit_name compunit =
  let (Cmo_format.Compunit cuname) = compunit.Cmo_format.cu_name in
  cuname

let mark_attribute_used _ = ()
