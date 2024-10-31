(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let default_persistent_signature_loader =
  let load = !Persistent_env.Persistent_signature.load in
  fun ~allow_hidden:_ -> load

let override_persistent_signature_loader f =
  Persistent_env.Persistent_signature.load := f ~allow_hidden:true
(* [allow_hidden] will be ignored in the case of OCaml 4 compiler, as the
   visibility parameter doesn't exist.*)

let make_persistent_signature ~filename ~cmi =
  Persistent_env.Persistent_signature.{filename; cmi}

let compunit_name compunit = compunit.Cmo_format.cu_name

let mark_attribute_used _ = ()
