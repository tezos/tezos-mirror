(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

val default_persistent_signature_loader :
  allow_hidden:bool ->
  unit_name:string ->
  Persistent_env.Persistent_signature.t option

(** [set_persistent_signature_loader f] calls
    `Persistent_env.Persistent_signature.load := f`, but handles the necessary
    wrapping for it to work on different version of OCaml. *)
val override_persistent_signature_loader :
  (allow_hidden:bool ->
  unit_name:string ->
  Persistent_env.Persistent_signature.t option) ->
  unit

(** [make_persistent_signature ~filename ~cmi] wraps `Persistent_signature.t` to be
    compatible with multiple version of OCaml. *)
val make_persistent_signature :
  filename:string ->
  cmi:Cmi_format.cmi_infos ->
  Persistent_env.Persistent_signature.t

(** [compunit_name cu] returns the name of the compilation unit *)
val compunit_name : Cmo_format.compilation_unit -> string

(** [mark_attribute_used attr] marks the attribute used, so that it doesn't
    trigger warning 53. See [Packer.check_syntax]. *)
val mark_attribute_used : Parsetree.attribute -> unit
