(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** [register_case case] registers a {!Profiler.kind} case

    [case] is a {!Profiler.view} i.e. a [View (('config, 'state) Profiler.kind)]
    that allows to gather all the cases of the extensible GADT {!Profiler.kind}
    in the same signature
*)
val register_case : Profiler.view Data_encoding.case -> unit

(** [kind_encoding] is the encoding of all the declared profiler kinds.

    Each time [register_case case] is called, [kind_encoding] is augmented with
    the registered [case] *)
val kind_encoding : Profiler.view Data_encoding.t
