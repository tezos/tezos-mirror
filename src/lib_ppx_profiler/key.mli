(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type content =
  | Empty
  | Ident of string
  | String of string
  | List of Ppxlib.expression list
  | Apply of Ppxlib.expression * (Ppxlib.arg_label * Ppxlib.expression) list
  | Other of Ppxlib.expression

(** How the profiler module is specified *)
type profiler_module =
  | Static of Longident.t  (** Static module path: Profiler, Foo.Bar *)
  | First_class of Ppxlib.expression
      (** First-class module expression: state.profiler, get_profiler () *)

type t = {
  verbosity : string option;
  profiler_module : profiler_module option;
  cpu_profiling : bool option;
  metadata : Ppxlib.expression option;
  content : content;
  driver_ids : Handled_drivers.t;
      (* Field used to decide if this attribute should be preprocessed or not
         based on the driver ids provided in the `TEZOS_PPX_PROFILER` env var.
         This is an opt-out field meaning that if the field is not provided
         the attribute will always be preprocessed when the ppx engine is
         enabled but if the field is provided it will only be preprocessed if
         one of the ids is also present in the drivers enabled by the env var.
      *)
}

val get_verbosity : Ppxlib.Location.t -> t -> Ppxlib.expression option

(** Returns the profiler module specification, defaulting to
    [Static (Lident "Profiler")] *)
val get_profiler_module : t -> profiler_module

val to_expression : Ppxlib.Location.t -> t -> Ppxlib.expression

val pp : Format.formatter -> t -> unit

val content : t -> content
