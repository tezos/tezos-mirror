(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

type instance_maker =
  verbosity:Profiler.verbosity ->
  directory:string ->
  profiling_config:Profiler.profiling_config ->
  name:string ->
  Profiler.instance

type 'config driver = (module Profiler.DRIVER with type config = 'config)

type 'instance_maker backend_infos = {
  instance_maker : 'instance_maker;
  view : Profiler.view;
}

module BackendMap : Map.S with type key = string

val registered_backends : instance_maker backend_infos BackendMap.t ref

(** [register_backend identifiers instance_maker driver] associates one or more
    identifier(s) with a profiler instantiator.

    e.g.
    {[
    let () = register_backend ["json"] (profiler ~suffix:".json") write_to_json_file

    let () = register_backend ["text"; "txt"] (profiler ~suffix:".txt") write_to_txt_file
    ]}

    The identifiers are matched against the PROFILING_BACKENDS environment
    variable in order to be selected when profiling.

    This module registers two default profilers defined in {!Simple_profiler}.
    One for [PROFILING_BACKENDS=json] which outputs raw data to json files, and
    one for [PROFILING_BACKENDS=txt] and [PROFILING_BACKENDS=text] that outputs
    plain text formatted reports.

    Note that these profilers add [_profiling.txt] or [_profiling.json]
    suffix to the file name. *)
val register_backend :
  string list -> ('config driver -> instance_maker) -> 'config driver -> unit

type wrapped_instance_maker =
  directory:string ->
  profiling_config:Profiler.profiling_config ->
  name:string ->
  Profiler.instance option

(** [selected_backends ~profiling_config ()] returns the selected backends
    using [profiling_config] (falling back to the [PROFILING_BACKENDS]
    environment variable when the config field is unset). *)
val selected_backends :
  profiling_config:Profiler.profiling_config ->
  wrapped_instance_maker backend_infos list option

(** [read_profiling_config_from_base_dir base_dir] reads the profiling
    config from the [config] file in [base_dir]. Returns
    {!Profiler.default_profiling_config} if the file is missing or has
    no [profiling] section. *)
val read_profiling_config_from_base_dir : string -> Profiler.profiling_config
