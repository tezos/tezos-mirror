(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Os : sig
  type t = Cos | Debian

  (** [default] is [Cos]. *)
  val default : t

  val of_string_exn : string -> t

  val of_string_opt : string -> t option

  val to_string : t -> string

  val typ : t Clap.typ

  val encoding : t Data_encoding.encoding
end

module Agent_configuration : sig
  type docker_image =
    | Gcp of {alias : string}
    | Octez_release of {tag : string}

  val default_docker_image : tezt_cloud:string -> docker_image

  type vm = private {
    machine_type : string;
    disk_type : string option;
    disk_size_gb : int option;
    docker_image : docker_image;
    dockerbuild_args : (string * string) list;
    max_run_duration : int option;
    binaries_path : string;
    os : Os.t;
  }

  type t = {name : string; vm : vm}

  val encoding : t Data_encoding.encoding

  val default_gcp_machine_type : string

  val default_gcp_binaries_path : string

  val default_max_run_duration : int option

  val make :
    os:Os.t ->
    binaries_path:string ->
    ?max_run_duration:int ->
    ?disk_type:string ->
    ?disk_size_gb:int ->
    ?dockerbuild_args:(string * string) list ->
    machine_type:string ->
    docker_image:docker_image ->
    name:string ->
    unit ->
    t
end

type notifier =
  | Notifier_null
  | Notifier_slack of {
      name : string;
      slack_bot_token : string;
      slack_channel_id : string;
    }
