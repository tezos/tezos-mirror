(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Os = struct
  type t = Cos | Debian

  let default = Cos

  let of_string_exn = function
    | "cos" -> Cos
    | "debian" -> Debian
    | _ -> invalid_arg "Os.of_string"

  let of_string_opt str =
    try of_string_exn str |> Option.some with Invalid_argument _ -> None

  let to_string = function Cos -> "cos" | Debian -> "debian"

  let typ = Clap.typ ~name:"os" ~dummy:Cos ~parse:of_string_opt ~show:to_string

  let encoding =
    let open Data_encoding in
    conv to_string of_string_exn string
end

module Agent_configuration = struct
  type docker_image =
    | Gcp of {alias : string}
    | Octez_release of {tag : string}

  type vm = {
    machine_type : string;
    docker_image : docker_image;
    max_run_duration : int option;
    binaries_path : string;
    os : Os.t;
  }

  type t = {name : string; vm : vm}

  let docker_image_encoding =
    let open Data_encoding in
    union
      [
        case
          ~title:"gcp"
          Json_only
          Data_encoding.(obj1 (req "gcp" string))
          (function Gcp {alias} -> Some alias | _ -> None)
          (fun alias -> Gcp {alias});
        case
          ~title:"octez_release"
          Json_only
          Data_encoding.(obj1 (req "octez" string))
          (function Octez_release {tag} -> Some tag | _ -> None)
          (fun tag -> Octez_release {tag});
      ]

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             name;
             vm =
               {
                 machine_type;
                 binaries_path;
                 docker_image;
                 max_run_duration = _;
                 os;
               };
           } -> (name, machine_type, binaries_path, docker_image, os))
      (fun (name, machine_type, binaries_path, docker_image, os) ->
        {
          name;
          vm =
            {
              machine_type;
              binaries_path;
              max_run_duration = None;
              docker_image;
              os;
            };
        })
      (obj5
         (req "name" string)
         (req "machine_type" string)
         (req "binaries_path" string)
         (req "docker_image" docker_image_encoding)
         (req "os" Os.encoding))

  let default_gcp_machine_type = "n1-standard-2"

  let default_docker_image ~tezt_cloud = Gcp {alias = tezt_cloud}

  let default_max_run_duration = Some 7200

  let default_gcp_binaries_path =
    Filename.get_temp_dir_name () // "tezt-runners"

  let make ~os ~binaries_path ?max_run_duration ~machine_type ~docker_image
      ~name () =
    let binaries_path =
      match docker_image with
      | Gcp _ -> binaries_path
      | Octez_release _ -> "/usr/local/bin"
    in
    {
      name;
      vm = {os; machine_type; docker_image; max_run_duration; binaries_path};
    }
end
