open Error_monad

let name = "TEZOS_PROTOCOL"

open Protocol
open TzFilename.Infix

let to_file ~dir:dirname ?hash ?env_version modules =
  let config_file =
    Data_encoding.Json.construct
      Meta.encoding
      {hash; expected_env_version = env_version; modules}
  in
  Lwt_utils_unix.Json.write_file (dirname // name) config_file

let of_file ~dir:dirname =
  let open Lwt_result_syntax in
  let* json = Lwt_utils_unix.Json.read_file (dirname // name) in
  return (Data_encoding.Json.destruct Meta.encoding json)

let find_component dirname module_name =
  let open Lwt_syntax in
  let name_lowercase = String.uncapitalize_ascii module_name in
  let implementation = (dirname // name_lowercase) ^ ".ml" in
  let interface = implementation ^ "i" in
  match (Sys.file_exists implementation, Sys.file_exists interface) with
  | false, _ -> Stdlib.failwith @@ "No such file: " ^ implementation
  | true, false ->
      let+ implementation = Lwt_utils_unix.read_file implementation in
      {name = module_name; interface = None; implementation}
  | true, true ->
      let+ interface = Lwt_utils_unix.read_file interface
      and+ implementation = Lwt_utils_unix.read_file implementation in
      {name = module_name; interface = Some interface; implementation}

let read_dir dir =
  let open Lwt_result_syntax in
  let* meta = of_file ~dir in
  let*! components = Lwt_list.map_s (find_component dir) meta.modules in
  let expected_env =
    match meta.expected_env_version with None -> V0 | Some v -> v
  in
  return (meta.hash, {expected_env; components})

let create_files dir units =
  let open Lwt_syntax in
  let* () = Lwt_utils_unix.remove_dir dir in
  let* () = Lwt_utils_unix.create_dir dir in
  let* files =
    Lwt_list.map_s
      (fun {name; interface; implementation} ->
        let name = String.lowercase_ascii name in
        let ml = dir // (name ^ ".ml") in
        let mli = dir // (name ^ ".mli") in
        let+ () = Lwt_utils_unix.create_file ml implementation
        and+ () =
          TzLwtreslib.Option.iter_s (Lwt_utils_unix.create_file mli) interface
        in
        match interface with None -> [ml] | Some _ -> [mli; ml])
      units
  in
  let files = List.concat files in
  Lwt.return files

let write_dir dir ?hash (p : t) =
  let open Lwt_syntax in
  let* _files = create_files dir p.components in
  to_file
    ~dir
    ?hash
    ~env_version:p.expected_env
    (List.map (fun {name; _} -> String.capitalize_ascii name) p.components)
