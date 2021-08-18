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
  Lwt_utils_unix.Json.read_file (dirname // name) >>=? fun json ->
  return (Data_encoding.Json.destruct Meta.encoding json)

let find_component dirname module_name =
  let name_lowercase = String.uncapitalize_ascii module_name in
  let implementation = (dirname // name_lowercase) ^ ".ml" in
  let interface = implementation ^ "i" in
  match (Sys.file_exists implementation, Sys.file_exists interface) with
  | (false, _) -> Stdlib.failwith @@ "No such file: " ^ implementation
  | (true, false) ->
      Lwt_utils_unix.read_file implementation >|= fun implementation ->
      {name = module_name; interface = None; implementation}
  | _ ->
      Lwt_utils_unix.read_file interface >>= fun interface ->
      Lwt_utils_unix.read_file implementation >|= fun implementation ->
      {name = module_name; interface = Some interface; implementation}

let read_dir dir =
  of_file ~dir >>=? fun meta ->
  Lwt_list.map_p (find_component dir) meta.modules >>= fun components ->
  let expected_env =
    match meta.expected_env_version with None -> V0 | Some v -> v
  in
  return (meta.hash, {expected_env; components})

open Lwt.Infix

let create_files dir units =
  Lwt_utils_unix.remove_dir dir >>= fun () ->
  Lwt_utils_unix.create_dir dir >>= fun () ->
  Lwt_list.map_s
    (fun {name; interface; implementation} ->
      let name = String.lowercase_ascii name in
      let ml = dir // (name ^ ".ml") in
      let mli = dir // (name ^ ".mli") in
      Lwt_utils_unix.create_file ml implementation >>= fun () ->
      match interface with
      | None -> Lwt.return [ml]
      | Some content ->
          Lwt_utils_unix.create_file mli content >>= fun () ->
          Lwt.return [mli; ml])
    units
  >>= fun files ->
  let files = List.concat files in
  Lwt.return files

let write_dir dir ?hash (p : t) =
  create_files dir p.components >>= fun _files ->
  to_file
    ~dir
    ?hash
    ~env_version:p.expected_env
    (List.map (fun {name; _} -> String.capitalize_ascii name) p.components)
