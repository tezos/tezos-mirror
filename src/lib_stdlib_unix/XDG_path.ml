(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2023 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/5128
   unify references to XDG *)

type kind = Data | Config | State | Bin | Cache | Runtime

type intent = Read_only | Read_write

let ( // ) = Filename.concat

(* See relative path specifications in interface file *)
let ignore_relative_paths = true

let guard_relative_path path =
  ignore_relative_paths && Filename.is_relative path

let home () =
  match Sys.getenv_opt "HOME" with
  | None -> Filename.current_dir_name
  | Some home -> home

(* Get the value of an XDG specified environment variable that is supposed to
   contain a path. *)
let get_env_path var_name default =
  match Sys.getenv_opt var_name with
  | None | Some "" -> default ()
  | Some path ->
      if guard_relative_path path then
        (* Note: the specification is vague about this case,
           this is an interpretation. *)
        default ()
      else path

(* Get the value of an environment variable that is supposed to contain a list of paths. *)
let get_env_paths var_name default =
  match Sys.getenv_opt var_name with
  | None | Some "" -> default
  | Some x -> (
      match
        List.filter
          (fun path -> not (guard_relative_path path))
          (* The specs defines paths separated by `:` *)
          (String.split_on_char ':' x)
      with
      | [] ->
          (* Note: the specification is vague about this case,
             this is an interpretation. *)
          default
      | _ :: _ as paths -> paths)

let xdg_data_home () =
  get_env_path "XDG_DATA_HOME" @@ fun () -> home () // ".local/share"

let xdg_data_dirs () =
  (* Elements in the environment variable are
     ordered by preference. Defaults are given by XDG Specs. *)
  get_env_paths "XDG_DATA_DIRS" ["/usr/local/share/"; "/usr/share/"]

let xdg_config_home () =
  get_env_path "XDG_CONFIG_HOME" @@ fun () -> home () // ".config"

let xdg_config_dirs () = get_env_paths "XDG_CONFIG_DIRS" ["/etc/xdg"]

let xdg_state_home () =
  get_env_path "XDG_STATE_HOME" @@ fun () -> home () // ".local/state"

let xdg_cache_home () =
  get_env_path "XDG_CACHE_HOME" @@ fun () -> home () // ".cache"

let xdg_runtime_dir () =
  get_env_path "XDG_RUNTIME_DIR" @@ fun () ->
  (* Note: the specification does not give a default value. *)
  Filename.get_temp_dir_name ()

let search get_home get_dirs path =
  let in_home = get_home () // path in
  if Sys.file_exists in_home then in_home
  else
    let rec find = function
      | [] -> in_home
      | head :: tail ->
          let in_head = head // path in
          if Sys.file_exists in_head then in_head else find tail
    in
    find (get_dirs ())

let get intent kind ~path =
  match (kind, intent) with
  | Data, Read_only -> search xdg_data_home xdg_data_dirs path
  | Data, Read_write -> xdg_data_home () // path
  | Config, Read_only -> search xdg_config_home xdg_config_dirs path
  | Config, Read_write -> xdg_config_home () // path
  | State, _ -> xdg_state_home () // path
  | Bin, _ -> home () // ".local/bin" // path
  | Cache, _ -> xdg_cache_home () // path
  | Runtime, _ -> xdg_runtime_dir () // path
