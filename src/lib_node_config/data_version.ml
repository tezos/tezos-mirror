(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Filename.Infix

let store_dir data_dir = data_dir // "store"

let context_dir data_dir = data_dir // "context"

let protocol_dir data_dir = data_dir // "protocol"

let lock_file data_dir = data_dir // "lock"

let default_identity_file_name = "identity.json"

let default_peers_file_name = "peers.json"

let default_config_file_name = "config.json"

let version_file_name = "version.json"

module Version = struct
  type t = {major : int; minor : int}

  let compare v1 v2 =
    let c = Int.compare v1.major v2.major in
    if c <> 0 then c else Int.compare v1.minor v2.minor

  let ( < ) v1 v2 = compare v1 v2 < 0

  let equal v1 v2 = compare v1 v2 = 0

  let make ~major ~minor =
    if Compare.Int.(major < 0 || minor < 0) then
      invalid_arg
        (Printf.sprintf
           "Version.make: version number cannot be negative: %d.%d"
           major
           minor) ;
    {major; minor}

  let parse_version_item s =
    match int_of_string_opt s with
    | None ->
        invalid_arg
          ("Version.parse_version_item: invalid integer: " ^ String.escaped s)
    | Some i -> i

  let of_string s =
    match String.split_on_char '.' s with
    | [major; minor] ->
        make ~major:(parse_version_item major) ~minor:(parse_version_item minor)
    | [_major; minor; patch] ->
        (* Allows a backward compatibility from the previous version schema,
           represented by a triplet. It transforms version X.Y.Z to Y.Z.
           Note that there were no version with X <> 0. *)
        make ~major:(parse_version_item minor) ~minor:(parse_version_item patch)
    | _ -> invalid_arg "Version.of_string: string is not of the form X.Y"

  let to_string {major; minor} = Printf.sprintf "%d.%d" major minor

  let pp fmt v = Format.pp_print_string fmt (to_string v)

  let encoding =
    let open Data_encoding in
    conv to_string of_string (obj1 (req "version" string))
end

(* Data_version history:
 *  - (0.)0.1 : original storage
 *  - (0.)0.2 : never released
 *  - (0.)0.3 : store upgrade (introducing history mode)
 *  - (0.)0.4 : context upgrade (switching from LMDB to IRMIN v2) -- v9.0
 *  - (0.)0.5 : never released -- v10.0~rc1 and 10.0~rc2
 *  - (0.)0.6 : store upgrade (switching from LMDB) -- v11.0
 *  - (0.)0.7 : new store metadata representation -- v12.1
 *  - (0.)0.8 : context upgrade (upgrade to irmin.3.0) -- v13.0
 *  - 1.0     : context upgrade (upgrade to irmin.3.3) -- v14.0
 *  - 2.0     : introduce context GC (upgrade to irmin.3.4) -- v15.0
 *  - 3.0     : change blocks' context hash semantics and introduce
                context split (upgrade to irmin.3.5) -- v16.0 *)

(* FIXME https://gitlab.com/tezos/tezos/-/issues/2861
   We should enable the semantic versioning instead of applying
   hardcoded rules.*)
let v_0_6 = Version.make ~major:0 ~minor:6

let v_0_7 = Version.make ~major:0 ~minor:7

let v_0_8 = Version.make ~major:0 ~minor:8

let v_1_0 = Version.make ~major:1 ~minor:0

let v_2_0 = Version.make ~major:2 ~minor:0

let v_3_0 = Version.make ~major:3 ~minor:0

let current_version = v_3_0

(* List of upgrade functions from each still supported previous
   version to the current [data_version] above. If this list grows too
   much, an idea would be to have triples (version, version,
   converter), and to sequence them dynamically instead of
   statically.
   The list of upgradable version must be updated as soon as a new
   upgrade function is added. We remove former upgrades if:
   - the upgrade requires a non-automatic upgrade
   - two automatic upgrades were already enabled
   - we want to deprecate a specific upgrade
*)
let upgradable_data_version =
  let open Lwt_result_syntax in
  let v_1_0_upgrade ~data_dir =
    let context_root = context_dir data_dir in
    (* The upgrade function consist in letting irmin doing its own
       file renaming. To do so, it must be done using a RW instance.*)
    let*! ctxt = Context.init ~readonly:false context_root in
    let*! () = Context.close ctxt in
    return_unit
  in
  let v_3_0_upgrade ~data_dir genesis =
    let store_dir = store_dir data_dir in
    Store.v_3_0_upgrade ~store_dir genesis
  in
  [
    ( v_0_6,
      fun ~data_dir genesis ~chain_name:_ ~sandbox_parameters:_ ->
        let* () = v_1_0_upgrade ~data_dir in
        v_3_0_upgrade ~data_dir genesis );
    ( v_0_7,
      fun ~data_dir genesis ~chain_name:_ ~sandbox_parameters:_ ->
        let* () = v_1_0_upgrade ~data_dir in
        v_3_0_upgrade ~data_dir genesis );
    ( v_0_8,
      fun ~data_dir genesis ~chain_name:_ ~sandbox_parameters:_ ->
        let* () = v_1_0_upgrade ~data_dir in
        v_3_0_upgrade ~data_dir genesis );
    ( v_1_0,
      fun ~data_dir genesis ~chain_name:_ ~sandbox_parameters:_ ->
        v_3_0_upgrade ~data_dir genesis );
    ( v_2_0,
      fun ~data_dir genesis ~chain_name:_ ~sandbox_parameters:_ ->
        v_3_0_upgrade ~data_dir genesis );
  ]

type error += Invalid_data_dir_version of Version.t * Version.t

type error += Invalid_data_dir of {data_dir : string; msg : string option}

type error += Could_not_read_data_dir_version of string

type error += Could_not_write_version_file of string

type error +=
  | Data_dir_needs_upgrade of {expected : Version.t; actual : Version.t}

let () =
  register_error_kind
    `Permanent
    ~id:"main.data_version.invalid_data_dir_version"
    ~title:"Invalid data directory version"
    ~description:"The data directory version was not the one that was expected"
    ~pp:(fun ppf (exp, got) ->
      Format.fprintf
        ppf
        "Invalid data directory version '%a' (expected '%a').@,\
         Your data directory is %s"
        Version.pp
        got
        Version.pp
        exp
        (if Version.compare got exp < 0 then
         "incompatible and cannot be automatically upgraded."
        else "too recent for this node version."))
    Data_encoding.(
      obj2
        (req "expected_version" Version.encoding)
        (req "actual_version" Version.encoding))
    (function
      | Invalid_data_dir_version (expected, actual) -> Some (expected, actual)
      | _ -> None)
    (fun (expected, actual) -> Invalid_data_dir_version (expected, actual)) ;
  register_error_kind
    `Permanent
    ~id:"main.data_version.invalid_data_dir"
    ~title:"Invalid data directory"
    ~description:"The data directory cannot be accessed or created"
    ~pp:(fun ppf (dir, msg_opt) ->
      Format.fprintf
        ppf
        "Invalid data directory '%s'%a."
        dir
        (Format.pp_print_option (fun fmt msg -> Format.fprintf fmt ": %s" msg))
        msg_opt)
    Data_encoding.(obj2 (req "datadir_path" string) (opt "message" string))
    (function
      | Invalid_data_dir {data_dir; msg} -> Some (data_dir, msg) | _ -> None)
    (fun (data_dir, msg) -> Invalid_data_dir {data_dir; msg}) ;
  register_error_kind
    `Permanent
    ~id:"main.data_version.could_not_read_data_dir_version"
    ~title:"Could not read data directory version file"
    ~description:"Data directory version file was invalid."
    Data_encoding.(obj1 (req "version_path" string))
    ~pp:(fun ppf path ->
      Format.fprintf
        ppf
        "Tried to read version file at '%s', but the file could not be found \
         or parsed."
        path)
    (function Could_not_read_data_dir_version path -> Some path | _ -> None)
    (fun path -> Could_not_read_data_dir_version path) ;
  register_error_kind
    `Permanent
    ~id:"main.data_version.could_not_write_version_file"
    ~title:"Could not write version file"
    ~description:"Version file cannot be written."
    Data_encoding.(obj1 (req "file_path" string))
    ~pp:(fun ppf file_path ->
      Format.fprintf
        ppf
        "Tried to write version file at '%s',  but the file could not be \
         written."
        file_path)
    (function
      | Could_not_write_version_file file_path -> Some file_path | _ -> None)
    (fun file_path -> Could_not_write_version_file file_path) ;
  register_error_kind
    `Permanent
    ~id:"main.data_version.data_dir_needs_upgrade"
    ~title:"The data directory needs to be upgraded"
    ~description:"The data directory needs to be upgraded"
    ~pp:(fun ppf (exp, got) ->
      Format.fprintf
        ppf
        "The data directory version is too old.@,\
         Found '%a', expected '%a'.@,\
         It needs to be upgraded with `octez-node upgrade storage`."
        Version.pp
        got
        Version.pp
        exp)
    Data_encoding.(
      obj2
        (req "expected_version" Version.encoding)
        (req "actual_version" Version.encoding))
    (function
      | Data_dir_needs_upgrade {expected; actual} -> Some (expected, actual)
      | _ -> None)
    (fun (expected, actual) -> Data_dir_needs_upgrade {expected; actual})

module Events = struct
  open Internal_event.Simple

  let section = ["node"; "data_version"]

  let dir_is_up_to_date =
    declare_0
      ~section
      ~level:Notice
      ~name:"dir_is_up_to_date"
      ~msg:"node data dir is up-to-date"
      ()

  let upgrading_node =
    declare_2
      ~section
      ~level:Notice
      ~name:"upgrading_node"
      ~msg:"upgrading data directory from {old_version} to {new_version}"
      ~pp1:Version.pp
      ("old_version", Version.encoding)
      ~pp2:Version.pp
      ("new_version", Version.encoding)

  let finished_upgrading_node =
    declare_2
      ~section
      ~level:Notice
      ~name:"finished_upgrading_node"
      ~msg:
        "the node's data directory was automatically upgraded from \
         {old_version} to {new_version} and is now up-to-date"
      ~pp1:Version.pp
      ("old_version", Version.encoding)
      ~pp2:Version.pp
      ("new_version", Version.encoding)

  let update_success =
    declare_0
      ~section
      ~level:Notice
      ~name:"update_success"
      ~msg:"the node data dir is now up-to-date"
      ()

  let aborting_upgrade =
    declare_0
      ~section
      ~level:Notice
      ~name:"aborting_upgrade"
      ~msg:"failed to upgrade storage"
      ()

  let upgrade_status =
    declare_2
      ~section
      ~level:Notice
      ~name:"upgrade_status"
      ~msg:
        "current version: {current_version}, available version: \
         {available_version}"
      ~pp1:Version.pp
      ("current_version", Version.encoding)
      ~pp2:Version.pp
      ("available_version", Version.encoding)

  let emit = emit
end

let version_file data_dir = Filename.concat data_dir version_file_name

let clean_directory files =
  let to_delete =
    Format.asprintf
      "%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
         Format.pp_print_string)
      files
  in
  Format.sprintf
    "Please provide a clean directory by removing the following files: %s"
    to_delete

let write_version_file data_dir =
  let version_file = version_file data_dir in
  Lwt_utils_unix.Json.write_file
    version_file
    (Data_encoding.Json.construct Version.encoding current_version)
  |> trace (Could_not_write_version_file version_file)

let read_version_file version_file =
  let open Lwt_result_syntax in
  let* json =
    trace
      (Could_not_read_data_dir_version version_file)
      (Lwt_utils_unix.Json.read_file version_file)
  in
  try return (Data_encoding.Json.destruct Version.encoding json)
  with _ -> tzfail (Could_not_read_data_dir_version version_file)

let check_data_dir_version data_dir =
  let open Lwt_result_syntax in
  let version_file = version_file data_dir in
  let*! file_exists = Lwt_unix.file_exists version_file in
  if not file_exists then tzfail (Could_not_read_data_dir_version version_file)
  else
    let* version = read_version_file version_file in
    if Version.(equal version current_version) then return_none
    else
      match
        List.find_opt
          (fun (v, _) -> Version.equal v version)
          upgradable_data_version
      with
      | Some f -> return_some f
      | None -> tzfail (Invalid_data_dir_version (current_version, version))

type ensure_mode = Exists | Is_bare | Is_compatible

let ensure_data_dir ~mode data_dir =
  let open Lwt_result_syntax in
  let write_version () =
    let* () = write_version_file data_dir in
    return_none
  in
  Lwt.catch
    (fun () ->
      let*! file_exists = Lwt_unix.file_exists data_dir in
      if file_exists then
        let*! files =
          Lwt_stream.to_list (Lwt_unix.files_of_directory data_dir)
        in
        let files =
          List.filter
            (fun s ->
              s <> "." && s <> ".." && s <> version_file_name
              && s <> default_identity_file_name
              && s <> default_config_file_name
              && s <> default_peers_file_name)
            files
        in
        match (files, mode) with
        | [], _ -> write_version ()
        | files, Is_bare ->
            let msg = Some (clean_directory files) in
            tzfail (Invalid_data_dir {data_dir; msg})
        | _, Is_compatible -> check_data_dir_version data_dir
        | _files, Exists -> return_none
      else
        let*! () = Lwt_utils_unix.create_dir ~perm:0o700 data_dir in
        write_version ())
    (function
      | Unix.Unix_error _ -> tzfail (Invalid_data_dir {data_dir; msg = None})
      | exc -> Lwt.reraise exc)

let upgrade_data_dir ~data_dir genesis ~chain_name ~sandbox_parameters =
  let open Lwt_result_syntax in
  let* o = ensure_data_dir ~mode:Is_compatible data_dir in
  match o with
  | None ->
      let*! () = Events.(emit dir_is_up_to_date ()) in
      return_unit
  | Some (version, upgrade) -> (
      let*! () = Events.(emit upgrading_node (version, current_version)) in
      let*! r = upgrade ~data_dir genesis ~chain_name ~sandbox_parameters in
      match r with
      | Ok _success_message ->
          let* () = write_version_file data_dir in
          let*! () = Events.(emit update_success ()) in
          return_unit
      | Error e ->
          let*! () = Events.(emit aborting_upgrade) () in
          Lwt.return (Error e))

let ensure_data_dir ?(mode = Is_compatible) genesis data_dir =
  let open Lwt_result_syntax in
  let* o = ensure_data_dir ~mode data_dir in
  match o with
  | None ->
      return_unit
      (* Enable automatic upgrade to avoid users to manually upgrade. *)
  | Some (version, _)
    when Version.(
           equal version v_2_0 || equal version v_1_0 || equal version v_0_6
           || equal version v_0_7 || equal version v_0_8) ->
      let* () =
        upgrade_data_dir ~data_dir genesis ~chain_name:() ~sandbox_parameters:()
      in
      let*! () =
        Events.(emit finished_upgrading_node (version, current_version))
      in
      return_unit
  | Some (version, _) when Version.(version < current_version) ->
      tzfail
        (Data_dir_needs_upgrade {expected = current_version; actual = version})
  | Some (version, _) ->
      tzfail (Invalid_data_dir_version (current_version, version))

let upgrade_status data_dir =
  let open Lwt_result_syntax in
  let* data_dir_version = read_version_file (version_file data_dir) in
  let*! () = Events.(emit upgrade_status (data_dir_version, current_version)) in
  return_unit
