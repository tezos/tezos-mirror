(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type status =
  | Dir_is_up_to_date
  | Upgrading_node of string * string
  | Update_success
  | Aborting_upgrade of string
  | Upgrade_status of bool * string * string option

let status_pp ppf = function
  | Dir_is_up_to_date ->
      Format.fprintf ppf "Node data dir is up-to-date."
  | Upgrading_node (version, data_version) ->
      Format.fprintf
        ppf
        "Upgrading node data dir from %s to %s...@.Please, do not interrupt \
         the process."
        version
        data_version
  | Update_success ->
      Format.fprintf ppf "The node data dir is now up-to-date!"
  | Aborting_upgrade errs ->
      Format.fprintf
        ppf
        "%s@.Aborting upgrade. The storage was not upgraded."
        errs
  | Upgrade_status (upgradable_status, data_dir_version, available_version) ->
      Format.fprintf
        ppf
        "Upgrade available: %a@.Current version: %a%a"
        Format.pp_print_bool
        upgradable_status
        Format.pp_print_string
        data_dir_version
        (Option.pp ~default:"" (fun ppf ->
             Format.fprintf ppf "@.Available version: %s"))
        available_version

type s = status Time.System.stamped

module Definition = struct
  let name = "node_data_version"

  type nonrec t = s

  let encoding =
    let open Data_encoding in
    Time.System.stamped_encoding
    @@ union
         [ case
             (Tag 0)
             ~title:"Dir_is_up_to_date"
             empty
             (function Dir_is_up_to_date -> Some () | _ -> None)
             (fun () -> Dir_is_up_to_date);
           case
             (Tag 1)
             ~title:"Upgrading_node"
             (tup2 string string)
             (function Upgrading_node (v, dd) -> Some (v, dd) | _ -> None)
             (fun (v, dd) -> Upgrading_node (v, dd));
           case
             (Tag 2)
             ~title:"Update_success"
             empty
             (function Update_success -> Some () | _ -> None)
             (fun () -> Update_success);
           case
             (Tag 3)
             ~title:"Aborting_upgrade"
             string
             (function Aborting_upgrade errs -> Some errs | _ -> None)
             (fun errs -> Aborting_upgrade errs);
           case
             (Tag 4)
             ~title:"Upgrade_status"
             (tup3 bool string (option string))
             (function
               | Upgrade_status (s, cv, av) -> Some (s, cv, av) | _ -> None)
             (fun (s, cv, av) -> Upgrade_status (s, cv, av)) ]

  let pp ppf (status : t) = Format.fprintf ppf "%a" status_pp status.data

  let doc = "Node data version status."

  let level (status : t) =
    match status.data with
    | Dir_is_up_to_date | Upgrading_node _ | Update_success ->
        Internal_event.Notice
    | Upgrade_status _ | Aborting_upgrade _ ->
        Internal_event.Error
end

module Event_node_data_version = Internal_event.Make (Definition)

let lwt_emit (status : status) =
  let time = Systime_os.now () in
  Event_node_data_version.emit
    ~section:(Internal_event.Section.make_sanitized [Definition.name])
    (fun () -> Time.System.stamp ~time status)
  >>= function
  | Ok () ->
      Lwt.return_unit
  | Error el ->
      Format.kasprintf
        Lwt.fail_with
        "Node_data_version_event.emit: %a"
        pp_print_error
        el
