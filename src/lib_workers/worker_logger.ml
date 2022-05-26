(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module type STATIC = sig
  val worker_name : string
end

module Make
    (Event : Worker_intf.EVENT)
    (Request : Worker_intf.VIEW)
    (Static : STATIC) :
  Worker_intf.LOGGER with module Event = Event and module Request = Request =
struct
  module Event = Event
  module Request = Request

  type status =
    | WorkerEvent of (Event.t * Internal_event.level)
    | Request of
        (Request.view * Worker_types.request_status * error list option)
    | Terminated
    | Timeout
    | Crashed of error list
    | Started of string option
    | Triggering_shutdown
    | Duplicate of string

  type t = status Time.System.stamped

  let status_encoding =
    let open Data_encoding in
    Time.System.stamped_encoding
    @@ union
         [
           case
             (Tag 0)
             ~title:"Event"
             (obj2
                (req "event" @@ dynamic_size Event.encoding)
                (req "level" Internal_event.Level.encoding))
             (function WorkerEvent (e, l) -> Some (e, l) | _ -> None)
             (fun (e, l) -> WorkerEvent (e, l));
           case
             (Tag 1)
             ~title:"Request"
             (obj3
                (req "request_view" @@ dynamic_size Request.encoding)
                (req "request_status" Worker_types.request_status_encoding)
                (req "errors" (option (list error_encoding))))
             (function Request (v, s, e) -> Some (v, s, e) | _ -> None)
             (fun (v, s, e) -> Request (v, s, e));
           case
             (Tag 2)
             ~title:"Terminated"
             Data_encoding.empty
             (function Terminated -> Some () | _ -> None)
             (fun () -> Terminated);
           case
             (Tag 3)
             ~title:"Timeout"
             Data_encoding.empty
             (function Timeout -> Some () | _ -> None)
             (fun () -> Timeout);
           case
             (Tag 4)
             ~title:"Crashed"
             (list error_encoding)
             (function Crashed errs -> Some errs | _ -> None)
             (fun errs -> Crashed errs);
           case
             (Tag 5)
             ~title:"Started"
             (option string)
             (function Started n -> Some n | _ -> None)
             (fun n -> Started n);
           case
             (Tag 6)
             ~title:"Triggering_shutdown"
             Data_encoding.empty
             (function Triggering_shutdown -> Some () | _ -> None)
             (fun () -> Triggering_shutdown);
           case
             (Tag 7)
             ~title:"Duplicate"
             string
             (function Duplicate n -> Some n | _ -> None)
             (fun n -> Duplicate n);
         ]

  let pp base_name ppf = function
    | WorkerEvent (evt, _) -> Format.fprintf ppf "%a" Event.pp evt
    | Request (view, {pushed; treated; completed}, None) ->
        Format.fprintf
          ppf
          "@[<v 0>%a@, %a@]"
          Request.pp
          view
          Worker_types.pp_status
          {pushed; treated; completed}
    | Request (view, {pushed; treated; completed}, Some errors) ->
        Format.fprintf
          ppf
          "@[<v 0>%a@, %a, %a@]"
          Request.pp
          view
          Worker_types.pp_status
          {pushed; treated; completed}
          (Format.pp_print_list Error_monad.pp)
          errors
    | Terminated -> Format.fprintf ppf "@[Worker terminated [%s] @]" base_name
    | Timeout ->
        Format.fprintf ppf "@[Worker terminated with timeout [%s] @]" base_name
    | Crashed errs ->
        Format.fprintf
          ppf
          "@[<v 0>Worker crashed [%s]:@,%a@]"
          base_name
          (Format.pp_print_list Error_monad.pp)
          errs
    | Started None -> Format.fprintf ppf "Worker started"
    | Started (Some n) -> Format.fprintf ppf "Worker started for %s" n
    | Triggering_shutdown -> Format.fprintf ppf "Triggering shutdown"
    | Duplicate name ->
        let full_name =
          if name = "" then base_name
          else Format.asprintf "%s_%s" base_name name
        in
        Format.fprintf ppf "Worker.launch: duplicate worker %s" full_name

  module Definition : Internal_event.EVENT_DEFINITION with type t = t = struct
    let section = None

    let name = Static.worker_name

    type nonrec t = t

    let encoding =
      let open Data_encoding in
      let v0_encoding = status_encoding in
      With_version.(encoding ~name (first_version v0_encoding))

    let pp ~short:_ ppf (status : t) =
      Format.fprintf ppf "%a" (pp Static.worker_name) status.data

    let doc = "Worker status."

    let level (status : t) =
      match status.data with
      | WorkerEvent (_, level) -> level
      | Request _ -> Internal_event.Debug
      | Timeout -> Internal_event.Notice
      | Terminated | Started _ -> Internal_event.Info
      | Crashed _ -> Internal_event.Error
      | Triggering_shutdown -> Internal_event.Debug
      | Duplicate _ -> Internal_event.Error
  end

  module LogEvent : Internal_event.EVENT with type t = t =
    Internal_event.Make (Definition)
end
