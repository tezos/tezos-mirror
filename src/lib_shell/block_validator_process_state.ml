(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

(** Event definition and registration for the standard block
   validation method *)
module Seq_validator_events = struct
  type status =
    | Init
    | Close
    | Validation_request of Block_hash.t * Chain_id.t
    | Validation_success of Block_hash.t * Ptime.t

  let status_pp ppf = function
    | Init ->
        Format.fprintf ppf "Initialized"
    | Close ->
        Format.fprintf ppf "Shutting down"
    | Validation_request (block_hash, chain_id) ->
        Format.fprintf
          ppf
          "Requesting validation of block %a for chain %a"
          Block_hash.pp_short
          block_hash
          Chain_id.pp_short
          chain_id
    | Validation_success (block_hash, start_time) ->
        Format.fprintf
          ppf
          "Block %a successfully validated in %a"
          Block_hash.pp_short
          block_hash
          Ptime.Span.pp
          (Ptime.diff (Systime_os.now ()) start_time)

  type s = status Time.System.stamped

  module Definition = struct
    let name = "block_validator_process_sequential"

    type nonrec t = s

    let encoding =
      let open Data_encoding in
      Time.System.stamped_encoding
      @@ union
           [ case
               (Tag 0)
               ~title:"Init"
               empty
               (function Init -> Some () | _ -> None)
               (fun () -> Init);
             case
               (Tag 1)
               ~title:"Close"
               empty
               (function Close -> Some () | _ -> None)
               (fun () -> Close);
             case
               (Tag 2)
               ~title:"Validation_request"
               (tup2 Block_hash.encoding Chain_id.encoding)
               (function
                 | Validation_request (h, c) -> Some (h, c) | _ -> None)
               (fun (h, c) -> Validation_request (h, c));
             case
               (Tag 3)
               ~title:"Validation_success"
               (tup2 Block_hash.encoding Time.System.encoding)
               (function
                 | Validation_success (bh, st) -> Some (bh, st) | _ -> None)
               (fun (bh, st) -> Validation_success (bh, st)) ]

    let pp ppf (status : t) = Format.fprintf ppf "%a" status_pp status.data

    let doc = "Sequential block validator status."

    let level (status : t) =
      match status.data with
      | Init | Close ->
          Internal_event.Notice
      | Validation_request _ | Validation_success _ ->
          Internal_event.Debug
  end

  module Event_block_validator_process = Internal_event.Make (Definition)

  let lwt_emit status =
    let time = Systime_os.now () in
    Event_block_validator_process.emit
      ~section:(Internal_event.Section.make_sanitized [Definition.name])
      (fun () -> Time.System.stamp ~time status)
    >>= function
    | Ok () ->
        Lwt.return_unit
    | Error el ->
        Format.kasprintf
          Lwt.fail_with
          "Block_validator_process_sequential_event.emit: %a"
          pp_print_error
          el

  let lwt_timed_emit status =
    let now = Systime_os.now () in
    lwt_emit status >>= fun () -> Lwt.return now
end

(** Event definition and registration for block validation using an
   external process *)
module External_validator_events = struct
  type status =
    | Init
    | Close
    | Process_status of Unix.process_status
    | Validator_started of int
    | Request of External_validation.request
    | Request_result of External_validation.request * Ptime.t

  let status_pp ppf = function
    | Init ->
        Format.fprintf ppf "Initialized"
    | Close ->
        Format.fprintf ppf "Shutting down"
    | Process_status s -> (
      match s with
      | WEXITED 0 ->
          Format.fprintf ppf "The process terminated normally"
      | WEXITED i ->
          Format.fprintf
            ppf
            "The process terminated abnormally with value %i"
            i
      | WSIGNALED i ->
          Format.fprintf ppf "The process was killed by signal %i" i
      | WSTOPPED i ->
          Format.fprintf ppf "The process was stopped by signal %i" i )
    | Validator_started pid ->
        Format.fprintf ppf "Block validator started on pid %i " pid
    | Request r ->
        Format.fprintf ppf "Request for %a" External_validation.request_pp r
    | Request_result (req, start_time) ->
        Format.fprintf
          ppf
          "Completion of %a in %a"
          External_validation.request_pp
          req
          Ptime.Span.pp
          (Ptime.diff (Systime_os.now ()) start_time)

  type s = status Time.System.stamped

  module Definition = struct
    let name = "block_validator_process_external"

    type nonrec t = s

    let process_status_encoding =
      let open Data_encoding in
      union
        Unix.
          [ case
              (Tag 0)
              ~title:"wexited"
              int31
              (function WEXITED i -> Some i | _ -> None)
              (fun i -> WEXITED i);
            case
              (Tag 1)
              ~title:"wsignaled"
              int31
              (function WSIGNALED i -> Some i | _ -> None)
              (fun i -> WSIGNALED i);
            case
              (Tag 2)
              ~title:"wstopped"
              int31
              (function WSTOPPED i -> Some i | _ -> None)
              (fun i -> WSTOPPED i) ]

    let encoding =
      let open Data_encoding in
      Time.System.stamped_encoding
      @@ union
           [ case
               (Tag 0)
               ~title:"Init"
               empty
               (function Init -> Some () | _ -> None)
               (fun () -> Init);
             case
               (Tag 1)
               ~title:"Close"
               empty
               (function Close -> Some () | _ -> None)
               (fun () -> Close);
             case
               (Tag 2)
               ~title:"Process_status"
               process_status_encoding
               (function Process_status s -> Some s | _ -> None)
               (fun s -> Process_status s);
             case
               (Tag 3)
               ~title:"Validation_started"
               int31
               (function Validator_started pid -> Some pid | _ -> None)
               (fun pid -> Validator_started pid);
             case
               (Tag 4)
               ~title:"Request_sent"
               External_validation.request_encoding
               (function Request r -> Some r | _ -> None)
               (fun r -> Request r);
             case
               (Tag 5)
               ~title:"Request_result"
               (tup2 External_validation.request_encoding Time.System.encoding)
               (function Request_result (r, st) -> Some (r, st) | _ -> None)
               (fun (r, st) -> Request_result (r, st)) ]

    let pp ppf (status : t) = Format.fprintf ppf "%a" status_pp status.data

    let doc = "External block validator status."

    let level (status : t) =
      match status.data with
      | Init | Close | Process_status (WEXITED 0) | Validator_started _ ->
          Internal_event.Notice
      | Process_status _ ->
          Internal_event.Fatal
      | Request _ | Request_result _ ->
          Internal_event.Debug
  end

  module Event_block_validator_process = Internal_event.Make (Definition)

  let lwt_emit status =
    let time = Systime_os.now () in
    Event_block_validator_process.emit
      ~section:(Internal_event.Section.make_sanitized [Definition.name])
      (fun () -> Time.System.stamp ~time status)
    >>= function
    | Ok () ->
        Lwt.return_unit
    | Error el ->
        Format.kasprintf
          Lwt.fail_with
          "Block_validator_process_external_event.emit: %a"
          pp_print_error
          el

  let lwt_timed_emit status =
    let now = Systime_os.now () in
    lwt_emit status >>= fun () -> Lwt.return now
end
