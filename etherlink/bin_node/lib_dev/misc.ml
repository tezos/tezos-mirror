(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let now () =
  let now = Ptime_clock.now () in
  let now = Ptime.to_rfc3339 now in
  Time.Protocol.of_notation_exn now

let with_timing event k =
  let open Lwt_syntax in
  let start = Time.System.now () in

  let* res = k () in

  let stop = Time.System.now () in
  let diff = Ptime.diff stop start in
  let* () = event diff in

  return res

let with_timing_f_e event k =
  let open Lwt_result_syntax in
  let start = Time.System.now () in

  let* res = k () in

  let stop = Time.System.now () in
  let diff = Ptime.diff stop start in
  let*! () = event res diff in

  return res

let unwrap_error_monad f =
  let open Lwt_syntax in
  let* res = f () in
  match res with
  | Ok v -> return v
  | Error errs ->
      Lwt.fail_with (Format.asprintf "%a" Error_monad.pp_print_trace errs)

let normalize_addr str =
  let str = String.lowercase_ascii str in
  match String.remove_prefix ~prefix:"0x" str with
  | Some str -> str
  | None -> str

let interpolate str
    (vars : (char * [`Available of string | `Disabled of string]) list) =
  let open Result_syntax in
  let vars = ('%', `Available "%") :: vars in
  let buf = Buffer.create (String.length str) in
  let* look =
    String.to_seq str
    |> Seq.E.fold_left
         (fun look c ->
           if look then
             match List.assoc_opt ~equal:( = ) c vars with
             | Some (`Available substitute) ->
                 Buffer.add_string buf substitute ;
                 return false
             | Some (`Disabled reason) ->
                 error_with "Cannot use %%%c in this context (%s)" c reason
             | None -> error_with "%%%c is not a valid variable" c
           else if c = '%' then return true
           else (
             Buffer.add_char buf c ;
             return false))
         false
  in
  if not look then return (Buffer.contents buf)
  else error_with "Trailing %% are not supported"
