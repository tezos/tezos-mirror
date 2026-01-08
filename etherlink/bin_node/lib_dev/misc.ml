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

let normalize_hex str =
  let open Result_syntax in
  let str = String.lowercase_ascii str in
  let str =
    match String.remove_prefix ~prefix:"0x" str with
    | Some str -> str
    | None -> str
  in
  let res = `Hex str in
  if Option.is_some (Hex.to_string res) then return res
  else error_with "%s is not a valid hexa-encoded string" str

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

let domain_count_cap () =
  (* The node require 5 domains minimum: one for the scheduler, one for the
     [Evm_context] worker, one for spawning arbitrary processes, one for
     forking for the Irmin GC, and at least one for the RPC handlers. *)
  max (min (Domain.recommended_domain_count ()) 16) 5

exception Timeout

let with_timeout t k =
  let open Lwt_result_syntax in
  Lwt.pick
    [
      k ();
      (let*! () = Lwt_unix.sleep (float_of_int t) in
       fail_with_exn Timeout);
    ]
