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

let interpolate str vars =
  let buf = Buffer.create (String.length str) in
  let look =
    String.fold_left
      (fun look c ->
        if look then
          match List.assoc_opt ~equal:( = ) c vars with
          | Some substitute ->
              Buffer.add_string buf substitute ;
              false
          | None -> raise (Invalid_argument "interpolate")
        else if c = '%' then true
        else (
          Buffer.add_char buf c ;
          false))
      false
      str
  in
  assert (not look) ;
  Buffer.contents buf

let rec download_file ~keep_alive ~working_dir url =
  let open Lwt_syntax in
  let open Filename.Infix in
  let on_failure reason =
    let* () = Events.download_failed url reason in
    if keep_alive then
      let* () = Lwt_unix.sleep 2. in
      download_file ~keep_alive ~working_dir url
    else failwith "Downloading %s failed" url
  in
  let tmp_snapshot_name = Filename.basename url in
  let target_path = working_dir // tmp_snapshot_name in
  let uri = Uri.of_string url in
  let start_time = Ptime_clock.now () in

  let rec periodic_emit ~size ~remaining_size =
    let* () = Lwt_unix.sleep 60.0 in
    let elapsed_time = Ptime.diff (Ptime_clock.now ()) start_time in
    let* () =
      Events.download_in_progress
        ~size
        ~remaining_size:!remaining_size
        ~elapsed_time
        url
    in
    periodic_emit ~size ~remaining_size
  in

  let download_task ~stream ~remaining_size =
    Lwt_io.with_file ~mode:Lwt_io.output target_path @@ fun out ->
    Lwt_stream.iter_s
      (fun chunk ->
        (* Per observations, this iteration over the stream is very
           eager, and cannot be canceled easily without yielding
           explicitely. *)
        let* () = Lwt.pause () in
        let chunk_len = String.length chunk in
        remaining_size :=
          Option.map
            (fun remaining_size -> remaining_size - chunk_len)
            !remaining_size ;
        Lwt_io.write out chunk)
      stream
  in

  Lwt.catch
    (fun () ->
      let* resp, body = Cohttp_lwt_unix.Client.get uri in
      match resp.status with
      | `OK ->
          let stream = Cohttp_lwt.Body.to_stream body in
          let size =
            Option.map
              int_of_string
              (Cohttp.Header.get resp.headers "content-length")
          in
          let remaining_size = ref size in
          let* () = Events.downloading_file ?size url in
          let* () =
            Lwt.pick
              [
                download_task ~stream ~remaining_size;
                periodic_emit ~size ~remaining_size;
              ]
          in
          Lwt_result.return target_path
      | #Cohttp.Code.status_code as status_code ->
          on_failure (Http_error status_code))
    (function
      | Lwt.Canceled as exn -> Lwt.reraise exn | exn -> on_failure (Exn exn))
