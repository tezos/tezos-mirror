(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Scheduler in OCaml that mimics cron functionality. *)

(* Needs to be extended to support ranges, multiple values and step values. *)
type time = {
  minute : int option;
  hour : int option;
  day : int option;
  month : int option;
  day_of_week : int option; (* 0-6, Sunday = 0 *)
}

type task = {name : string; time : time; action : unit -> unit Lwt.t}

type t = {
  tasks : task list;
  shutdown : unit Lwt.t;
  trigger_shutdown : unit Lwt.u;
}

let validate_time s =
  let in_range ~v (min, max) =
    Option.fold ~none:true ~some:(fun v -> v >= min && v <= max) v
  in
  in_range ~v:s.minute (0, 59)
  && in_range ~v:s.hour (0, 23)
  && in_range ~v:s.day (1, 31)
  && in_range ~v:s.month (1, 12)
  && in_range ~v:s.day_of_week (0, 6)

let time_of_string s =
  let xs = String.split_on_char ' ' s in
  match xs with
  | [min; hour; day; month; dow] ->
      let parse str =
        match str with "*" -> None | s -> Some (int_of_string s)
      in

      {
        minute = parse min;
        hour = parse hour;
        day = parse day;
        month = parse month;
        day_of_week = parse dow;
      }
  | _ -> failwith (Format.asprintf "Invalid cron string format: %s" s)

let time_to_string spec =
  let to_string = Option.fold ~none:"*" ~some:string_of_int in
  String.concat
    " "
    [
      to_string spec.minute;
      to_string spec.hour;
      to_string spec.day;
      to_string spec.month;
      to_string spec.day_of_week;
    ]

let task ~name ~tm ~action =
  let time = time_of_string tm in
  let is_valid = validate_time time in
  if not is_valid then
    failwith
      (Format.asprintf "Invalid time specification: %s" (time_to_string time)) ;
  {name; time; action}

let init ~tasks =
  let shutdown, trigger_shutdown = Lwt.task () in
  {tasks; shutdown; trigger_shutdown}

(* Check if a task should run at the given time.

   The `+ 1` is needed because OCaml's Unix module and the cron
   standard use different numbering conventions for months:
   - Unix.tm_mon ranges from 0 to 11 (January = 0, December = 11)
   - Cron standard uses 1 to 12 (January = 1, December = 12) *)
let should_run task tm =
  let matches value = Option.fold ~none:true ~some:(Int.equal value) in
  matches tm.Unix.tm_min task.time.minute
  && matches tm.Unix.tm_hour task.time.hour
  && matches tm.Unix.tm_mday task.time.day
  && matches (succ tm.Unix.tm_mon) task.time.month
  && matches tm.Unix.tm_wday task.time.day_of_week

let run ~now_tm t =
  Lwt_list.iter_p
    (fun task ->
      if should_run task now_tm then (
        Log.info "chronos.%s: starting task" task.name ;
        Lwt.catch
          (fun () ->
            let* () = task.action () in
            Log.info "chronos.%s: task successfully executed" task.name ;
            Lwt.return_unit)
          (fun exn ->
            Log.error
              "chronos.%s: task failed with the following exception: %s"
              task.name
              (Printexc.to_string exn) ;
            Lwt.return_unit))
      else Lwt.return_unit)
    t.tasks

let start t =
  Log.info
    "chronos: starting with %d tasks:@;%a"
    (List.length t.tasks)
    (Format.pp_print_list ~pp_sep:Format.pp_print_newline (fun ppf task ->
         Format.fprintf
           ppf
           "- '%s' with time '%s'"
           task.name
           (time_to_string task.time)))
    t.tasks ;
  let rec loop last_tm =
    let now_tm = Unix.(gmtime (gettimeofday ())) in
    let* () =
      if last_tm.Unix.tm_min <> now_tm.Unix.tm_min then run ~now_tm t
      else Lwt_unix.sleep 1.
    in
    loop now_tm
  in
  (* Runs the loop until the shutdown is triggered. *)
  Background.register
    (Lwt.pick
       [
         (let now_tm = Unix.(gmtime (gettimeofday ())) in
          loop now_tm);
         t.shutdown;
       ])

let shutdown t =
  Log.info "chronos: shutdown" ;
  Lwt.wakeup t.trigger_shutdown ()
