(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Scheduler in OCaml that mimics cron functionality. *)

(* Follows mostly https://man.freebsd.org/cgi/man.cgi?crontab%285%29:

   The format of a cron command is very much the V7 standard, with a
   number of upward-compatible extensions. Each line has five time and
   date fields.

   Commands are executed when the minute, hour, and month of year
   fields match the current time, and when at least one of the two day
   fields (day of month, or day of week) matches the current time.

   The time and date fields are:

   field	   allowed values
   -----	   --------------
   minute	   0-59
   hour            0-23
   day of month    1-31
   month	   1-12
   day of week     0-6

   A field may be an asterisk `*`, which always stands for `first-last`.

   Ranges of numbers are allowed. Ranges are two numbers separated
   with a hyphen. The specified range is inclusive. For example, 8-11
   for an `hours` entry specifies execution at hours 8, 9, 10 and 11.

   Lists are allowed. A list is a set of numbers (or ranges) separated
   by commas. Examples: `1,2,5,9`.

   Step values can be used in conjunction with ranges. Following a
   range with `/<number>` specifies skips of the number's value
   through the range. For example, `0-23/2` can be used in the hours
   field to specify command execution every other hour (the
   alternative in the V7 standard is
   `0,2,4,6,8,10,12,14,16,18,20,22`).

   Note: Steps are only accepted for minutes and hours.
*)

type field =
  | All
  | List of int list
  | Range of {first : int; last : int; step : int option}
  | Value of int

let whitespace =
  let open Angstrom in
  let is_whitespace c = Char.equal ' ' c in
  take_while1 is_whitespace

let digits =
  let open Angstrom in
  let is_digit = function '0' .. '9' -> true | _ -> false in
  int_of_string <$> take_while1 is_digit

let value =
  let open Angstrom in
  lift (fun n -> Value n) digits

let all =
  let open Angstrom in
  lift (fun _ -> All) (char '*')

let list =
  let open Angstrom in
  lift (fun values -> List values) (sep_by1 (char ',') digits)

let range =
  let open Angstrom in
  lift3
    (fun first last step -> Range {first; last; step})
    digits
    (char '-' *> digits)
    (option None (Option.some <$> char '/' *> digits))

let field = Angstrom.choice [range; list; all; value]

type time = {
  minute : field;
  hour : field;
  day : field;
  month : field;
  day_of_week : field;
}

let parser =
  let open Angstrom in
  (fun minute hour day month day_of_week ->
    {minute; hour; day; month; day_of_week})
  <$> field <* whitespace <*> field <* whitespace <*> field <* whitespace
  <*> field <* whitespace <*> field

let parse s = Angstrom.parse_string parser ~consume:All s

type task = {
  name : string;
  time : time;
  randomized_delay : int;
  action : unit -> unit Lwt.t;
}

type t = {
  tasks : task list;
  shutdown : unit Lwt.t;
  trigger_shutdown : unit Lwt.u;
}

let validate_time s =
  let in_range ~kind ~v (min, max) =
    let check v = v >= min && v <= max in
    match v with
    | All -> true
    | Value v -> check v
    | Range {first; last; step} -> (
        let b = first <= last && check first && check last in
        match (step, kind) with
        | None, _ | Some _, (`hour | `minute) -> b
        | Some _, (`day | `month | `day_of_week) -> false)
    | List vs -> List.for_all (fun v -> v >= min && v <= max) vs
  in
  in_range ~kind:`minute ~v:s.minute (0, 59)
  && in_range ~kind:`hour ~v:s.hour (0, 23)
  && in_range ~kind:`day ~v:s.day (1, 31)
  && in_range ~kind:`month ~v:s.month (1, 12)
  && in_range ~kind:`day_of_week ~v:s.day_of_week (0, 6)

let time_of_string s =
  match parse s with
  | Ok v -> v
  | Error _ -> failwith (Format.asprintf "Invalid cron string format: %s" s)

let pp_time ppf spec =
  let pp ppf = function
    | All -> Format.fprintf ppf "*"
    | Value v -> Format.pp_print_int ppf v
    | Range {first; last; step} ->
        Format.fprintf
          ppf
          "%d-%d%s"
          first
          last
          (Option.fold ~none:"" ~some:(Format.sprintf "/%d") step)
    | List vs ->
        Format.pp_print_list
          ~pp_sep:(fun ppf () -> Format.fprintf ppf ",")
          Format.pp_print_int
          ppf
          vs
  in
  Format.fprintf
    ppf
    "%a %a %a %a %a"
    pp
    spec.minute
    pp
    spec.hour
    pp
    spec.day
    pp
    spec.month
    pp
    spec.day_of_week

let time_to_string tm = Format.asprintf "%a" pp_time tm

let task ~name ~tm ~action ?(randomized_delay = 0) () =
  let time = time_of_string tm in
  let is_valid = validate_time time in
  if not is_valid then
    failwith
      (Format.asprintf
         "chronos: invalid time specification (%s)"
         (time_to_string time)) ;
  if not (randomized_delay >= 0) then
    failwith
      (Format.asprintf
         "chronos: randomized delay (%d) must be greater than zero"
         randomized_delay) ;
  {name; time; action; randomized_delay}

let init ~tasks =
  let shutdown, trigger_shutdown = Lwt.task () in
  {tasks; shutdown; trigger_shutdown}

(* Check if a task should run at the given time.

   The `(succ tm.Unix.tm_mon)` is needed because OCaml's Unix module
   and the cron standard use different numbering conventions for
   months:
   - Unix.tm_mon ranges from 0 to 11 (January = 0, December = 11)
   - Cron standard uses 1 to 12 (January = 1, December = 12) *)

let should_run task tm =
  let matches t v =
    match v with
    | Value v -> Int.equal v t
    | All -> true
    | Range {first; last; step = None} -> first <= t && t <= last
    | Range {first; last; step = Some step} ->
        first <= t && t <= last && first mod step = t mod step
    | List vs -> List.mem t vs
  in
  matches tm.Unix.tm_min task.time.minute
  && matches tm.Unix.tm_hour task.time.hour
  && matches tm.Unix.tm_mday task.time.day
  && matches (succ tm.Unix.tm_mon) task.time.month
  && matches tm.Unix.tm_wday task.time.day_of_week

let run ~now_tm t =
  Lwt_list.iter_p
    (fun task ->
      if should_run task now_tm then (
        let* () =
          let delay = Random.int (succ task.randomized_delay) in
          if delay > 0 then (
            Log.info
              "chronos.%s: wait for a delay of %d seconds before starting"
              task.name
              delay ;
            Lwt_unix.sleep (float delay))
          else unit
        in
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
