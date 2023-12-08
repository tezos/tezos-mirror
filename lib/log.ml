type level = FATAL | ERROR | WARNING | INFO | DEBUG

let level_encoding : level TzPervasives.Data_encoding.t =
  Data_encoding.string_enum
    [
      ("FATAL", FATAL);
      ("ERROR", ERROR);
      ("WARNING", WARNING);
      ("INFO", INFO);
      ("DEBUG", DEBUG);
    ]

type t = string

let verbosity : level ref = ref FATAL

let level_of_string = function
  | "FATAL" -> Some DEBUG
  | "ERROR" -> Some ERROR
  | "WARNING" -> Some WARNING
  | "INFO" -> Some INFO
  | "DEBUG" -> Some DEBUG
  | _ -> None

let string_of_level = function
  | FATAL -> "FATAL"
  | ERROR -> "ERROR"
  | WARNING -> "WARNING"
  | INFO -> "INFO"
  | DEBUG -> "DEBUG"

let int_of_level = function
  | FATAL -> 0
  | ERROR -> 3
  | WARNING -> 4
  | INFO -> 6
  | DEBUG -> 7

let log0 level id =
  let v = int_of_level level in
  fun str ->
    if v <= int_of_level !verbosity then
      let s = Unix.gettimeofday () in
      let tm = Unix.gmtime s in
      let level = string_of_level level in
      let str =
        match String.split_on_char '\n' @@ str () with
        | [] -> ""
        | [s] -> s
        | list -> String.concat "\n\t" list
      in
      ignore
      @@ Lwt_io.fprintf
           Lwt_io.stdout
           "%04d-%02d-%02d %02d:%02d:%02d.%03d %s %s: %s\n%!"
           (1900 + tm.Unix.tm_year)
           (succ tm.Unix.tm_mon)
           tm.Unix.tm_mday
           tm.Unix.tm_hour
           tm.Unix.tm_min
           tm.Unix.tm_sec
           ((Float.modf s |> fst) *. 100. |> truncate)
           level
           id
           str

let logger =
  let id_ref = ref 0 in
  let name = Filename.basename Sys.executable_name in
  fun () ->
    let id = !id_ref in
    (* Keep the log easier to read. Increase the 999 limit if you need more concurrent loggers. *)
    if id < 999 then incr id_ref else id_ref := 0 ;
    Printf.sprintf "%s[%03d]" name id

let fatal = log0 FATAL

let error = log0 ERROR

let warning = log0 WARNING

let info = log0 INFO

let debug = log0 DEBUG
