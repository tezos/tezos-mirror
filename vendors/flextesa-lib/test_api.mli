(** Test APIs by hitting endpoints. *)

open Internal_pervasives

type display_policy = [`All | `Lines of int | `No | `On_error of display_policy]

val call :
     ?comment:(Caml.Format.formatter -> unit)
  -> ?expect:[< `Anything | `Status of Cohttp.Code.status_code > `Status]
  -> ?show_body:display_policy
  -> ?show_response:display_policy
  -> ?how:[`Get | `Post_json of Ezjsonm.t | `Delete_json of Ezjsonm.t]
  -> < console: Console.t ; .. > Base_state.t
  -> api_prefix:string
  -> path:string
  -> ( < body_json: (Ezjsonm.value, string) Result.t ; body_lines: string list >
     , [> System_error.t | `Scenario_error of string] )
     Asynchronous_result.t
