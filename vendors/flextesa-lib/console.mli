(** Print messages, and prompt user input from the terminal. *)

open Internal_pervasives

type t =
  { color: bool
  ; buffer: Buffer.t
  ; channel: Lwt_io.output_channel
  ; with_timestamp: bool
  ; formatter: Caml.Format.formatter }

val make : bool -> bool -> t
val pp : Caml.Format.formatter -> t -> unit

val cli_term : unit -> t Cmdliner.Term.t
(** {!Cmdliner.Term.t} which configures the console interaction
    (e.g. the ["--color"] option). *)

val say :
     < application_name: string ; console: t ; .. >
  -> Easy_format.t
  -> (unit, [> System_error.t]) Asynchronous_result.t

val sayf :
     < application_name: string ; console: t ; .. >
  -> (Caml.Format.formatter -> unit -> unit)
  -> (unit, [> System_error.t]) Asynchronous_result.t

(** Create interactive prompts. *)
module Prompt : sig
  type item = private
    { commands: string list
    ; description: string
    ; details: unit Fmt.t option
    ; action:
           Sexplib0.Sexp.t list
        -> ( [`Help | `Loop | `Quit]
           , [`Command_line of string | System_error.t] )
           Asynchronous_result.t }

  val item :
       ?details:unit Fmt.t
    -> description:string
    -> string list
    -> (   Sexplib0.Sexp.t list
        -> ( [`Help | `Loop | `Quit]
           , [`Command_line of string | System_error.t] )
           Asynchronous_result.t)
    -> item
  (** [item description command_aliases action] creates a command
      which performs [action]; action gets all the arguments parsed as
      S-Expressions. *)

  val quit : ?description:string -> string list -> item
  val help : ?description:string -> string list -> item

  val unit_and_loop :
       ?details:unit Fmt.t
    -> description:string
    -> string list
    -> (   Sexplib0.Sexp.t list
        -> ( unit
           , [`Command_line of string | System_error.t] )
           Asynchronous_result.t)
    -> item

  val default_commands : unit -> item list
  (** The default commands are {!help} and {!quit}. *)

  val command :
       ?with_defaults:bool
    -> < application_name: string ; console: t ; .. >
    -> commands:item list
    -> (unit, [> System_error.t]) Asynchronous_result.t
  (** Prompt for a command among [~commnands]. *)
end

val display_errors_of_command :
     < application_name: string ; console: t ; .. >
  -> ?should_output:bool
  -> < err: string list ; out: string list ; status: Unix.process_status ; .. >
  -> (bool, [> System_error.t]) Asynchronous_result.t
(** Display the results of a command if it fails (see {!Process_result.t}). *)
