(** Functions for building test-scenarios commands. *)

open Internal_pervasives

module Common_errors : sig
  type t =
    [ `Die of int
    | `Empty_protocol_list
    | `Precheck_failure of string
    | Process_result.Error.t
    | `Scenario_error of string
    | System_error.t
    | Test_scenario.Inconsistency_error.t
    | `Waiting_for of string * [`Time_out] ]

  val pp : t Fmt.t
end

module Command_making_state : sig
  type specific =
    < command_name: string
    ; env_config: Environment_configuration.t
    ; manpager: Manpage_builder.State.t >

  type 'a t = 'a constraint 'a = < application_name: string ; specific ; .. >

  val make :
       application_name:string
    -> command_name:string
    -> unit
    -> < application_name: string
       ; command_name: string
       ; env_config: Environment_configuration.t
       ; manpager: Manpage_builder.State.t >
end

(** Make {!Cmdliner} commands from {!Asynchronous_result} functions. *)
module Run_command : sig
  val or_hard_fail :
       < application_name: string ; console: Console.t ; .. >
    -> (unit -> (unit, ([> ] as 'a)) Asynchronous_result.t)
    -> pp_error:(Caml.Format.formatter -> 'a -> unit)
    -> unit

  val make :
       pp_error:(Caml.Format.formatter -> ([> ] as 'errors) -> unit)
    -> ( < application_name: string ; console: Console.t ; .. >
       * (unit -> (unit, 'errors) Asynchronous_result.t) )
       Cmdliner.Term.t
    -> Cmdliner.Cmd.info
    -> unit Cmdliner.Term.t * Cmdliner.Cmd.info
end

module Full_default_state : sig
  val cmdliner_term :
       _ Command_making_state.t
    -> ?default_interactivity:Interactive_test.Interactivity.t
    -> ?disable_interactivity:bool
    -> unit
    -> < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; operations_log: Log_recorder.Operations.t
       ; paths: Paths.t
       ; pauser: Interactive_test.Pauser.t
       ; runner: Running_processes.State.t
       ; test_interactivity: Interactive_test.Interactivity.t >
       Cmdliner.Term.t
end

(** Legacy API for {!Full_default_state.cmdliner_term}. *)
val cli_state :
     ?default_interactivity:Interactive_test.Interactivity.t
  -> ?disable_interactivity:bool
  -> name:string
  -> unit
  -> < application_name: string
     ; env_config: Environment_configuration.t
     ; console: Console.t
     ; operations_log: Log_recorder.Operations.t
     ; paths: Paths.t
     ; pauser: Interactive_test.Pauser.t
     ; runner: Running_processes.State.t
     ; test_interactivity: Interactive_test.Interactivity.t >
     Cmdliner.Term.t
(** Create a full [state] value for test-scenarios. *)
