(** Tools to manage interactivity in test scenarios. *)

open Internal_pervasives

(** Implementations of common {!Console.Prompt.item}. *)
module Commands : sig
  val cmdline_fail :
       ( 'a
       , Caml.Format.formatter
       , unit
       , ('b, [> `Command_line of string]) Asynchronous_result.t )
       format4
    -> 'a

  val no_args :
    'a list -> (unit, [> `Command_line of string]) Asynchronous_result.t

  val flag : string -> Sexplib0.Sexp.t list -> bool

  val unit_loop_no_args :
       description:string
    -> string list
    -> (   unit
        -> ( unit
           , [`Command_line of string | System_error.t] )
           Asynchronous_result.t)
    -> Console.Prompt.item

  val du_sh_root :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> Console.Prompt.item

  val processes :
       < application_name: string
       ; console: Console.t
       ; runner: Running_processes.State.t
       ; .. >
    -> Console.Prompt.item

  val curl_rpc :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> port:int
    -> path:string
    -> ( Ezjsonm.value option
       , [> `Command_line of string | System_error.t] )
       Asynchronous_result.t

  val do_jq :
       < application_name: string ; console: Console.t ; paths: Paths.t ; .. >
    -> msg:string
    -> f:(Ezjsonm.value -> 'b)
    -> Ezjsonm.value option
    -> ('b, [> `Command_line of string]) Asynchronous_result.t

  val curl_unit_display :
       ?jq:(Ezjsonm.value -> Ezjsonm.value)
    -> ?pp_json:(Formatter.t -> Ezjsonm.value -> unit)
    -> < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> string list
    -> default_port:int
    -> path:string
    -> doc:string
    -> Console.Prompt.item

  val curl_metadata :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> default_port:int
    -> Console.Prompt.item

  val curl_level :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> default_port:int
    -> Console.Prompt.item

  val curl_baking_rights :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> default_port:int
    -> Console.Prompt.item

  val all_levels :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> nodes:Tezos_node.t list
    -> Console.Prompt.item

  val show_process :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> Console.Prompt.item

  val kill_all :
    < runner: Running_processes.State.t ; .. > -> Console.Prompt.item

  val secret_keys :
       < application_name: string ; console: Console.t ; .. >
    -> protocol:Tezos_protocol.t
    -> Console.Prompt.item

  val better_call_dev :
       < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> default_port:int
    -> Console.Prompt.item

  val arbitrary_command_on_all_clients :
       ?make_admin:(Tezos_client.t -> Tezos_admin_client.t)
    -> ?command_names:string list
    -> < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> clients:Tezos_client.t list
    -> Console.Prompt.item

  val arbitrary_commands_for_each_client :
       ?make_admin:(Tezos_client.t -> Tezos_admin_client.t)
    -> ?make_command_names:(int -> string list)
    -> < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> clients:Tezos_client.t list
    -> Console.Prompt.item list

  val arbitrary_commands_for_each_and_all_clients :
       ?make_admin:(Tezos_client.t -> Tezos_admin_client.t)
    -> ?make_individual_command_names:(int -> string list)
    -> ?all_clients_command_names:string list
    -> < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; env_config: Environment_configuration.t
       ; runner: Running_processes.State.t
       ; .. >
    -> clients:Tezos_client.t list
    -> Console.Prompt.item list

  val bake_command :
       < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> clients:Tezos_client.Keyed.t list
    -> Console.Prompt.item

  val generate_traffic_command :
       < application_name: string
       ; console: Console.t
       ; operations_log: Log_recorder.Operations.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> clients:Tezos_client.Keyed.t list
    -> Console.Prompt.item

  val all_defaults :
       < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; env_config: Environment_configuration.t
       ; runner: Running_processes.State.t
       ; .. >
    -> nodes:Tezos_node.t list
    -> Console.Prompt.item list
end

(** Configurable (through {!Cmdliner.Term.t}) interactivity of
    test-scenarios. *)
module Interactivity : sig
  type t = [`Full | `None | `On_error | `At_end]

  val pause_on_error : < test_interactivity: t ; .. > -> bool
  val pause_on_success : < test_interactivity: t ; .. > -> bool
  val is_interactive : < test_interactivity: t ; .. > -> bool
  val cli_term : ?default:t -> unit -> t Cmdliner.Term.t
end

(** A {!Pauser.t} is tool to include optional prompting pauses in
    test-scenarios. *)
module Pauser : sig
  type t = private
    { mutable extra_commands: Console.Prompt.item list
    ; default_end: [`Sleep of float] }

  val make : ?default_end:[`Sleep of float] -> Console.Prompt.item list -> t

  val add_commands : < pauser: t ; .. > -> Console.Prompt.item list -> unit
  (** Add commands to the current pauser. *)

  val generic :
       < application_name: string
       ; console: Console.t
       ; pauser: t
       ; test_interactivity: Interactivity.t
       ; .. >
    -> ?force:bool
    -> Easy_format.t list
    -> (unit, [> System_error.t]) Asynchronous_result.t
  (** Pause the test according to [state#interactivity] (overridden
      with [~force:true]), the pause displays the list of
      {!Easy_format.t}s and prompts the user for commands (see
      {!add_commands}). *)

  val run_test :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; pauser: t
       ; runner: Running_processes.State.t
       ; test_interactivity: Interactivity.t
       ; .. >
    -> (unit -> (unit, ([> System_error.t] as 'errors)) Asynchronous_result.t)
    -> pp_error:(Caml.Format.formatter -> 'errors -> unit)
    -> unit
    -> (unit, [> System_error.t | `Die of int]) Asynchronous_result.t
  (** Run a test-scenario and deal with potential errors according
      to [state#test_interactivity]. *)
end
