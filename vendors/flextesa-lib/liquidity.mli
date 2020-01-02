open Internal_pervasives

module Data : sig
  type t

  val to_string : t -> string
  val rawf : ('a, Caml.Format.formatter, unit, t) format4 -> 'a
  val empty_set : t
  val address : string -> t
  val tuple : t list -> t
  val int : int -> t
  val string : string -> t
  val nat : int -> t
  val tez : [`Mutez of int] -> t
  val list : t list -> t
  val set : t list -> t
  val key_hash : string -> t
  val key : string -> t
  val account_key : Tezos_protocol.Account.t -> t
  val account_key_hash : Tezos_protocol.Account.t -> t
  val signature : string -> t
  val bytes : string -> t
  val some : t -> t
  val none : t
  val typed_none : string -> t
  val record : (string * t) list -> t
end

module Contract : sig
  type t = private {name: string; paths: string list; main_name: string option}

  val make :
    ?library:string list -> ?main_name:string -> string -> path:string -> t

  val build_dir : < paths: Paths.t ; .. > -> t -> string

  val ensure_build_dir :
       < paths: Paths.t ; runner: Running_processes.State.t ; .. > Base_state.t
    -> t
    -> ( string
       , [> System_error.t | Process_result.Error.t] )
       Asynchronous_result.t

  val base_liquidity_command : 'a -> t -> string

  val michelson :
       < paths: Paths.t ; runner: Running_processes.State.t ; .. > Base_state.t
    -> t
    -> ( string
       , [> System_error.t | Process_result.Error.t] )
       Asynchronous_result.t

  val storage_initialization :
       < application_name: string
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> t
    -> tezos_node:string
    -> storage:Data.t list
    -> ( string
       , [> System_error.t | Process_result.Error.t] )
       Asynchronous_result.t

  val arguments :
       < paths: Paths.t ; runner: Running_processes.State.t ; .. > Base_state.t
    -> t
    -> entry_point:string
    -> data:Data.t
    -> ( string
       , [> System_error.t | Process_result.Error.t] )
       Asynchronous_result.t

  val cmdliner_term : prefix:string -> name:string -> unit -> t Cmdliner.Term.t
end

module On_chain : sig
  val tezos_client_keyed_originate_contract :
       ?force:bool
    -> ?transferring:int
    -> ?burn_cap:float
    -> < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; env_config: Environment_configuration.t
       ; runner: Running_processes.State.t
       ; .. >
    -> Tezos_client.Keyed.t
    -> name:string
    -> source:string
    -> storage:string
    -> ( Process_result.t
       , [> Process_result.Error.t | System_error.t] )
       Asynchronous_result.t

  val build_and_deploy :
       ?burn_cap:float
    -> < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; operations_log: Log_recorder.Operations.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> Contract.t
    -> keyed_client:Tezos_client.Keyed.t
    -> storage:(string * Data.t) list
    -> balance:int
    -> ( string
       , [> Process_result.Error.t | System_error.t | Process_result.Error.t]
       )
       Asynchronous_result.t

  val silent_client_cmd :
       < paths: Paths.t
       ; env_config: Environment_configuration.t
       ; runner: Running_processes.State.t
       ; .. >
       Base_state.t
    -> client:Tezos_client.t
    -> string list
    -> (bool * Process_result.t, [> System_error.t]) Asynchronous_result.t

  val call :
       ?msg:string
    -> ?should:[ `Be_ok
               | `Fail
               | `Script_failwith_re of Re.re
               | `Command_stderr_re of Re.re ]
    -> ?transferring:int
    -> ?burn_cap:float
    -> < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> Contract.t
    -> keyed_client:Tezos_client.Keyed.t
    -> entry_point:string
    -> data:Data.t
    -> ( Process_result.t
       , [> System_error.t | `Scenario_error of string | Process_result.Error.t]
       )
       Asynchronous_result.t

  val key_with_type_json : [< `Nat of int] -> [> Ezjsonm.t] * [> Ezjsonm.t]

  val big_map_get :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; env_config: Environment_configuration.t
       ; runner: Running_processes.State.t
       ; .. >
    -> client:Tezos_client.t
    -> address:string
    -> key:[< `Nat of int]
    -> ( < post: string ; result: Ezjsonm.value >
       , [> Process_result.Error.t | System_error.t] )
       Asynchronous_result.t

  val show_contract_command :
       < application_name: string
       ; console: Console.t
       ; env_config: Environment_configuration.t
       ; paths: Paths.t
       ; runner: Running_processes.State.t
       ; .. >
    -> client:Tezos_client.t
    -> name:string
    -> address:string
    -> pp_error:(   Caml.Format.formatter
                 -> [> Process_result.Error.t | System_error.t]
                 -> unit)
    -> Console.Prompt.item

  val big_map_get_command :
       < application_name: string
       ; console: Console.t
       ; paths: Paths.t
       ; env_config: Environment_configuration.t
       ; runner: Running_processes.State.t
       ; .. >
    -> names:string list
    -> thing:string
    -> client:Tezos_client.t
    -> name:string
    -> address:string
    -> key_of_string:(   string
                      -> ( [< `Nat of int]
                         , ([> Process_result.Error.t
                            | System_error.t
                            | `Scenario_error of string ]
                            as
                            'errors) )
                         Asynchronous_result.t)
    -> pp_error:(Caml.Format.formatter -> 'errors -> unit)
    -> Console.Prompt.item
end
