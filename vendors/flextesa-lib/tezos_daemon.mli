type baker_args = {key : string; lb_vote : string option}

type args = private
  | Baker : baker_args -> args
  | Endorser : string -> args
  | Accuser : args

type t = private
  { node: Tezos_node.t
  ; client: Tezos_client.t
  ; exec: Tezos_executable.t
  ; args: args
  ; name_tag: string option }

val of_node :
     ?name_tag:string
  -> Tezos_node.t
  -> args
  -> exec:Tezos_executable.t
  -> client:Tezos_client.t
  -> t

val baker_of_node :
     ?name_tag:string
  -> Tezos_node.t
  -> key:string
  -> lb_vote:string option
  -> exec:Tezos_executable.t
  -> client:Tezos_client.t
  -> t

val endorser_of_node :
     ?name_tag:string
  -> Tezos_node.t
  -> key:string
  -> exec:Tezos_executable.t
  -> client:Tezos_client.t
  -> t

val accuser_of_node :
     ?name_tag:string
  -> Tezos_node.t
  -> exec:Tezos_executable.t
  -> client:Tezos_client.t
  -> t

val arg_to_string : args -> string

val to_script :
     < env_config: Environment_configuration.t ; paths: Paths.t ; .. >
  -> t
  -> unit Genspio.Language.t

val process :
     < env_config: Environment_configuration.t ; paths: Paths.t ; .. >
  -> t
  -> Running_processes.Process.t
