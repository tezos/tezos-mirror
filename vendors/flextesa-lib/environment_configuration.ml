open! Internal_pervasives

(** Global configuration from environment variables and such. *)

type 'a var = {name: string; doc: string list; transform: string option -> 'a}

let var name ~doc ~transform = {name; doc; transform}

let var_default name ~doc ~default =
  var name
    ~doc:(doc @ [Fmt.str "The default is `%s`." default])
    ~transform:(Option.value ~default)

type t =
  { prefix: string
  ; disabled: bool
  ; default_cors_origin: string option var
  ; better_call_dev_base_url: string var
  ; default_events_level: string option var }

let default () =
  { prefix= "flextesa_"
  ; disabled= false
  ; default_cors_origin=
      var "node_cors_origin"
        ~doc:
          [ "Add CORS headers to all nodes in the sandbox."
          ; "An empty value means “none.”" ] ~transform:(function
        | Some "" | None -> None
        | Some other -> Some other)
  ; better_call_dev_base_url=
      var_default "better_call_dev_base_url"
        ~doc:["Set the base URL for the better-call.dev explorer."]
        ~default:"https://better-call.dev/sandbox"
  ; default_events_level=
      var "default_events_level"
        ~doc:
          [ "Set the minimum evenet-level for the event-logging of all \
             tezos-executables."; "The default is `warning`."
          ; "The value `none` means: completely disable the event-logging." ]
        ~transform:(function
        | None -> Some "warning"
        | Some "none" -> None
        | Some s -> Some s) }

type 'a state = < env_config: t ; .. > as 'a

let prefix state = state#env_config.prefix
let varname pref var = Fmt.str "%s%s" pref var.name

let init state env_config =
  let reg_var vf =
    let v = vf env_config in
    Manpage_builder.State.register_env_variable state
      (varname env_config.prefix v)
      v.doc in
  reg_var (fun e -> e.default_cors_origin) ;
  reg_var (fun e -> e.better_call_dev_base_url) ;
  reg_var (fun e -> e.default_events_level)

let get_var_from_environment state varf =
  let var = varf state#env_config in
  Caml.Sys.getenv_opt (varname state#env_config.prefix var) |> var.transform

let default_cors_origin state =
  get_var_from_environment state (fun e -> e.default_cors_origin)

let better_call_dev_base_url state =
  get_var_from_environment state (fun e -> e.better_call_dev_base_url)

let default_events_level state =
  get_var_from_environment state (fun e -> e.default_events_level)
