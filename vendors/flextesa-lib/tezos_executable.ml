open Internal_pervasives

module Make_cli = struct
  let flag name = [sprintf "--%s" name]
  let opt name s = [sprintf "--%s" name; s]
  let optf name fmt = ksprintf (opt name) fmt
end

module Unix_files_sink = struct
  type t = {matches: string list option; level_at_least: string}

  let all_at_least level_at_least = {matches= None; level_at_least}
  let all_notices = all_at_least "notice"
  let all_info = all_at_least "info"
end

type kind = [`Node | `Baker | `Endorser | `Accuser | `Client | `Admin]

type t =
  { kind: kind
  ; binary: string option
  ; unix_files_sink: Unix_files_sink.t option
  ; environment: (string * string) list }

let make ?binary ?unix_files_sink ?(environment = []) (kind : [< kind]) =
  {kind; binary; unix_files_sink; environment}

let kind_string (kind : [< kind]) =
  match kind with
  | `Accuser -> "accuser-alpha"
  | `Baker -> "baker-alpha"
  | `Endorser -> "endorser-alpha"
  | `Node -> "node"
  | `Client -> "client"
  | `Admin -> "admin-client"

let default_binary t = sprintf "tezos-%s" (kind_string t.kind)
let get t = Option.value t.binary ~default:(default_binary t)

let call state t ~path args =
  let open Genspio.EDSL in
  let unix_files_sink =
    match t.unix_files_sink with
    | Some s -> Some s
    | None ->
        Environment_configuration.default_events_level state
        |> Option.map ~f:Unix_files_sink.all_at_least in
  seq
    ( Option.value_map unix_files_sink ~default:[] ~f:(function
        | {matches= None; level_at_least} ->
            [ setenv
                ~var:(str "TEZOS_EVENTS_CONFIG")
                (ksprintf str "unix-files://%s?level-at-least=%s"
                   (path // "events") level_at_least) ]
        | _other -> assert false)
    @ [ exec ["mkdir"; "-p"; path]
      ; write_stdout
          ~path:(path // "last-cmd" |> str)
          (printf (str "ARGS: %s\\n") [str (String.concat ~sep:" " args)])
      ; exec (get t :: args) ] )

let cli_term ?(extra_doc = "") state kind prefix =
  let open Cmdliner in
  let open Term in
  let docs = Manpage_builder.section state ~rank:2 ~name:"EXECUTABLE PATHS" in
  pure (fun binary -> {kind; binary; unix_files_sink= None; environment= []})
  $ Arg.(
      value
      & opt (some string) None
      & info ~docs
          [sprintf "%s-%s-binary" prefix (kind_string kind)]
          ~doc:
            (sprintf "Binary for the `tezos-%s` to use%s." (kind_string kind)
               extra_doc))
