open Internal_pervasives

type t = {id: string; port: int; exec: Tezos_executable.t}
type client = t

let no_node_client ~exec = {id= "C-null"; port= 0; exec}

let of_node ~exec n =
  let id = sprintf "C-%s" n.Tezos_node.id in
  let port = n.Tezos_node.rpc_port in
  {id; port; exec}

let base_dir t ~state = Paths.root state // sprintf "Client-base-%s" t.id

open Tezos_executable.Make_cli

let client_command ?(wait = "none") t ~state args =
  Tezos_executable.call t.exec
    ~path:(base_dir t ~state // "exec-client")
    ( ("--wait" :: wait :: optf "port" "%d" t.port)
    @ opt "base-dir" (base_dir ~state t)
    @ args )

let bootstrapped_script t ~state =
  let open Genspio.EDSL in
  let cmd =
    loop_until_true ~attempts:5 ~sleep:1
      ~on_failed_attempt:(fun _ ->
        eprintf (str "Bootstrap attempt failed\\n") [])
      (succeeds (client_command t ~state ["bootstrapped"])) in
  seq
    [ exec ["mkdir"; "-p"; base_dir ~state t]
    ; if_seq cmd ~t:[eprintf (str "Node Bootstrapped\\n") []] ]

let bootstrapped t ~state =
  let genspio = bootstrapped_script t ~state in
  Running_processes.run_genspio state (sprintf "bootstrap-%s" t.id) genspio
  >>= fun _ -> return ()

let import_secret_key_script t ~state name key =
  client_command t ~state ["import"; "secret"; "key"; name; key; "--force"]

let activate_protocol_script t ~state protocol =
  let open Genspio.EDSL in
  check_sequence ~verbosity:(`Announce "activating-protocol")
    [ ( "add-activator-key"
      , import_secret_key_script t ~state
          (Tezos_protocol.dictator_name protocol)
          (Tezos_protocol.dictator_secret_key protocol) )
    ; ( "activate-protocol"
      , ensure "activate-alpha-only-once"
          ~condition:
            (greps_to
               (str protocol.Tezos_protocol.hash)
               (client_command t ~state
                  ["rpc"; "get"; "/chains/main/blocks/head/metadata"]))
          ~how:
            [ ( "activate"
              , client_command t ~state @@ opt "block" "genesis"
                @ [ "activate"; "protocol"; protocol.Tezos_protocol.hash; "with"
                  ; "fitness"
                  ; sprintf "%d" protocol.Tezos_protocol.expected_pow
                  ; "and"; "key"
                  ; Tezos_protocol.dictator_name protocol
                  ; "and"; "parameters"
                  ; Tezos_protocol.protocol_parameters_path ~config:state
                      protocol ] ) ] ) ]

let import_secret_key t ~state name key =
  Running_processes.run_genspio state
    (sprintf "client-%s-import-key-%s-as-%s" t.id name key)
    (import_secret_key_script t ~state name key)
  >>= fun _ -> return ()

let register_as_delegate t ~state keyname =
  Running_processes.run_genspio state
    (sprintf "client-%s-register-as-delegate-for-%s" t.id keyname)
    Genspio.EDSL.(
      if_seq
        ( succeeds
        @@ client_command t ~state
             ["register"; "key"; keyname; "as"; "delegate"] )
        ~t:[say "SUCCESS: Registering %s as delegate" [str keyname]]
        ~e:[say "FAILURE: Registering %s as delegate" [str keyname]])
  >>= fun _ -> return ()

let activate_protocol t ~state protocol =
  Running_processes.run_genspio state
    (sprintf "activate_protocol-%s-%s" t.id protocol.Tezos_protocol.id)
    (activate_protocol_script t ~state protocol)
  >>= fun _ -> return ()

module Command_error = struct
  type t = [`Client_command_error of string * string list option]

  let failf ?args fmt =
    ksprintf (fun s -> fail (`Client_command_error (s, args) : [> t])) fmt

  let pp fmt (`Client_command_error (msg, args) : t) =
    Format.fprintf fmt "Client-command-error:@ %s%s" msg
      (Option.value_map args ~default:"" ~f:(fun l ->
           sprintf " (args: %s)"
             (List.map ~f:(sprintf "%S") l |> String.concat ~sep:", ")))
end

open Command_error
open Console

let client_cmd ?wait state ~client args =
  Running_processes.run_cmdf state "sh -c %s"
    ( client_command ?wait client ~state args
    |> Genspio.Compile.to_one_liner |> Filename.quote )
  >>= fun res ->
  Console.display_errors_of_command state res
  >>= fun success -> return (success, res)

let successful_client_cmd ?wait state ~client args =
  client_cmd state ?wait ~client args
  >>= fun (success, res) ->
  match success with
  | true -> return res
  | false ->
      failf ~args "Client-command failure: %s" (String.concat ~sep:" " args)

let rpc state ~client meth ~path =
  let args =
    match meth with
    | `Get -> ["rpc"; "get"; path]
    | `Post s -> ["rpc"; "post"; path; "with"; s] in
  successful_client_cmd state ~client args
  >>= fun res ->
  let output = String.concat ~sep:"\n" res#out in
  try
    let json = Jqo.of_string output in
    return json
  with e -> (
    try
      Ezjsonm.from_string (sprintf "[ %s ]" output)
      |> function `A [one] -> return one | _ -> raise e
    with e ->
      say state
        EF.(
          list
            [ desc (shout "Output:") (markdown_verbatim output)
            ; desc (shout "Error:")
                (markdown_verbatim (String.concat ~sep:"\n" res#err)) ])
      >>= fun () ->
      failf ~args "RPC failure cannot parse json: %s" Exn.(to_string e) )

let find_applied_in_mempool state ~client ~f =
  successful_client_cmd state ~client
    ["rpc"; "get"; "/chains/main/mempool/pending_operations"]
  >>= fun res ->
  try
    let json = Jqo.of_string (String.concat ~sep:"\n" res#out) in
    let found = Jqo.field ~k:"applied" json |> Jqo.list_find ~f in
    say state
      EF.(
        desc
          (af "piece of mempool found (client %s):" client.id)
          (markdown_verbatim (Ezjsonm.to_string json)))
    >>= fun () -> return (Some found)
  with e ->
    say state
      EF.(desc (shout "not found in mempool") (af "%s" (Exn.to_string e)))
    >>= fun () -> return None

let mempool_has_operation state ~client ~kind =
  find_applied_in_mempool state ~client ~f:(fun o ->
      Jqo.field o ~k:"contents"
      |> Jqo.list_exists ~f:(fun op -> Jqo.field op ~k:"kind" = `String kind))
  >>= fun found_or_not -> return (found_or_not <> None)

let block_has_operation state ~client ~level ~kind =
  successful_client_cmd state ~client
    ["rpc"; "get"; sprintf "/chains/main/blocks/%d/operations" level]
  >>= fun res ->
  try
    let json = Jqo.of_string (String.concat ~sep:"\n" res#out) in
    let found =
      Jqo.list_exists json ~f:(fun olist ->
          Jqo.list_exists olist ~f:(fun o ->
              Jqo.field o ~k:"contents"
              |> Jqo.list_exists ~f:(fun op ->
                     Jqo.field op ~k:"kind" = `String kind))) in
    say state
      EF.(
        desc
          (af "looking for %S in block %d: %sfound" kind level
             (if found then "" else "not "))
          (af "%s" (Ezjsonm.to_string json)))
    >>= fun () -> return found
  with e ->
    say state
      EF.(
        desc
          (ksprintf shout "Operation %S not found in block" kind)
          (af "%s" (Exn.to_string e)))
    >>= fun () -> return false

let get_block_header state ~client block =
  let path =
    sprintf "/chains/main/blocks/%s/header"
      (match block with `Head -> "head" | `Level i -> Int.to_string i) in
  rpc state ~client `Get ~path

let list_known_addresses state ~client =
  successful_client_cmd state ~client ["list"; "known"; "addresses"]
  >>= fun res ->
  let re =
    Re.(
      compile
        (seq
           [ group (rep1 (alt [alnum; char '_']))
           ; str ": "
           ; group (rep1 alnum)
           ; alt [space; eol; eos] ])) in
  return
    (List.filter_map res#out
       ~f:
         Re.(
           fun line ->
             match exec_opt re line with
             | None -> None
             | Some matches -> Some (Group.get matches 1, Group.get matches 2)))

module Ledger = struct
  type hwm = {main: int; test: int; chain: Tezos_crypto.Chain_id.t option}

  let set_hwm state ~client ~uri ~level =
    successful_client_cmd state ~client
      [ "set"; "ledger"; "high"; "watermark"; "for"; uri; "to"
      ; string_of_int level ]
    >>= fun _ -> return ()

  let get_hwm state ~client ~uri =
    successful_client_cmd state ~client
      [ "get"; "ledger"; "high"; "watermark"; "for"; uri
      ; "--no-legacy-instructions" ]
    (* TODO: Use --for-script when available *)
    >>= fun res ->
    (* e.g. The high water mark values for married-bison-ill-burmese/P-256 are
            0 for the main-chain (NetXH12Aer3be93) and
            0 for the test-chain. *)
    let re =
      Re.(
        let num = rep1 digit in
        compile
          (seq
             [ group num
             ; str " for the main-chain ("
             ; group (rep1 alnum)
             ; str ") and "; group num; str " for the test-chain." ])) in
    let matches = Re.exec re (String.concat ~sep:" " res#out) in
    try
      return
        { main= int_of_string (Re.Group.get matches 1)
        ; chain=
            (let v = Re.Group.get matches 2 in
             if v = "'Unspecified'" then None
             else Some (Tezos_crypto.Chain_id.of_b58check_exn v))
        ; test= int_of_string (Re.Group.get matches 3) }
    with e ->
      failf
        "Couldn't understand result of 'get high watermark for %S': error %S: \
         from %S"
        uri (Exn.to_string e)
        (String.concat ~sep:"\n" res#out)

  let show_ledger state ~client ~uri =
    successful_client_cmd state ~client ["show"; "ledger"; uri]
    (* TODO: Use --for-script when available *)
    >>= fun res ->
    list_known_addresses state ~client
    >>= fun known_addresses ->
    let pk = Re.(rep1 alnum) in
    let addr_re = Re.(compile (seq [str "* Public Key Hash: "; group pk])) in
    let pubkey_re = Re.(compile (seq [str "* Public Key: "; group pk])) in
    let out = String.concat ~sep:" " res#out in
    try
      let pubkey = Re.(Group.get (exec pubkey_re out) 1) in
      let pubkey_hash = Re.(Group.get (exec addr_re out) 1) in
      let name =
        match
          List.find known_addresses ~f:(fun (_, pkh) -> pkh = pubkey_hash)
        with
        | None -> ""
        | Some (alias, _) -> alias in
      return
        (Tezos_protocol.Account.key_pair name ~pubkey ~pubkey_hash
           ~private_key:uri)
    with e ->
      failf "Couldn't understand result of 'show ledger %S': error %S: from %S"
        uri (Exn.to_string e)
        (String.concat ~sep:"\n" res#out)

  let deauthorize_baking state ~client ~uri =
    successful_client_cmd state ~client
      ["deauthorize"; "ledger"; "baking"; "for"; uri]
    >>= fun _ -> return ()

  let get_authorized_key state ~client ~uri =
    successful_client_cmd state ~client
      ["get"; "ledger"; "authorized"; "path"; "for"; uri]
    >>= fun res ->
    let re_uri =
      Re.(compile (seq [str "Authorized baking URI: "; group (rep1 any); eol]))
    in
    let re_none = Re.(compile (str "No baking key authorized")) in
    let out = String.concat ~sep:" " res#out in
    return
      Re.(
        match exec_opt re_none out with
        | Some _ -> None
        | None -> Some (Group.get (exec re_uri out) 1))
end

module Keyed = struct
  type t = {client: client; key_name: string; secret_key: string}

  let make client ~key_name ~secret_key = {client; key_name; secret_key}

  let initialize state {client; key_name; secret_key} =
    successful_client_cmd state ~client
      ["import"; "secret"; "key"; key_name; secret_key; "--force"]

  let bake ?chain state baker msg =
    let chain_arg =
      Option.value_map chain ~default:[] ~f:(fun c -> ["--chain"; c]) in
    successful_client_cmd state ~client:baker.client
      ( chain_arg
      @ ["bake"; "for"; baker.key_name; "--force"; "--minimal-timestamp"] )
    >>= fun res ->
    Log_recorder.Operations.bake state ~client:baker.client.id ~output:res#out
      msg ;
    say state
      EF.(
        desc
          (af "Successful bake (%s: %s):" baker.client.id msg)
          (ocaml_string_list res#out))

  let endorse state baker msg =
    successful_client_cmd state ~client:baker.client
      ["endorse"; "for"; baker.key_name]
    >>= fun res ->
    Log_recorder.Operations.endorse state ~client:baker.client.id
      ~output:res#out msg ;
    say state
      EF.(
        desc
          (af "Successful bake (%s: %s):" baker.client.id msg)
          (ocaml_string_list res#out))

  let generate_nonce state {client; key_name; _} data =
    successful_client_cmd state ~client
      ["generate"; "nonce"; "hash"; "for"; key_name; "from"; data]
    >>= fun res -> return (List.hd_exn res#out)

  let forge_and_inject state {client; key_name; _} ~json =
    rpc state ~client ~path:"/chains/main/blocks/head/helpers/forge/operations"
      (`Post (Ezjsonm.to_string json))
    >>= fun res ->
    let operation_bytes = match res with `String s -> s | _ -> assert false in
    let bytes_to_sign = "0x03" ^ operation_bytes in
    successful_client_cmd state ~client
      ["sign"; "bytes"; bytes_to_sign; "for"; key_name]
    >>= fun sign_res ->
    let to_decode =
      List.hd_exn sign_res#out
      |> String.chop_prefix_exn ~prefix:"Signature:"
      |> String.strip in
    say state EF.(desc (shout "TO DECODE:") (af "%S" to_decode))
    >>= fun () ->
    let decoded =
      Option.value_exn ~message:"base58 dec"
        (Tezos_crypto.Base58.safe_decode to_decode)
      |> Hex.of_string ?ignore:None |> Hex.show in
    say state EF.(desc (shout "DECODED:") (af "%S" decoded))
    >>= fun () ->
    let actual_signature =
      String.chop_prefix_exn ~prefix:"09f5cd8612" decoded in
    say state
      EF.(
        desc_list (af "Injecting Operation")
          [ ef_json "Injecting" (json :> Ezjsonm.value)
          ; desc (haf "op:")
              (af "%d: %S" (String.length operation_bytes) operation_bytes)
          ; desc (haf "sign:")
              (af "%d: %S" (String.length actual_signature) actual_signature)
          ])
    >>= fun () ->
    rpc state ~client ~path:"/injection/operation?chain=main"
      (`Post (sprintf "\"%s%s\"" operation_bytes actual_signature))
end
