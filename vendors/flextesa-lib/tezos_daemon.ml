open Internal_pervasives

type baker_args = {key : string; lb_vote : string option}

type args =
  | Baker : baker_args -> args
  | Endorser : string -> args
  | Accuser : args

type t =
  { node: Tezos_node.t
  ; client: Tezos_client.t
  ; exec: Tezos_executable.t
  ; args: args
  ; name_tag: string option }

let of_node ?name_tag node args ~exec ~client =
  {node; exec; client; args; name_tag}

let baker_of_node ?name_tag nod ~key ~lb_vote =
  of_node nod ?name_tag (Baker {key; lb_vote})
let endorser_of_node ?name_tag nod ~key = of_node nod ?name_tag (Endorser key)
let accuser_of_node ?name_tag nod = of_node ?name_tag nod Accuser

let arg_to_string = function
  | Baker {key; lb_vote = _} -> sprintf "baker-%s" key
  | Endorser k -> sprintf "endorser-%s" k
  | Accuser -> "accuser"

let to_script state (t : t) =
  let base_dir = Tezos_client.base_dir ~state t.client in
  let call t args =
    Tezos_executable.call state t.exec
      ~path:
        ( base_dir
        // sprintf "exec-%s-%d%s" (arg_to_string t.args)
             t.node.Tezos_node.rpc_port
             (Option.value_map t.name_tag ~default:"" ~f:(sprintf "-%s")) )
      args in
  match t.args with
  | Baker {key; lb_vote} ->
      let node_path = Tezos_node.data_dir state t.node in
      let lb_vote =
        match lb_vote with
        | None -> []
        | Some vote -> ["--liquidity-baking-toggle-vote"; vote]
      in
      call t @@
        [ "--endpoint"
        ; sprintf "http://localhost:%d" t.node.Tezos_node.rpc_port
        ; "--base-dir"; base_dir; "run"; "with"; "local"; "node"; node_path ]
        @ lb_vote
        @ [ key ]
  | Endorser key ->
      call t
        [ "--endpoint"
        ; sprintf "http://localhost:%d" t.node.Tezos_node.rpc_port
        ; "--base-dir"; base_dir; "run"; key ]
  | Accuser ->
      call t
        [ "--endpoint"
        ; sprintf "http://localhost:%d" t.node.Tezos_node.rpc_port
        ; "--base-dir"; base_dir; "run"; "--preserved-levels"; "10" ]

let process state (t : t) =
  Running_processes.Process.genspio
    (sprintf "%s-for-%s%s" (arg_to_string t.args) t.node.Tezos_node.id
       (Option.value_map t.name_tag ~default:"" ~f:(sprintf "-%s")))
    (to_script state t)
